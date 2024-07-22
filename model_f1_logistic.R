## File: paper_f1_logistic_model.R
## Purpose: Run logistic models for Top N binary outcomes where N is 3 -18

library(Matrix)
library(ppcor)
library(Kendall)
library(purrr)

# Function: make_matrix_rows
# Purpose: Make the sparse matrix for processing
# Parameters: 
#             lineup - list of races with binary flags for driver/car
#             players_in - list of constructors & drivers
#             dnf_partial - Flag that deterines whether dnf credit is assigned 
#                           to either driver/constructor or both 
make_matrix_rows <- function(lineup, players_in, dnf_partial = FALSE) {
  
  
  driver <- lineup[1]
  constructor <- lineup[2]
  
  
  dnf_driver <-  as.logical(lineup[3])
  dnf_constructor <- as.logical(lineup[4])
  
  
  
  zeroRow <- rep(0, length(players_in))
  
  
  if(dnf_partial){
    if (!dnf_driver){
      zeroRow[which(players_in == driver)] <- 1
    }
    else{
      zeroRow[which(players_in == driver)] <- 0
    }
    
    if (!dnf_constructor){
      zeroRow[which(players_in == constructor)] <-  1
    }
    else{
      zeroRow[which(players_in == constructor)] <-  0
    }
  }
  else{
    zeroRow[which(players_in == driver)] <- 1
    zeroRow[which(players_in == constructor)] <-  1
    
  }
  
  
  return(zeroRow)
  
}


# Function: get_rapm_iterative_logistic
# Purpose: Make the sparse matrix for processing
# Parameters: 
#             rapm_model - sparse matrix (X) for model training
#             target - list of rank finishes corresponding to rapm_model rows
#             weights - weights for training
get_rapm_iterative_logistic <- function(rapm_model, results_full, target, weights){
  
  
  driver_list <- unique(results_full$driver_id)
  parent_constructor_list <- unique(results_full$parent_constructor_id)
  constructor_list <- unique(results_full$constructor_id)
  driver_cons_list <- c(driver_list, parent_constructor_list)
  
  
  #Run Initial Model to get Lambda
  cv_model <- glmnet::cv.glmnet(x = rapm_model, ##ncol = 1058
                                y = target,
                                alpha = 0, 
                                weights = weights,
                                standardize = FALSE, 
                                family = "binomial",
                                type.measure = "class") 
  #Best Lambda
  lam <- cv_model$lambda.min 
  
  #Model refit using that lambda
  coef_model <- glmnet::glmnet(x = rapm_model, 
                               y = target,
                               weights = weights,
                               alpha = 0, 
                               standardize = FALSE,
                               lambda = lam,
                               family = "binomial",
                               standard.error = TRUE)
  
  
  
  #Driver Only Model
  driver_only_model <- glmnet::glmnet(x = rapm_model[,1:length(driver_list)], 
                                      y = target,
                                      weights = weights,
                                      alpha = 0, 
                                      standardize = FALSE,
                                      lambda = lam,
                                      family = "binomial",
                                      standard.error = TRUE)
  
  
  #Constructor Only Model
  constructor_only_model <- glmnet::glmnet(
    x = rapm_model[,(length(driver_list) + 1):length(driver_cons_list)], 
    y = target,
    weights = weights,
    alpha = 0, 
    standardize = FALSE,
    lambda = lam,
    family = "binomial",
    standard.error = TRUE)
  
  #Pull Coefficients
  player_coefficients <- coef_model$beta ## length = 1058
  player_coefficients_error <- coef_model ## length = 1058
  
  
  rapm <- player_coefficients[1:length(driver_cons_list)]
  #rapm_error <- player_coefficients_error[1:length(driver_cons_list)]
  
  rapm_frame <- data.frame("entity_id" = driver_cons_list,
                           "rapm" = rapm)
  
  return(list(rapm_frame, rapm, coef_model, driver_only_model, constructor_only_model))
  
  
}


# Function: decay_function
# Purpose: Apply time decay rate for weighting
# Parameters: 
#             time_diff - difference in seasons or rounds from current model round
#             decay_rate - rate of exponetial decay
decay_function <- function(time_diff, decay_rate) {
  time_decay_weight <- exp(decay_rate * time_diff)
  time_decay_weight <- pmax(time_decay_weight, 0)  # Set lower limit of 0
  time_decay_weight <- pmin(time_decay_weight, 1)  # Set upper limit of 1
  return(time_decay_weight)
}


# Function to calculate Kendall's Tau for each race
calc_kendall_tau <- function(data, race_col, y_test_col, y_pred_col) {
  results <- data %>%
    group_by(!!sym(race_col)) %>%
    summarise(
      kendall_tau = Kendall(!!sym(y_test_col), !!sym(y_pred_col))$tau,
      spearman_rho = cor(!!sym(y_test_col), !!sym(y_pred_col), method = "spearman")
    )
  
  
  return(list(results = results, 
              mean_tau = mean(results$kendall_tau), 
              mean_spearman_rho = mean(results$spearman_rho)))
}

# Function: apply_loess_smoothing
# Purpose: Apply the LOESS smoothing to reduce noise
# Parameters: 
#             data - data grouped by a single driver/const to smooth
#             span - span parameter for the LOESS
#             base_min_points - Base minimum points we want for LOESS
#             weight_limit - Max weighting toward LOESS data
#             max_history - max history we want to include in LOESS
apply_loess_smoothing <- function(data, span = 0.3, 
                                  base_min_points = 3, 
                                  weight_limit = 0.9,
                                  max_history = 20) {
  
  min_points <- ceiling(base_min_points / span)
  
  # Check if there are enough data points for LOESS smoothing
  if (nrow(data) < min_points) {
    # Not enough data points, return raw coefficient
    smoothed_coefficient <- 0
  } else {
    # Check for zero variance in rapm values
    if (all(data$rapm == data$rapm[1])) {
      # All rapm values are the same, return the raw coefficient
      smoothed_coefficient <- 0
    } else {
      # Fit the LOESS model using historical data
      loess_fit <- loess(rapm ~ overall_round, data = data, span = span)
      
      # Get the latest overall_round
      latest_overall_round <- max(data$overall_round)
      
      # Predict the coefficient for the latest overall_round
      smoothed_coefficient <- predict(loess_fit, 
                                      newdata = data.frame(overall_round = latest_overall_round))
      
    }
  }
  weight = calculate_weight_raw(history_length = nrow(data),
                                weight_limit = weight_limit,
                                max_history = max_history)
  
  latest_rapm = data %>% arrange(-overall_round) %>% pull(rapm) %>% head(1)
  
  smoothed_coefficient_adj <- weight * smoothed_coefficient +
    (1 - weight) * latest_rapm
  
  return(list(smoothed_coefficient = smoothed_coefficient, 
              smoothed_coefficient_adj = smoothed_coefficient_adj))
}


# Function: calculate_weight_raw
# Purpose: Create weighted average betweewn model coef and LOESS derived ones
# Parameters: 
#             history_length - how many races for that driver/constructor
#             weight_limit - Max weighting toward LOESS data
#             max_history - max history we want to include in LOESS
calculate_weight_raw <- function(history_length, weight_limit = 0.9, max_history = 20) {
  weight_raw <- min(history_length / max_history, weight_limit)
  return(weight_raw)
}




# Function: get_logistic_model
# Purpose:  Call Logistic model for a given Top N & produce all the
#           key model metrics
# Parameters: 
#             results_full - Model data
#             dnf_partial - Whether to give partial credits for DNFs
get_logistic_model <- function(results_full, dnf_partial = FALSE){
  
  
  position_pred_weights <- results_full %>%
    group_by(date) %>%
    mutate(total_cars = max(position),
           cars_outside_points = total_cars - 10,
           position_pred_weight = ifelse(position <= 10, 1, 
                                         (cars_outside_points - (position -  10) + 1 ) /(cars_outside_points + 1))
    ) %>%
    ungroup() %>%
    pull(position_pred_weight)
  
  
  results_pred <- results_full 
  
  
  driver_list <- unique(results_full$driver_id)
  parent_constructor_list <- unique(results_full$parent_constructor_id)
  constructor_list <- unique(results_full$constructor_id)
  
  driver_cons_list <- c(driver_list, parent_constructor_list)
  
  #Calculate player matrix
  driv_const_matrix <- t(apply(results_full[, c('driver_id', 'parent_constructor_id', 'dnf_driver', 'dnf_constructor')], 1, 
                               function(x) make_matrix_rows(lineup = x, players_in = driver_cons_list, dnf_partial)))
  
  
  driver_matrix_range <- 1:length(driver_list)
  constructor_matrix_range <- (length(driver_list) + 1):length(driver_cons_list)
  
  
  race_dates <- unique(results_full$date)
  
  
  
  rapm_history <- data.frame(
    entity_id = integer(),
    rapm = numeric(),
    rapm_loess = numeric(),
    rapm_blended = numeric(),
    model_date = as.Date(character()),
    circuit = character(),
    season = numeric(),
    round = numeric(),
    overall_round = numeric(),
    dev_ratio = numeric()
  )
  
  i = 1
  
  
  
  
  for(i in 1:(length(race_dates))){
    
    #Get all current info
    current_race_date = race_dates[i]
    
    current_circuit = results_full %>%
      filter(date == current_race_date) %>%
      pull(circuit_id) %>% unique()
    
    current_season = results_full %>%
      filter(date == current_race_date) %>%
      pull(season) %>% unique()
    
    current_round_overall = results_full %>%
      filter(date == current_race_date) %>%
      pull(round_overall) %>% unique()
    
    current_round = as.numeric(results_full %>%
                                 filter(date == current_race_date) %>%
                                 pull(round) %>% unique())
    
    
    print(paste0("Starting ", current_circuit, ", ", substr(current_race_date, 1,4), " - ", current_race_date))
    
    #Get Index Range for the Current Date Selected
    start_index <- min(which(results_full$date <= current_race_date))  
    end_index <- max(which(results_full$date <= current_race_date))  
    
    rapm_model <- driv_const_matrix[start_index:end_index, ]
    target <- results_full[start_index:end_index, ] %>% pull('log_flag')
    
    #Set up time  - deprecated, using round/season decay
    #time_diff <- as.Date(current_race_date, origin = "1970-01-01") - 
    #  results_full[start_index:end_index, 'date']
    #time_decay_weights <- decay_function(as.numeric(time_diff), -0.005274) * rep(1, nrow(rapm_model))
    
    #Round decay
    round_diff <- as.vector(current_round_overall - results_full[start_index:end_index, 'round_overall'] + 1)$round_overall
    round_decay_weights <- decay_function(as.numeric(round_diff), 
                                          decay_rate = -round_decay) * rep(1, nrow(rapm_model))
    
    #Season Decay
    season_diff <- current_season - (results_full[start_index:end_index, ] %>% pull(season)) + 1
    season_decay_weight <- decay_function(as.numeric(round_diff), -season_decay) * rep(1, nrow(rapm_model))
    
    
    #position_weights <- position__pred_weights[start_index:end_index]
    
    prediction_weighting = round_decay_weights * season_decay_weight
    
    prediction_weighting = prediction_weighting * position_pred_weights[start_index:end_index]
    
    
    #position_weights <- position__pred_weights[start_index:end_index]
    
    #prediction_weighting = round_decay_weights
    
    rapm_response <- get_rapm_iterative_logistic(rapm_model, results_full, target, prediction_weighting )
    
    
    
    rapm_output_frame <- rapm_response[[1]] %>%
      mutate(model_date = as.Date(current_race_date),
             season = current_season,
             round = current_round,
             overall_round = current_round_overall,
             circuit = current_circuit,
             dev_ratio =   rapm_response[[3]]$dev.ratio,
             rapm_loess = NA,
             rapm_blended = NA)
    
    
    
    
    
    # Store the RAPM history
    rapm_history <- rbind(rapm_history, rapm_output_frame) %>%
      group_by(entity_id) %>%
      mutate(
        span_value = if_else(str_ends(entity_id, "-c"), constructor_span, 
                             driver_span),
        weight_value = if_else(str_ends(entity_id, "-c"), constructor_weight, 
                               driver_weight),
        temp = list(apply_loess_smoothing(cur_data(), 
                                          span = 0.3,
                                          weight_limit = max(weight_value),
                                          max_history = 40)),
        rapm_loess = ifelse(overall_round == max(overall_round), 
                            map_dbl(temp, "smoothed_coefficient"),
                            rapm_loess),
        rapm_blended = ifelse(overall_round == max(overall_round), 
                              map_dbl(temp, "smoothed_coefficient_adj"),
                              rapm_blended),
      )
    
    
    
    
    
    
    
    
    rapm_predict_model <- rapm_response[[3]]
    rapm_predict_model_driver <- rapm_response[[4]]
    rapm_predict_model_constructor <- rapm_response[[5]]
    
    
    rapm_coeff <- rapm_history %>% 
      filter(overall_round == max(overall_round))
    
    if(i != length(race_dates)){
      
      next_race_date = race_dates[i+1]
      next_race_start_index <- min(which(results_full$date == next_race_date))  
      next_race_end_index <- max(which(results_full$date == next_race_date))  
      
      results_pred[(next_race_start_index:next_race_end_index),'position_pred_loess' ] <- 
        driv_const_matrix[next_race_start_index:next_race_end_index,] %*% rapm_coeff$rapm_loess
      
      results_pred[(next_race_start_index:next_race_end_index),'position_pred_blended' ] <- 
        driv_const_matrix[next_race_start_index:next_race_end_index,] %*% rapm_coeff$rapm_blended
      
      results_pred[(next_race_start_index:next_race_end_index),'position_pred' ] <- 
        predict(rapm_predict_model, newx = driv_const_matrix[next_race_start_index:next_race_end_index,],
                type = "response")
      
      
      results_pred[(next_race_start_index:next_race_end_index),'position_pred_driver' ] <- 
        predict(rapm_predict_model_driver, newx = driv_const_matrix[next_race_start_index:next_race_end_index, 
                                                                    driver_matrix_range],
                type = "response")
      
      results_pred[(next_race_start_index:next_race_end_index),'position_pred_constructor' ] <- 
        predict(rapm_predict_model_constructor, newx = driv_const_matrix[next_race_start_index:next_race_end_index,
                                                                         constructor_matrix_range],
                type = "response")
      
    }
  }
  
  
  
  
  results_pred_test <- results_pred %>%
    filter(season > 2013, season < 2024) %>%
    filter(!is.na(position_pred))
  
  
  # Log-likelihood function
  log_likelihood <- function(y, y_pred) {
    sum(y * log(y_pred) + (1 - y) * log(1 - y_pred))
  }
  
  # Actual binary outcomes
  y <- results_pred_test$log_flag
  
  # Predictions from the models
  y_pred_full <- results_pred_test$position_pred
  y_pred_driver <- results_pred_test$position_pred_driver
  y_pred_constructor <- results_pred_test$position_pred_constructor
  
  # Compute log-likelihoods
  log_lik_full <- log_likelihood(y, y_pred_full)
  log_lik_driver <- log_likelihood(y, y_pred_driver)
  log_lik_constructor <- log_likelihood(y, y_pred_constructor)
  
  cat("Log-likelihood for the full model:", log_lik_full, "\n")
  cat("Log-likelihood for the driver-only model:", log_lik_driver, "\n")
  cat("Log-likelihood for the constructor-only model:", log_lik_constructor, "\n")
  
  
  
  # Compute the null model log-likelihood
  log_lik_null <- log_likelihood(y, mean(y))
  
  # Compute pseudo R-squared
  pseudo_r2 <- function(log_lik, log_lik_null) {
    1 - (log_lik / log_lik_null)
  }
  
  pseudo_r2_full <- pseudo_r2(log_lik_full, log_lik_null)
  pseudo_r2_driver <- pseudo_r2(log_lik_driver, log_lik_null)
  pseudo_r2_constructor <- pseudo_r2(log_lik_constructor, log_lik_null)
  
  cat("Pseudo R-squared for the full model:", pseudo_r2_full, "\n")
  cat("Pseudo R-squared for the driver-only model:", pseudo_r2_driver, "\n")
  cat("Pseudo R-squared for the constructor-only model:", pseudo_r2_constructor, "\n")
  
  
  # Calculate partial pseudo R-squared for drivers
  partial_pseudo_r2_driver <- pseudo_r2_full - pseudo_r2_constructor
  cat("Partial pseudo R-squared for drivers:", partial_pseudo_r2_driver, "\n")
  
  # Calculate partial pseudo R-squared for constructors
  partial_pseudo_r2_constructor <- pseudo_r2_full - pseudo_r2_driver
  cat("Partial pseudo R-squared for drivers:", partial_pseudo_r2_driver, "\n")
  cat("Partial pseudo R-squared for constructors:", partial_pseudo_r2_constructor, "\n")
  
  return(c(log_lik_full, log_lik_driver, log_lik_constructor, 
           pseudo_r2_full, partial_pseudo_r2_driver, partial_pseudo_r2_constructor))
  
  
}



# Hyperparameters for time-decay and LOESS Blending
season_decay= 0.75
round_decay= 0.075
constructor_span = 0.3
driver_span = 0.5
constructor_weight = 0.3
driver_weight = 0.7


dnf_partial = FALSE


#Empty Dataframe to load up
log_model_performance <- data.frame(dnf_filter = character(0),
                                    top_n_boolean = numeric(0),
                                    log_lik_full = numeric(0),
                                    log_lik_driver = numeric(0),
                                    log_lik_constructor = numeric(0),
                                    pseudo_r2_full = numeric(0),
                                    partial_pseudo_r2_driver = numeric(0),
                                    partial_pseudo_r2_constructor = numeric(0))


for(dnf_model in c('all_dnf')){
  if(dnf_model == 'no_dnf'){
    log_model_results <-  results_unfiltered %>%
      filter(finished, !is.na(position))
  }
  else{
    log_model_results <-  results_unfiltered %>%
      filter( !is.na(position)) 
  }
  for(i in c(4:23)){
    print(paste0("Running Model for DNF ", dnf_model, " and Top ", i - 1, " Results"))
    
    #Filter to Top N logistic
    log_model_results <- log_model_results  %>%
      mutate(log_flag = ifelse(position < i, 1, 0))
    
    #Include Partial or Not
    if(dnf_model == 'partial_credit_dnf'){
      log_model_metrics = get_logistic_model(results_full = log_model_results, dnf_partial = TRUE)
    }
    else{
      log_model_metrics = get_logistic_model(results_full = log_model_results, dnf_partial = FALSE)
    }
    
    #Model Metrics
    log_model_performance <- log_model_performance %>%
      add_row(dnf_filter = dnf_model,
              top_n_boolean = i - 1,
              log_lik_full = log_model_metrics[[1]],
              log_lik_driver = log_model_metrics[[2]],
              log_lik_constructor = log_model_metrics[[3]],
              pseudo_r2_full = log_model_metrics[[4]],
              partial_pseudo_r2_driver = log_model_metrics[[5]],
              partial_pseudo_r2_constructor = log_model_metrics[[6]])
    
    
  }
}




saveRDS(object = log_model_performance, file = paste0("f1dataR - Exports/Data/log_model_t20_all_dnf_performance.rds"))




