## File: f1_core_model.R
## Purpose: Core model run for F1 data

library(Matrix)
library(stringr)
library(Kendall)
library(boot)

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
  
  #dnf flag assignment if needed
  dnf_driver <-  as.logical(lineup[3])
  dnf_constructor <- as.logical(lineup[4])
  
  
  #Assign sparse rows
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
  
  #Return one sparse row
  return(zeroRow)
  
}



# Function: get_rapm_iterative_r2
# Purpose: Make the sparse matrix for processing
# Parameters: 
#             rapm_model - sparse matrix (X) for model training
#             target - list of rank finishes corresponding to rapm_model rows
#             weights - weights for training


get_rapm_iterative_r2 <- function(rapm_model, target, weights){
  
  
  #Run Initial Model to get Lambda
  cv_model <- glmnet::cv.glmnet(x = rapm_model,
                                y = target,
                                alpha = 0, 
                                weights = weights,
                                standardize = FALSE, 
                                type.measure = "mae")
  #Best Lambda
  lam <- cv_model$lambda.min 
  
  #Model refit using that lambda
  coef_model <- glmnet::glmnet(x = rapm_model, 
                               y = target,
                               weights = weights,
                               alpha = 0, 
                               standardize = FALSE,
                               lambda = lam,
                               type.measure = "mae",
                               standard.error = TRUE)
  
  boot_results <- boot(data =  data.frame(target = target, rapm_model, weights = weights), 
                       statistic = ridge_func_boot, 
                       R = 50)  # number of bootstrap replicates
  
  se <- apply(boot_results$t, 2, sd)
  se <- se[2:length(se)]
  
  
  #Driver Only Model for partial r2
  driver_only_model <- glmnet::glmnet(x = rapm_model[,1:length(driver_list)], 
                                      y = target,
                                      weights = weights,
                                      alpha = 0, 
                                      standardize = FALSE,
                                      lambda = lam,
                                      type.measure = "mae",
                                      standard.error = TRUE)
  
  
  #Constructor Only Model for partial r2
  constructor_only_model <- glmnet::glmnet(
    x = rapm_model[,(length(driver_list) + 1):length(driver_cons_list)], 
    y = target,
    weights = weights,
    alpha = 0, 
    standardize = FALSE,
    lambda = lam,
    type.measure = "mae",
    standard.error = TRUE)
  
  #Pull Coefficients
  player_coefficients <- coef_model$beta ## length = 1058
  player_coefficients_error <- se ## length = 1058
  
  
  rapm <- player_coefficients[1:length(driver_cons_list)]
  
  rapm_frame <- data.frame("entity_id" = driver_cons_list,
                           "rapm" = rapm)
  
  return(list(rapm_frame, player_coefficients_error, coef_model, driver_only_model, constructor_only_model))
  
  
}


ridge_func_boot <- function(data, indices) {
  d <- data[indices,]
  x_boot <- as.matrix(d[, -c(1,ncol(d))])  # All columns except first (target) and last (weights)
  y_boot <- d[, 1]  # First column (target)
  weights_boot <- d[, ncol(d)]  # Last column (weights)
  
  cv_model <- glmnet::cv.glmnet(x = x_boot,
                                y = y_boot,
                                alpha = 0, 
                                weights = weights_boot,
                                standardize = FALSE, 
                                type.measure = "mae")
  
  lam <- cv_model$lambda.min
  
  coef_model <- glmnet::glmnet(x = x_boot, 
                               y = y_boot,
                               alpha = 0, 
                               weights = weights_boot,
                               standardize = FALSE,
                               lambda = lam,
                               type.measure = "mae")
  
  return(as.vector(coef(coef_model)))
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








#Constructor Weight: 0.3
#Driver Weight: 0.7
#Constructor Span: 0.3
#Driver Span: 0.5



# Hyperparameters for time-decay and LOESS Blending
season_decay= 0.75
round_decay= 0.075
constructor_span = 0.3
driver_span = 0.5
constructor_weight = 0.3
driver_weight = 0.7


# Set the data for the model
# Currently using the DNF-inclusive model
results_full <- results_unfiltered %>%
  filter(finished)

#Dataframe in which we'll store the prediction
results_pred <- results_full 

# Get weightint based on predictions
position_pred_weights <- results_full %>%
  group_by(date) %>%
  mutate(total_cars = max(position),
         cars_outside_points = total_cars - 10,
         position_pred_weight = ifelse(position <= 10, 1, (cars_outside_points - (position -  10) + 1 ) /(cars_outside_points + 1))
  ) %>%
  ungroup() %>%
  pull(position_pred_weight)


# Get list of drivers/construcotrs
driver_list <- unique(results_full$driver_id)
parent_constructor_list <- unique(results_full$parent_constructor_id)
constructor_list <- unique(results_full$constructor_id)

driver_cons_list <- c(driver_list, parent_constructor_list)


# Create the sparse matrix
driv_const_matrix <- t(apply(results_full[, c('driver_id', 'parent_constructor_id', 'dnf_driver', 'dnf_constructor')], 1, 
                             function(x) make_matrix_rows(lineup = x, players_in = driver_cons_list)))



# Set ranges for the loop to iterate through
driver_matrix_range <- 1:length(driver_list)
constructor_matrix_range <- (length(driver_list) + 1):length(driver_cons_list)
race_dates <- unique(results_full$date)



#Empty dataframe that we'll fill in
rapm_history <- data.frame(
  entity_id = integer(),
  rapm = numeric(),
  rapm_loess = numeric(),
  rapm_blended = numeric(),
  rapm_error = numeric(),
  model_date = as.Date(character()),
  circuit = character(),
  season = numeric(),
  round = numeric(),
  overall_round = numeric(),
  dev_ratio = numeric()
)




# For Loop to create/predict models per race
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
  
  
  
  # Status print
  print(paste0("Starting ", current_circuit, ", ", substr(current_race_date, 1,4), " - ", current_race_date))
  
  # Get Index Range for the Current Date Selected
  start_index <- min(which(results_full$date <= current_race_date))  
  end_index <- max(which(results_full$date <= current_race_date))  
  
  
  # Get X/y items to pass to model
  rapm_model <- driv_const_matrix[start_index:end_index, ]
  target <- results_full[start_index:end_index, ] %>% pull('position')
  
  # Round decay
  round_diff <- as.vector(current_round_overall - results_full[start_index:end_index, 'round_overall'] + 1)$round_overall
  round_decay_weights <- decay_function(as.numeric(round_diff), 
                                        decay_rate = -round_decay) * rep(1, nrow(rapm_model))
  
  # Season Decay
  season_diff <- current_season - (results_full[start_index:end_index, ] %>% pull(season)) + 1
  season_decay_weight <- decay_function(as.numeric(season_diff), -season_decay) * rep(1, nrow(rapm_model))
  
  
  
  
  
  
  
  # Apply the position weighting + time decay weighting
  prediction_weighting = round_decay_weights * season_decay_weight
  
  prediction_weighting = prediction_weighting * position_pred_weights[start_index:end_index]
  
  
  # Get Response dataframe
  rapm_response <- get_rapm_iterative_r2(rapm_model, target, prediction_weighting )
  
  
  # Store the outputs
  rapm_output_frame <- rapm_response[[1]] %>%
    mutate(model_date = as.Date(current_race_date),
           season = current_season,
           round = current_round,
           overall_round = current_round_overall,
           circuit = current_circuit,
           dev_ratio =   rapm_response[[3]]$dev.ratio,
           rapm_loess = NA,
           rapm_blended = NA,
           rapm_error = rapm_response[[2]])
  
  
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
  
  
  
  
  
  
  # Store the other ancillary outputs
  rapm_predict_model <- rapm_response[[3]]
  rapm_predict_model_driver <- rapm_response[[4]]
  rapm_predict_model_constructor <- rapm_response[[5]]
  
  # Store the coefficients
  rapm_coeff <- rapm_history %>% 
    filter(overall_round == max(overall_round))
  
  # Store all the race predictions less the last race
  if(i != length(race_dates)){
    
    next_race_date = race_dates[i+1]
    next_race_start_index <- min(which(results_full$date == next_race_date))  
    next_race_end_index <- max(which(results_full$date == next_race_date))  
    
    results_pred[(next_race_start_index:next_race_end_index),'position_pred' ] <- 
      predict(rapm_predict_model, newx = driv_const_matrix[next_race_start_index:next_race_end_index,])
    
    results_pred[(next_race_start_index:next_race_end_index),'position_pred_loess' ] <- 
      driv_const_matrix[next_race_start_index:next_race_end_index,] %*% rapm_coeff$rapm_loess
    
    results_pred[(next_race_start_index:next_race_end_index),'position_pred_blended' ] <- 
      driv_const_matrix[next_race_start_index:next_race_end_index,] %*% rapm_coeff$rapm_blended
    
    results_pred[(next_race_start_index:next_race_end_index),'position_pred' ] <- 
      predict(rapm_predict_model, newx = driv_const_matrix[next_race_start_index:next_race_end_index,])
    
    
    results_pred[(next_race_start_index:next_race_end_index),'position_pred_driver' ] <- 
      predict(rapm_predict_model_driver, newx = driv_const_matrix[next_race_start_index:next_race_end_index, 
                                                                  driver_matrix_range])
    
    results_pred[(next_race_start_index:next_race_end_index),'position_pred_constructor' ] <- 
      predict(rapm_predict_model_constructor, newx = driv_const_matrix[next_race_start_index:next_race_end_index,
                                                                       constructor_matrix_range])
    
  }
}







# Save Down Model
saveRDS(object = rapm_history, 
        file = paste0("f1dataR - Exports/Models/rapm_history_posWeighted_noDNF_bootstrapped.rds"))
saveRDS(object = rapm_history, 
        file = paste0("f1dataR - Exports/Data/rapm_history_posWeighted_noDNF_bootstrapped.csv"))





#### Ad Hoc Model Metric Testing
results_pred <- results_pred %>%
  group_by(date) %>%
  mutate(position_pred_rank = rank(position_pred),
         position_pred_rank_loess = rank(position_pred_loess),
         position_pred_rank_blended = rank(position_pred_blended),
         position_pred_rank_driver = rank(position_pred_driver),
         position_pred_rank_const = rank(position_pred_constructor),
         total_cars = max(position),
         position_pred_weight = (total_cars - position + 1) / total_cars,
         position_pred_diff = abs(position_pred_rank - position)) %>%
  ungroup()



# Test results start after warm start of 2012/2013
results_pred_test <- results_pred %>% filter(season > 2013)




#MAE
mae_unweighted <- sum(results_pred_test$position_pred_diff)/nrow(results_pred_test)
mae_weighted <- sum(results_pred_test$position_pred_diff * results_pred_test$position_pred_weight)/sum(results_pred_test$position_pred_weight)
print(paste0("MAE Unweighted: ", mae_unweighted))
print(paste0("MAE Weighted: ", mae_weighted))



## Kendall & Spearman


# Function: calc_kendall_tau
# Purpose: XXXX
# Parameters: 
#             XXX - XXX
#             XXX - XXX
#             XXX - XXX
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




model_metrics_blended <- calc_kendall_tau(data = results_pred_test, 
                                          race_col = 'date',
                                          y_test_col = 'position',
                                          y_pred_col = 'position_pred_rank_blended')


#Get Model Metrics
model_metrics_full <- calc_kendall_tau(data = results_pred_test, 
                                       race_col = 'date',
                                       y_test_col = 'position',
                                       y_pred_col = 'position_pred_rank')


model_metrics_blended <- calc_kendall_tau(data = results_pred_test, 
                                          race_col = 'date',
                                          y_test_col = 'position',
                                          y_pred_col = 'position_pred_rank_blended')
# Spearman Rho
spearman_overall <- model_metrics_full$mean_spearman_rho
spearman_overall_blended <- model_metrics_blended$mean_spearman_rho
print(paste0("Spearman Overall: ", spearman_overall, " - Blended: ", spearman_overall))


# SKendall Tau
kt_overall <-  model_metrics_full$mean_tau
kt_overall_blended <- model_metrics_blended$mean_tau
print(paste0("Kendall Tau Overall: ", kt_overall, " - Blended: ", kt_overall_blended))


