library(extrafont)
library(Matrix)
library(ppcor)
library(Kendall)


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




get_rapm_iterative_r2 <- function(rapm_model, target, weights){
  
  
  #Run Initial Model to get Lambda
  cv_model <- glmnet::cv.glmnet(x = rapm_model, ##ncol = 1058
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
  
  
  
  #Driver Only Model
  driver_only_model <- glmnet::glmnet(x = rapm_model[,1:length(driver_list)], 
                                      y = target,
                                      weights = weights,
                                      alpha = 0, 
                                      standardize = FALSE,
                                      lambda = lam,
                                      type.measure = "mae",
                                      standard.error = TRUE)
  
  
  #Constructor Only Model
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
  player_coefficients_error <- coef_model ## length = 1058
  
  
  rapm <- player_coefficients[1:length(driver_cons_list)]
  #rapm_error <- player_coefficients_error[1:length(driver_cons_list)]
  
  rapm_frame <- data.frame("entity_id" = driver_cons_list,
                           "rapm" = rapm)
  
  return(list(rapm_frame, rapm, coef_model, driver_only_model, constructor_only_model))
  
  
}




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


# Function to calculate weights based on history length
#Max History of 20 in order to capture a full season
calculate_weight_raw <- function(history_length, weight_limit = 0.9, max_history = 20) {
  weight_raw <- min(history_length / max_history, weight_limit)
  return(weight_raw)
}


dnf_partial = FALSE
get_model_performance <- function(results_full, 
                                  dnf_partial = FALSE, 
                                  save_file_name = "rapm_history_base_model"){
  
  
  #print(paste0('Constructor Span is: ', constructor_span))
  #print(paste0('Driver Span is: ', driver_span))
  
  position_pred_weights <- results_full %>%
    group_by(date) %>%
    mutate(total_cars = max(position),
           cars_outside_points = total_cars - 10,
           position_pred_weight = ifelse(position <= 10, 1, (cars_outside_points - (position -  10) + 1 ) /(cars_outside_points + 1))
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
    target <- results_full[start_index:end_index, ] %>% pull('position')
    
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
    season_decay_weight <- decay_function(as.numeric(season_diff), -season_decay) * rep(1, nrow(rapm_model))
    
    
    #position_weights <- position__pred_weights[start_index:end_index]
    
    prediction_weighting = round_decay_weights * season_decay_weight
    
    prediction_weighting = prediction_weighting * position_pred_weights[start_index:end_index]
    
    
    #position_weights <- position__pred_weights[start_index:end_index]
    
    #prediction_weighting = round_decay_weights
    
    rapm_response <- get_rapm_iterative_r2(rapm_model, target, prediction_weighting )
    
    
    
    rapm_output_frame <- rapm_response[[1]] %>%
      mutate(model_date = as.Date(current_race_date),
             season = current_season,
             round = current_round,
             overall_round = current_round_overall,
             circuit = current_circuit,
             dev_ratio =   rapm_response[[3]]$dev.ratio,
             rapm_loess = NA,
             rapm_blended = NA)
    
    
    
    
    
    library(purrr)
    library(stringr)
    
    
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
                                          max_history = max_history_value)),
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
  
  
  
  #saveRDS(object = rapm_history, file = paste0("f1dataR - Exports/", save_file_name, '.rds'))
  
  
  
  
  
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
  
  
  
  results_pred_test <- results_pred %>% filter(season > 2013, season < 2025)
  
  
  
  mae_unweighted <- sum(results_pred_test$position_pred_diff)/nrow(results_pred_test)
  mae_weighted <- sum(results_pred_test$position_pred_diff * results_pred_test$position_pred_weight)/sum(results_pred_test$position_pred_weight)
  
  
  
  
  
  
  y = results_pred_test$position
  
  ###Spearman Rho + partial residual
  
  
  # Compute Overall Spearman and Kendalls
  model_metrics_full <- calc_kendall_tau(data = results_pred_test, 
                                         race_col = 'date',
                                         y_test_col = 'position',
                                         y_pred_col = 'position_pred_rank')
  
  
  model_metrics_blended <- calc_kendall_tau(data = results_pred_test, 
                                            race_col = 'date',
                                            y_test_col = 'position',
                                            y_pred_col = 'position_pred_rank_blended')
  
  ### Calculate Spermans
  spearman_overall <- model_metrics_full$mean_spearman_rho
  
  spearman_overall_blended <- model_metrics_blended$mean_spearman_rho
  
  
  ### Calculate Kendall's Tau
  kt_overall <-  model_metrics_full$mean_tau
  
  kt_overall_blended <- model_metrics_blended$mean_tau
  
  
  
  
  
  ##Calculate Partials 
  
  
  # Define a function to calculate partial correlations using the formula
  calculate_partial_correlation <- function(corr_matrix) {
    precision_matrix <- solve(corr_matrix)
    partial_corr_driver <- -precision_matrix["driver_predictions", "y"] /
      sqrt(precision_matrix["driver_predictions", "driver_predictions"] * precision_matrix["y", "y"])
    
    partial_corr_constructor <- -precision_matrix["constructor_predictions", "y"] /
      sqrt(precision_matrix["constructor_predictions", "constructor_predictions"] * precision_matrix["y", "y"])
    
    return(list(driver = partial_corr_driver, constructor = partial_corr_constructor))
  }
  
  # Group by race and calculate partial correlations for each race
  race_partial_corrs <- results_pred_test %>%
    group_by(date) %>%
    summarise(
      spearman_driver = cor(position, position_pred_rank_driver, method = "spearman"),
      spearman_constructor = cor(position, position_pred_rank_const, method = "spearman"),
      spearman_driver_constructor = cor(position_pred_rank_driver, position_pred_rank_const, method = "spearman"),
      kendall_driver = cor(position, position_pred_rank_driver, method = "kendall"),
      kendall_constructor = cor(position, position_pred_rank_const, method = "kendall"),
      kendall_driver_constructor = cor(position_pred_rank_driver, position_pred_rank_const, method = "kendall")
    ) %>%
    mutate(
      spearman_corr_matrix = pmap(list(spearman_driver, spearman_constructor, spearman_driver_constructor), 
                                  ~matrix(c(1, ..1, ..2, ..1, 1, ..3, ..2, ..3, 1), nrow = 3, ncol = 3, 
                                          dimnames = list(c("y", "driver_predictions", "constructor_predictions"), 
                                                          c("y", "driver_predictions", "constructor_predictions")))),
      kendall_corr_matrix = pmap(list(kendall_driver, kendall_constructor, kendall_driver_constructor), 
                                 ~matrix(c(1, ..1, ..2, ..1, 1, ..3, ..2, ..3, 1), nrow = 3, ncol = 3, 
                                         dimnames = list(c("y", "driver_predictions", "constructor_predictions"), 
                                                         c("y", "driver_predictions", "constructor_predictions")))),
      partial_spearman = map(spearman_corr_matrix, calculate_partial_correlation),
      partial_kendall = map(kendall_corr_matrix, calculate_partial_correlation)
    ) %>%
    ungroup()
  
  # Extract partial correlations for drivers and constructors
  race_partial_corrs <- race_partial_corrs %>%
    mutate(
      partial_spearman_driver = map_dbl(partial_spearman, "driver"),
      partial_spearman_constructor = map_dbl(partial_spearman, "constructor"),
      partial_kendall_driver = map_dbl(partial_kendall, "driver"),
      partial_kendall_constructor = map_dbl(partial_kendall, "constructor")
    )
  
  
  average_partial_spearman_driver <- mean(race_partial_corrs$partial_spearman_driver, na.rm = TRUE)
  average_partial_spearman_constructor <- mean(race_partial_corrs$partial_spearman_constructor, na.rm = TRUE)
  average_partial_kendall_driver <- mean(race_partial_corrs$partial_kendall_driver, na.rm = TRUE)
  average_partial_kendall_constructor <- mean(race_partial_corrs$partial_kendall_constructor, na.rm = TRUE)
  
  
  
  ###Weighteed Correlations
  
  ##Weighteed Correlations - Spearman
  
  library(wCorr)
  
  # Function to calculate weighted Spearman's rho
  weighted_spearman <- function(x, y, weights) {
    return(wCorr::weightedCorr(x, y, weights, method = "spearman"))
  }
  
  # Calculate weighted Spearman's rho for each race
  race_weighted_spearman <- results_pred_test %>%
    group_by(date) %>%
    summarise(
      weighted_spearman_overall = weighted_spearman(position, position_pred_rank, position_pred_weight),
      weighted_spearman_blended = weighted_spearman(position, position_pred_rank_blended, position_pred_weight),
      weighted_spearman_driver = weighted_spearman(position, position_pred_rank_driver, position_pred_weight),
      weighted_spearman_constructor = weighted_spearman(position, position_pred_rank_const, position_pred_weight),
      weighted_spearman_driver_constructor = weighted_spearman(position_pred_rank_driver, position_pred_rank_const, position_pred_weight)
    )
  
  # Average the weighted Spearman's rho across all races
  average_weighted_spearman_overall <- mean(race_weighted_spearman$weighted_spearman_overall, na.rm = TRUE)
  average_weighted_spearman_blended <- mean(race_weighted_spearman$weighted_spearman_blended, na.rm = TRUE)
  
  average_weighted_spearman_driver <- mean(race_weighted_spearman$weighted_spearman_driver, na.rm = TRUE)
  average_weighted_spearman_constructor <- mean(race_weighted_spearman$weighted_spearman_constructor, na.rm = TRUE)
  average_weighted_spearman_driver_constructor <- mean(race_weighted_spearman$weighted_spearman_driver_constructor, na.rm = TRUE)
  
  
  # Create the weighted correlation matrix manually using averaged values
  weighted_corr_matrix_avg <- matrix(NA, ncol = 3, nrow = 3)
  colnames(weighted_corr_matrix_avg) <- rownames(weighted_corr_matrix_avg) <- c("y", "driver_predictions", "constructor_predictions")
  
  weighted_corr_matrix_avg["y", "y"] <- 1
  weighted_corr_matrix_avg["driver_predictions", "driver_predictions"] <- 1
  weighted_corr_matrix_avg["constructor_predictions", "constructor_predictions"] <- 1
  
  weighted_corr_matrix_avg["y", "driver_predictions"] <- average_weighted_spearman_driver
  weighted_corr_matrix_avg["driver_predictions", "y"] <- average_weighted_spearman_driver
  
  weighted_corr_matrix_avg["y", "constructor_predictions"] <- average_weighted_spearman_constructor
  weighted_corr_matrix_avg["constructor_predictions", "y"] <- average_weighted_spearman_constructor
  
  weighted_corr_matrix_avg["driver_predictions", "constructor_predictions"] <- average_weighted_spearman_driver_constructor
  weighted_corr_matrix_avg["constructor_predictions", "driver_predictions"] <- average_weighted_spearman_driver_constructor
  
  
  
  # Invert the weighted correlation matrix to get the precision matrix
  precision_matrix_avg <- solve(weighted_corr_matrix_avg)
  
  
  # Partial Spearman's rho for drivers
  partial_spearman_weighted_driver_avg <- -precision_matrix_avg["driver_predictions", "y"] /
    sqrt(precision_matrix_avg["driver_predictions", "driver_predictions"] * precision_matrix_avg["y", "y"])
  
  # Partial Spearman's rho for constructors
  partial_spearman_weighted_constructor_avg <- -precision_matrix_avg["constructor_predictions", "y"] /
    sqrt(precision_matrix_avg["constructor_predictions", "constructor_predictions"] * precision_matrix_avg["y", "y"])
  
  
  
  
  
  
  
  #Weighted KT
  
  # Function to calculate weighted Kendall's tau by hand
  calculate_weighted_kendall_tau <- function(x, y, weights) {
    n <- length(x)
    concordant <- 0
    discordant <- 0
    total_weight <- 0
    
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        weight_ij <- weights[i] * weights[j]
        total_weight <- total_weight + weight_ij
        
        if ((x[i] < x[j] && y[i] < y[j]) || (x[i] > x[j] && y[i] > y[j])) {
          concordant <- concordant + weight_ij
        } else if ((x[i] < x[j] && y[i] > y[j]) || (x[i] > x[j] && y[i] < y[j])) {
          discordant <- discordant + weight_ij
        }
      }
    }
    
    weighted_kendall_tau <- (concordant - discordant) / total_weight
    return(weighted_kendall_tau)
  }
  
  
  
  # Group by race and calculate weighted Kendall's tau for each race
  race_results <- results_pred_test %>%
    group_by(date) %>%
    summarise(
      weighted_kendall_overall = calculate_weighted_kendall_tau(position, position_pred_rank, position_pred_weight),
      weighted_kendall_blended = calculate_weighted_kendall_tau(position, position_pred_rank_blended, position_pred_weight),
      weighted_kendall_driver = calculate_weighted_kendall_tau(position, position_pred_rank_driver, position_pred_weight),
      weighted_kendall_constructor = calculate_weighted_kendall_tau(position, position_pred_rank_const, position_pred_weight),
      weighted_kendall_driver_constructor = calculate_weighted_kendall_tau(position_pred_rank_driver, position_pred_rank_const, position_pred_weight)
    ) %>%
    ungroup()
  
  
  average_kendall_weighted_overall <- mean(race_results$weighted_kendall_overall, na.rm = TRUE)
  average_kendall_weighted_overall_blended <- mean(race_results$weighted_kendall_blended, na.rm = TRUE)
  average_kendall_driver <- mean(race_results$weighted_kendall_driver, na.rm = TRUE)
  average_kendall_constructor <- mean(race_results$weighted_kendall_constructor, na.rm = TRUE)
  average_kendall_driver_constructor <- mean(race_results$weighted_kendall_driver_constructor, na.rm = TRUE)
  
  
  
  # Create the weighted correlation matrix using the averaged values
  weighted_corr_matrix_avg <- matrix(NA, ncol = 3, nrow = 3)
  colnames(weighted_corr_matrix_avg) <- rownames(weighted_corr_matrix_avg) <- c("y", "driver_predictions", "constructor_predictions")
  
  weighted_corr_matrix_avg["y", "y"] <- 1
  weighted_corr_matrix_avg["driver_predictions", "driver_predictions"] <- 1
  weighted_corr_matrix_avg["constructor_predictions", "constructor_predictions"] <- 1
  
  weighted_corr_matrix_avg["y", "driver_predictions"] <- average_kendall_driver
  weighted_corr_matrix_avg["driver_predictions", "y"] <- average_kendall_driver
  
  weighted_corr_matrix_avg["y", "constructor_predictions"] <- average_kendall_constructor
  weighted_corr_matrix_avg["constructor_predictions", "y"] <- average_kendall_constructor
  
  weighted_corr_matrix_avg["driver_predictions", "constructor_predictions"] <- average_kendall_driver_constructor
  weighted_corr_matrix_avg["constructor_predictions", "driver_predictions"] <- average_kendall_driver_constructor
  
  
  # Invert the weighted correlation matrix to get the precision matrix
  precision_matrix_avg <- solve(weighted_corr_matrix_avg)
  
  # Partial Kendall's tau for drivers
  partial_kendall_weighted_driver_avg <- -precision_matrix_avg["driver_predictions", "y"] /
    sqrt(precision_matrix_avg["driver_predictions", "driver_predictions"] * precision_matrix_avg["y", "y"])
  
  # Partial Kendall's tau for constructors
  partial_kendall_weighted_constructor_avg <- -precision_matrix_avg["constructor_predictions", "y"] /
    sqrt(precision_matrix_avg["constructor_predictions", "constructor_predictions"] * precision_matrix_avg["y", "y"])
  
  
  
  
  # Return a named list
  metrics <- list(
    #MAE
    mae_unweighted = mae_unweighted,
    mae_weighted = mae_weighted,
    ##Spearman
    #Speaman Unweighted
    spearman_overall = spearman_overall,
    spearman_overall_blended = spearman_overall_blended,
    spearman_driver_partial = average_partial_spearman_driver,
    spearman_constructor_partial = average_partial_spearman_constructor,
    spearman_const_driver_ratio = (average_partial_spearman_constructor^2)/(average_partial_spearman_driver^2),
    #Speaman Weighted
    spearman_weighted_overall = average_weighted_spearman_overall,
    spearman_weighted_blended = average_weighted_spearman_blended,
    spearman_weighted_driver_partial = partial_spearman_weighted_driver_avg,
    spearman_weighted_constructor_partial = partial_spearman_weighted_constructor_avg,
    spearman_weighted_const_driver_ratio = (partial_spearman_weighted_constructor_avg^2)/(partial_spearman_weighted_driver_avg^2),
    ##Kendall Tau
    #Unweighted
    kt_overall = kt_overall,
    kt_overall_blended = kt_overall_blended,
    kendall_driver_partial = average_partial_kendall_driver,
    kendall_constructor_partial = average_partial_kendall_constructor,
    kendall_const_driver_ratio = (average_partial_kendall_constructor^2)/(average_partial_kendall_driver^2),
    #Weighted
    kendall_weighted_overall = average_kendall_weighted_overall,
    kendall_weighted_overall_blended = average_kendall_weighted_overall_blended,
    kendall_weighted_driver_partial = partial_kendall_weighted_driver_avg,
    kendall_weighted_constructor_partial = partial_kendall_weighted_constructor_avg,
    kendall_weighted_const_driver_ratio = (partial_kendall_weighted_constructor_avg^2)/(partial_kendall_weighted_driver_avg^2)
  )
  
  
  
  return(metrics)
  
}






# Hyperparameters for time-decay and LOESS Blending

# Hyperparameters for time-decay and LOESS Blending
season_decay= 0.75
round_decay= 0.075
constructor_span = 0.3
driver_span = 0.3
constructor_weight = 0.3
driver_weight = 0.3
max_history_value = 40





metrics_df <- data.frame()

#'no_dnf', 'partial_credit_dnf', 'all_dnf', 
# 'partial_credit_dnf', 'all_dnf', 'quali'

for (dnf_model in c( 'partial_credit_dnf', 'all_dnf', 'quali')) {
  cat("Processing dnf_model:", dnf_model, "\n")
  
  if (dnf_model == 'no_dnf') {
    log_model_results <- results_unfiltered %>%
      filter(finished)
  } else if (dnf_model == 'quali') {
    log_model_results <- results_unfiltered %>%
      filter(!is.na(quali_position)) %>%
      mutate(position = quali_position)
  } else {
    log_model_results <- results_unfiltered
  }
  
  if (dnf_model != 'partial_credit_dnf') {
    model_metrics <- get_model_performance(
      results_full = log_model_results,
      dnf_partial = TRUE,
      save_file_name = paste0('rapm_history_posWeighted_', dnf_model)
    )
  } else {
    model_metrics <- get_model_performance(
      results_full = log_model_results,
      dnf_partial = FALSE,
      save_file_name = paste0('rapm_history_posWeighted_', dnf_model)
    )
  }
  
  # Add the dnf_model to the metrics
  model_metrics$dnf_model <- dnf_model
  
  # Convert the named list to a dataframe
  metrics_df_iteration <- as.data.frame(t(unlist(model_metrics)))
  
  # Append the result to the existing dataframe
  metrics_df <- rbind(metrics_df, metrics_df_iteration)
}

# Print the final dataframe
print(metrics_df)




saveRDS(object = metrics_df, file = paste0("f1dataR - Exports/Data/model_metrics_core.rds"))


