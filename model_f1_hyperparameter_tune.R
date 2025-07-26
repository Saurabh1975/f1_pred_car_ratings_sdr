library(purrr)
library(Kendall)
season_decay = 0.9
round_decay = 0.9

test_response <- decay_parameter_tune(season_decay, round_decay)


# Define the function to perform random parameter search
random_parameter_search <- function(n_iter = 100) {
  # Initialize an empty dataframe to store results
  results <- data.frame(season_decay = numeric(0),
                        round_decay = numeric(0),
                        mae_unweighted = numeric(0),
                        mae_weighted = numeric(0))
  
  # Perform random search
  for (i in 1:n_iter) {
    # Generate random values for season_decay and round_decay
    season_decay <- runif(1, min = 0, max = 1)
    round_decay <- runif(1, min = 0, max = 1)
    
    # Evaluate the function
    test_response <- decay_parameter_tune(season_decay, round_decay)
    
    # Store the results in the dataframe
    results <- results %>%
      add_row(season_decay = season_decay,
              round_decay = round_decay,
              mae_unweighted = test_response[[1]],
              mae_weighted = test_response[[2]])
  }
  
  return(results)
}

# Perform the random parameter search with a specified number of iterations
n_iterations <- 1
search_results <- random_parameter_search(n_iter = n_iterations)




decay_parameter_tune <- function(season_decay, round_decay){
  
  print(season_decay)
  print(round_decay)
  
  
  results_full <- results_unfiltered %>%
    filter(finished)
  
  position__pred_weights <- results_full %>%
    group_by(date) %>%
    mutate(total_cars = max(position),
           position_pred_weight = (total_cars - position + 1) / sum(1:total_cars)) %>%
    ungroup() %>%
    pull(position_pred_weight)
  
  
  #Dataframe in which we'll store the prediction
  results_pred <- results_full 
  
  
  
  
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
  
  
  
  
  
  
  results_pred <- results_pred %>%
    group_by(date) %>%
    mutate(position_pred_rank = rank(position_pred),
           total_cars = max(position),
           position_pred_weight = (total_cars - position + 1) / sum(1:total_cars),
           position_pred_diff = abs(position_pred_rank - position)) %>%
    ungroup()
  
  
  
  
  results_pred_test <- results_pred %>% filter(season > 2013)
  
  
  #MAE
  mae_unweighted <- sum(results_pred_test$position_pred_diff)/nrow(results_pred_test)
  mae_weighted <- sum(results_pred_test$position_pred_diff * results_pred_test$position_pred_weight)/sum(results_pred_test$position_pred_weight)
  
  
  #Kendall Tau
  # Compute Overall Spearman and Kendalls
  model_metrics_full <- calc_kendall_tau(data = results_pred_test, 
                                         race_col = 'date',
                                         y_test_col = 'position',
                                         y_pred_col = 'position_pred_rank')
  
  kt_overall <-  model_metrics_full$mean_tau
  
  
  ndcg_at_k <- results_pred_test %>%
    group_by(round_overall) %>%
    summarise(
      ndcg_3 = ndcg_at_k(position, position_pred_rank, 3),
      ndcg_6 = ndcg_at_k(position, position_pred_rank, 6),
      ndcg_10 = ndcg_at_k(position, position_pred_rank, 10),
      .groups = 'drop'
    ) %>%
    summarise(
      avg_ndcg_3 = mean(ndcg_3, na.rm = TRUE),
      avg_ndcg_6 = mean(ndcg_6, na.rm = TRUE),
      avg_ndcg_10 = mean(ndcg_10, na.rm = TRUE)
    )
  
  metrics <- list(
    #MAE
    mae_unweighted = mae_unweighted,
    mae_weighted = mae_weighted,
    ##Kendall Tau
    kt_overall = kt_overall,
    ##Top K
    #nDCG
    nDCG_top3 = ndcg_at_k$avg_ndcg_3,
    nDCG_top6 = ndcg_at_k$avg_ndcg_6,
    nDCG_top10 = ndcg_at_k$avg_ndcg_10
  )
  
  
  
  return(metrics)
  
}




# Define grid search function
grid_search <- function(season_decays, round_decays) {
  results <- tibble::tibble(
    season_decay = numeric(),
    round_decay = numeric(),
    mae_unweighted = numeric(),
    mae_weighted = numeric(),
    kt_overall = numeric(),
    nDCG_top3 = numeric(),
    nDCG_top6 = numeric(),
    nDCG_top10 = numeric()
  )
  
  for (season_decay in season_decays) {
    for (round_decay in round_decays) {
      test_response <- decay_parameter_tune(season_decay, round_decay)
      results <- results %>%
        add_row(season_decay = season_decay,
                round_decay = round_decay,
                #MAE
                mae_unweighted = test_response$mae_unweighted,
                mae_weighted = test_response$mae_weighted,
                #KT
                kt_overall = test_response$kt_overall,
                #nDCG
                nDCG_top3 = test_response$nDCG_top3,
                nDCG_top6 = test_response$nDCG_top6,
                nDCG_top10 = test_response$nDCG_top10
        )
    }
  }
  
  return(results)
}

# Define the parameter grid
season_decays <- c(0.75,  0.75/10, 0.75/100)
round_decays <- c(0.75,  0.75/10, 0.75/100)



# Perform the grid search
grid_results <- grid_search(season_decays, round_decays)


saveRDS(grid_results, file = 'f1dataR - Exports/Data/time_decay_grid_search.rds')