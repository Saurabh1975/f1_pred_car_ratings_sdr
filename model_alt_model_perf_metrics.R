
library(dplyr)

position_weighted = TRUE
dnf_inclusive = TRUE


get_model_dnf_weight_metrics <- function(position_weighted, dnf_inclusive){
  
  file_path =  sprintf(
      "f1dataR - Exports/Data/RAPM Outputs/rapm_history_pos%s_%s_bootstrapped_results_pred.rds",
                       ifelse(position_weighted, "Weighted", "Unweighted"),
                       ifelse(dnf_inclusive, "DNF", "noDNF"))
  
  
  results_pred <- readRDS(file_path) 
  
  
  
  
  
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
  
  
  kt_overall <-  model_metrics_full$mean_tau
  kt_overall_blended <-  model_metrics_blended$mean_tau

  
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
  
  average_kendall_weighted_overall_blended
  
  
  dcg <- function(relevance) {
    sum((2^relevance - 1) / log2(seq_along(relevance) + 1))
  }
  
  ndcg_at_k <- function(actual_positions, predicted_positions, k) {
    n <- length(actual_positions)
    if (n < k) return(NA)  # Not enough drivers in the race
    
    rel_scores <- rep(0, n)
    actual_top_k <- order(actual_positions)[1:k]
    rel_scores[actual_top_k] <- rev(seq_len(k))
    
    pred_order <- order(predicted_positions)
    pred_relevance <- rel_scores[pred_order]
    
    dcg_k <- dcg(pred_relevance[1:k])
    idcg_k <- dcg(sort(rel_scores, decreasing = TRUE)[1:k])
    
    if (idcg_k == 0) return(0)
    return(dcg_k / idcg_k)
  }
  
  
  
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
  
  
  
  
  # Function to compute MAE@k for a single race
  mae_at_k <- function(actual_positions, predicted_positions, k) {
    top_k_predicted_indices <- order(predicted_positions)[1:k]
    actual_top_k <- actual_positions[top_k_predicted_indices]
    predicted_top_k <- predicted_positions[top_k_predicted_indices]
    
    mean(abs(actual_top_k - predicted_top_k))
  }
  
  # Compute MAE@3, @6, and @10 per race and average
  mae_summary <- results_pred_test %>%
    group_by(round_overall) %>%
    summarise(
      mae_3 = mae_at_k(position, position_pred_rank, 3),
      mae_6 = mae_at_k(position, position_pred_rank, 6),
      mae_10 = mae_at_k(position, position_pred_rank, 10),
      .groups = 'drop'
    ) %>%
    summarise(
      avg_mae_3 = mean(mae_3, na.rm = TRUE),
      avg_mae_6 = mean(mae_6, na.rm = TRUE),
      avg_mae_10 = mean(mae_10, na.rm = TRUE)
    )
  
  
  
  
  # Return a named list
  metrics <- list(
    #MAE
    mae_unweighted = mae_unweighted,
    mae_weighted = mae_weighted,
    ##Kendall Tau
    #Unweighted
    kt_overall = kt_overall,
    kt_overall_blended = kt_overall_blended,
    #Weighted
    kendall_weighted_overall = average_kendall_weighted_overall,
    kendall_weighted_overall_blended = average_kendall_weighted_overall_blended,
    ##Top K
    #nDCG
    nDCG_top3 = ndcg_at_k$avg_ndcg_3,
    nDCG_top6 = ndcg_at_k$avg_ndcg_6,
    nDCG_top10 = ndcg_at_k$avg_ndcg_10,
    ##Top K
    #nDCG
    mae_top3 = mae_summary$avg_mae_3,
    mae_top6 = mae_summary$avg_mae_6,
    mae_top10 = mae_summary$avg_mae_10
  )
  
  return(metrics)


}





get_model_dnf_weight_metrics(position_weighted, dnf_inclusive)





dnf_weight_metrics_df <- data.frame()


for(position_weighted in c(TRUE, FALSE)){
  for(dnf_inclusive in c(TRUE, FALSE)){
    model_metrics <- get_model_dnf_weight_metrics(position_weighted, dnf_inclusive)
    
    model_metrics$position_weighted <- ifelse(position_weighted, 
                                              'Pos. Weighted', 'Pos. Unweighted')
    
    model_metrics$dnf_inclusive <- ifelse(dnf_inclusive, 
                                              'Included', 'Excluded')
    
    # Convert the named list to a dataframe
    metrics_df_iteration <- as.data.frame(t(unlist(model_metrics)))
    
    # Append the result to the existing dataframe
    dnf_weight_metrics_df <- rbind(dnf_weight_metrics_df, metrics_df_iteration)
    
    
    }
}

dnf_weight_metrics_df <-  dnf_weight_metrics_df %>%
  mutate(across(
    .cols = !c("position_weighted", "dnf_inclusive"),
    .fns = as.numeric
  ))


colnames(dnf_weight_metrics_df)

#Make Table


tbl_pos_weight_dnf <- dnf_weight_metrics_df %>%
  arrange(dnf_inclusive) %>%
  dplyr::select(dnf_inclusive, position_weighted,  kt_overall,  mae_unweighted,
         mae_top3, mae_top6, mae_top10) %>%
  gt() %>%
  tab_header(
    title = md("**Weighted/Unweighted Rank Model Performance**"),
    subtitle = paste0("Tested on 2014 - 2024 Races")) %>% 
  cols_label(
    position_weighted  = "Pos. Weighted",
    dnf_inclusive = "DNFs",
    kt_overall = "Kendall Tau (𝜏)",
    mae_unweighted =  "Overall",
    mae_top3 =  "T3",
    mae_top6 =  "T6",
    mae_top10 =  "T10"
  )  %>%
  fmt_number(
    columns = c(kt_overall),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(mae_unweighted, mae_top3, mae_top6, mae_top10),
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_text(
        font = c(google_font(name = "Roboto Mono")),      )
    ),
    locations = cells_body(columns = c(kt_overall,  mae_unweighted,
                                       mae_top3, mae_top6, mae_top10))
  ) %>%
  tab_spanner(
    label = "Mean Absolute Error (MAE)",
    columns = c(mae_unweighted, mae_top3, mae_top6, mae_top10)
  ) %>%
  tab_options(
    table.font.names = "Roboto", 
    table.font.size = 14,
    heading.title.font.size = 20,
    heading.subtitle.font.size = 13,
    column_labels.font.size = 13,
    column_labels.font.weight = 'bold',
    row_group.background.color = '#fAfAfA',
    row_group.font.size = 11,
    row_group.font.weight = 'bold',
    data_row.padding = px(5)
  ) 



gtsave(data = tbl_pos_weight_dnf, 
       filename = paste0("f1dataR - Exports/Visuals/Model Metrics/pos_weight_dnf_metrics.png"),
       zoom = 4)
