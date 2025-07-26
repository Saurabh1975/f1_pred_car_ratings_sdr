
# Generate dynamic filename
filename <- sprintf("f1dataR - Exports/Data/rapm_history_pos%s_%s_bootstrapped",
                    ifelse(position_weighted, "Weighted", "Unweighted"),
                    ifelse(dnf_inclusive, "DNF", "noDNF"))


results_pred_test <- readRDS(paste0(filename, "_results_pred.rds"))


#### Ad Hoc Model Metric Testing
results_pred_test <- results_pred_test %>%
  group_by(date) %>%
  filter(finished) %>%
  filter(season > 2013)  %>%
  mutate(
        position_adj = rank(position),
        position_pred_rank = rank(position_pred),
         position_pred_rank_loess = rank(position_pred_loess),
         position_pred_rank_blended = rank(position_pred_blended),
         position_pred_rank_driver = rank(position_pred_driver),
         position_pred_rank_const = rank(position_pred_constructor),
         total_cars = max(position),
         position_pred_weight = (total_cars - position + 1) / total_cars,
         position_pred_diff = abs(position_pred_rank - position_adj),
         position_pred_diff_loess = abs(position_pred_rank_loess - position_adj),
         position_pred_diff_blended = abs(position_pred_rank_blended - position_adj)) 






#MAE Base
mae_unweighted <- sum(results_pred_test$position_pred_diff)/nrow(results_pred_test)
mae_weighted <- sum(results_pred_test$position_pred_diff * results_pred_test$position_pred_weight)/sum(results_pred_test$position_pred_weight)
print(paste0("MAE Unweighted: ", mae_unweighted))
print(paste0("MAE Weighted: ", mae_weighted))

#MAE Blended
mae_unweighted_blended <- sum(results_pred_test$position_pred_diff_blended)/nrow(results_pred_test)
mae_weighted_blended <- sum(results_pred_test$position_pred_diff_blended * results_pred_test$position_pred_weight)/sum(results_pred_test$position_pred_weight)
print(paste0("MAE Unweighted: ", mae_unweighted_blended))
print(paste0("MAE Weighted: ", mae_weighted_blended))



model_metrics_blended <- calc_kendall_tau(data = results_pred_test, 
                                          race_col = 'date',
                                          y_test_col = 'position_adj',
                                          y_pred_col = 'position_pred_rank_blended')


#Get Model Metrics
model_metrics_full <- calc_kendall_tau(data = results_pred_test, 
                                       race_col = 'date',
                                       y_test_col = 'position_adj',
                                       y_pred_col = 'position_pred_rank')


# Spearman Rho
spearman_overall <- model_metrics_full$mean_spearman_rho
spearman_overall_blended <- model_metrics_blended$mean_spearman_rho
print(paste0("Spearman Overall: ", spearman_overall, " - Blended: ", spearman_overall))


# SKendall Tau
kt_overall <-  model_metrics_full$mean_tau
kt_overall_blended <- model_metrics_blended$mean_tau
print(paste0("Kendall Tau Overall: ", kt_overall, " - Blended: ", kt_overall_blended))



dnf_weight_perf_metrics <- data.frame(
  position_weighted = position_weighted,
  dnf_inclusive = dnf_inclusive,
  mae_unweighted = mae_unweighted,
  mae_weighted = mae_weighted,
  mae_unweighted_blended = mae_unweighted_blended,
  mae_weighted_blended = mae_weighted_blended,
  spearman_overall = spearman_overall,
  spearman_overall_blended = spearman_overall_blended,
  kendall_tau_overall = kt_overall,
  kendall_tau_overall_blended = kt_overall_blended
)
