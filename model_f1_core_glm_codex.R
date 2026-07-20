## Separate driver/constructor regulation-decay variant of model_f1_core_glm.R
##
## This script retains the rolling-RAPM output shape of the core model.  Unlike
## row-level sample weights, which affect both entities in a result equally,
## it uses alternating ridge updates: driver estimates receive driver-specific
## regulation weights and constructor estimates receive constructor-specific
## regulation weights.

library(dplyr)
library(purrr)
library(stringr)
library(Matrix)
library(Kendall)
library(glmnet)

source("data_f1_api_clean.R")

make_matrix_rows_codex <- function(lineup, players_in) {
  row <- numeric(length(players_in))
  row[match(lineup[1], players_in)] <- 1
  row[match(lineup[2], players_in)] <- 1
  row
}

decay_function_codex <- function(time_diff, decay_rate) {
  pmin(pmax(exp(decay_rate * time_diff), 0), 1)
}

# A penalty is applied once for every regulation boundary crossed between an
# observation and the race being predicted.  Lower values discard more of the
# prior-regulation signal.  Major changes are deliberately stronger.
regulation_penalty <- c(
  "2014" = "major",
  "2017" = "minor",
  "2022" = "minor",
  "2026" = "major"
)

driver_regulation_multiplier <- c(major = 0.60, minor = 0.85)
constructor_regulation_multiplier <- c(major = 0.20, minor = 0.65)

regulation_weight_codex <- function(observation_season, target_season,
                                    boundary_type, multipliers) {
  boundary_year <- as.integer(names(boundary_type))
  vapply(observation_season, function(season) {
    crossed <- boundary_type[boundary_year > season & boundary_year <= target_season]
    if (length(crossed) == 0) return(1)
    prod(unname(multipliers[crossed]))
  }, numeric(1))
}

# Coordinate-descent ridge.  Each update is a ridge regression on the outcome
# remaining after the other component's current estimate.  This makes the
# component-specific weights meaningful without changing the prediction schema.
fit_separate_regulation_ridge <- function(driver_x, constructor_x, y,
                                          driver_weights, constructor_weights,
                                          lambda, iterations = 6) {
  driver_fit <- glmnet(driver_x, y, alpha = 0, lambda = lambda,
                       weights = driver_weights, standardize = FALSE)
  driver_beta <- as.numeric(coef(driver_fit))[-1]
  constructor_beta <- numeric(ncol(constructor_x))

  for (iteration in seq_len(iterations)) {
    constructor_fit <- glmnet(
      constructor_x, y - as.numeric(driver_x %*% driver_beta),
      alpha = 0, lambda = lambda, weights = constructor_weights,
      standardize = FALSE
    )
    constructor_beta <- as.numeric(coef(constructor_fit))[-1]

    driver_fit <- glmnet(
      driver_x, y - as.numeric(constructor_x %*% constructor_beta),
      alpha = 0, lambda = lambda, weights = driver_weights,
      standardize = FALSE
    )
    driver_beta <- as.numeric(coef(driver_fit))[-1]
  }

  list(driver_beta = driver_beta, constructor_beta = constructor_beta,
       intercept = 0, lambda = lambda)
}

apply_loess_smoothing_codex <- function(data, span, weight_limit, max_history) {
  min_points <- ceiling(3 / span)
  latest <- data %>% arrange(desc(overall_round)) %>% pull(rapm) %>% first()
  smoothed <- if (nrow(data) < min_points || all(data$rapm == data$rapm[1])) {
    0
  } else {
    predict(loess(rapm ~ overall_round, data = data, span = span),
            newdata = data.frame(overall_round = max(data$overall_round)))
  }
  raw_weight <- min(nrow(data) / max_history, weight_limit)
  list(smoothed_coefficient = smoothed,
       smoothed_coefficient_adj = raw_weight * smoothed + (1 - raw_weight) * latest)
}

# Hyperparameters shared with the core model.
season_decay <- 0.75
round_decay <- 0.075
constructor_span <- 0.3
driver_span <- 0.3
constructor_weight <- 0.3
driver_weight <- 0.3
max_history_value <- 40
dnf_inclusive <- FALSE
position_weighted <- TRUE

results_full <- if (dnf_inclusive) results_unfiltered else filter(results_unfiltered, finished)
results_full <- results_full %>% ungroup() %>% arrange(date, driver_id)
results_pred <- results_full

position_pred_weights <- results_full %>%
  group_by(date) %>%
  mutate(total_cars = max(position), cars_outside_points = total_cars - 10,
         position_pred_weight = if (position_weighted) {
           if_else(position <= 10, 1,
             (cars_outside_points - (position - 10) + 1) / (cars_outside_points + 1))
         } else {
           rep(1, n())
         }) %>%
  ungroup() %>% pull(position_pred_weight)

driver_list <- unique(results_full$driver_id)
constructor_list <- unique(results_full$parent_constructor_id)
driver_cons_list <- c(driver_list, constructor_list)
design_matrix <- t(apply(results_full[, c("driver_id", "parent_constructor_id")], 1,
                         make_matrix_rows_codex, players_in = driver_cons_list))
driver_x_all <- design_matrix[, seq_along(driver_list), drop = FALSE]
constructor_x_all <- design_matrix[, length(driver_list) + seq_along(constructor_list), drop = FALSE]
race_dates <- unique(results_full$date)

rapm_history <- data.frame(entity_id = character(), rapm = numeric(), rapm_loess = numeric(),
  rapm_blended = numeric(), rapm_error = numeric(), model_date = as.Date(character()),
  circuit = character(), season = numeric(), round = numeric(), overall_round = numeric(),
  dev_ratio = numeric())

for (i in seq_along(race_dates)) {
  current_date <- race_dates[i]
  current_rows <- which(results_full$date == current_date)
  end_index <- max(current_rows)
  train_rows <- seq_len(end_index)
  current_season <- as.numeric(unique(results_full$season[current_rows]))
  current_overall_round <- as.numeric(unique(results_full$round_overall[current_rows]))
  current_round <- as.numeric(unique(results_full$round[current_rows]))
  current_circuit <- unique(results_full$circuit_id[current_rows])
  observation_season <- results_full$season[train_rows]
  round_diff <- current_overall_round - results_full$round_overall[train_rows] + 1
  season_diff <- current_season - observation_season + 1
  common_weight <- decay_function_codex(round_diff, -round_decay) *
    decay_function_codex(season_diff, -season_decay) * position_pred_weights[train_rows]
  driver_fit_weight <- common_weight * regulation_weight_codex(
    observation_season, current_season, regulation_penalty, driver_regulation_multiplier)
  constructor_fit_weight <- common_weight * regulation_weight_codex(
    observation_season, current_season, regulation_penalty, constructor_regulation_multiplier)

  # Retain the core model's CV-selected ridge strength; only the allocation of
  # historical evidence differs in the alternating component updates.
  cv_fit <- cv.glmnet(design_matrix[train_rows, , drop = FALSE], results_full$position[train_rows],
                      alpha = 0, weights = common_weight, standardize = FALSE, type.measure = "mae")
  fit <- fit_separate_regulation_ridge(driver_x_all[train_rows, , drop = FALSE],
    constructor_x_all[train_rows, , drop = FALSE], results_full$position[train_rows],
    driver_fit_weight, constructor_fit_weight, cv_fit$lambda.min)
  beta <- c(fit$driver_beta, fit$constructor_beta)

  output <- data.frame(entity_id = driver_cons_list, rapm = beta) %>%
    mutate(model_date = as.Date(current_date), season = current_season,
      round = current_round, overall_round = current_overall_round, circuit = current_circuit,
      dev_ratio = NA_real_, rapm_loess = NA_real_, rapm_blended = NA_real_, rapm_error = NA_real_)
  rapm_history <- bind_rows(rapm_history, output) %>%
    group_by(entity_id) %>%
    mutate(span_value = if_else(str_ends(entity_id, "-c"), constructor_span, driver_span),
      weight_value = if_else(str_ends(entity_id, "-c"), constructor_weight, driver_weight),
      temp = list(apply_loess_smoothing_codex(cur_data(), max(span_value), max(weight_value), max_history_value)),
      rapm_loess = if_else(overall_round == max(overall_round), map_dbl(temp, "smoothed_coefficient"), rapm_loess),
      rapm_blended = if_else(overall_round == max(overall_round), map_dbl(temp, "smoothed_coefficient_adj"), rapm_blended)) %>%
    ungroup() %>% select(-span_value, -weight_value, -temp)

  if (i < length(race_dates)) {
    next_rows <- which(results_full$date == race_dates[i + 1])
    current_coeff <- filter(rapm_history, overall_round == current_overall_round)
    loess_beta <- current_coeff$rapm_loess[match(driver_cons_list, current_coeff$entity_id)]
    blended_beta <- current_coeff$rapm_blended[match(driver_cons_list, current_coeff$entity_id)]
    results_pred[next_rows, "position_pred"] <- as.numeric(design_matrix[next_rows, , drop = FALSE] %*% beta)
    results_pred[next_rows, "position_pred_loess"] <- as.numeric(design_matrix[next_rows, , drop = FALSE] %*% loess_beta)
    results_pred[next_rows, "position_pred_blended"] <- as.numeric(design_matrix[next_rows, , drop = FALSE] %*% blended_beta)
    results_pred[next_rows, "position_pred_driver"] <- as.numeric(driver_x_all[next_rows, , drop = FALSE] %*% fit$driver_beta)
    results_pred[next_rows, "position_pred_constructor"] <- as.numeric(constructor_x_all[next_rows, , drop = FALSE] %*% fit$constructor_beta)
  }
  message("Completed ", current_circuit, " ", current_season, " round ", current_round)
}

filename <- sprintf("f1dataR - Exports/Data/RAPM Outputs/rapm_history_pos%s_%s_bootstrapped_codex",
  ifelse(position_weighted, "Weighted", "Unweighted"), ifelse(dnf_inclusive, "DNF", "noDNF"))
saveRDS(rapm_history, paste0(filename, ".rds"))
saveRDS(results_pred, paste0(filename, "_results_pred.rds"))

# Same race-level ranking target as the core model, mapped to season so that
# regulation-transition seasons can be compared directly.
season_kendall_metrics <- results_pred %>%
  filter(!is.na(position_pred), season > 2013) %>%
  group_by(date, season) %>%
  summarise(kendall_tau = Kendall(position, rank(position_pred))$tau,
    kendall_tau_loess = Kendall(position, rank(position_pred_loess))$tau,
    kendall_tau_blended = Kendall(position, rank(position_pred_blended))$tau,
    .groups = "drop") %>%
  group_by(season) %>%
  summarise(races = n(), kendall_tau = mean(kendall_tau, na.rm = TRUE),
    kendall_tau_loess = mean(kendall_tau_loess, na.rm = TRUE),
    kendall_tau_blended = mean(kendall_tau_blended, na.rm = TRUE), .groups = "drop") %>%
  mutate(regulation_change = case_when(
    season %in% c(2014, 2026) ~ "major",
    season %in% c(2017, 2022) ~ "minor",
    TRUE ~ "none"))

metrics_file <- "f1dataR - Exports/Data/model_metrics_core_codex_by_season.csv"
write.csv(season_kendall_metrics, metrics_file, row.names = FALSE)
print(season_kendall_metrics)
