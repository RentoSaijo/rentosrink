## ----- Ridge xG External Test ----- ##
## Scores the saved ridge workflows on the same external seasons used by
## models/xG/compare.R. This requires access to the season play-by-play data.

suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))
suppressMessages(library(tidymodels))
suppressMessages(library(yardstick))

source(file.path("models", "xG", "prepare.R"))

EVAL_SEASONS <- c(20212022L, 20232024L, 20252026L)
RESULTS_DIR <- file.path("models", "xG", "nhlscraper", "results")
MODEL_DIR <- file.path("models", "xG", "nhlscraper")
MODEL_LABEL <- "ridge_glmnet"

load_models <- function() {
  datasets <- c("sd", "ev", "pp", "sh", "en", "so")

  purrr::map(
    datasets,
    function(dataset) {
      readRDS(file.path(MODEL_DIR, paste0(dataset, "_ridge.rds")))
    }
  ) %>%
    rlang::set_names(datasets)
}

prepare_scoring_data <- function(data) {
  logical_predictors <- setdiff(
    names(data)[vapply(data, is.logical, logical(1))],
    "isGoal"
  )

  data %>%
    dplyr::mutate(
      seasonStart = as.integer(substr(as.character(gameId), 1, 4)),
      dplyr::across(
        dplyr::all_of(logical_predictors),
        ~ factor(dplyr::if_else(.x, "yes", "no"), levels = c("no", "yes"))
      )
    )
}

score_partitions <- function(partitions, models) {
  purrr::imap_dfr(
    partitions,
    function(data, dataset) {
      model <- models[[dataset]]
      scoring_data <- prepare_scoring_data(data)
      preds <- predict(model, new_data = scoring_data, type = "prob")

      data %>%
        dplyr::select(gameId, eventId, isGoal) %>%
        dplyr::mutate(
          dataset = dataset,
          model = MODEL_LABEL,
          season = season_id_from_game_id(gameId),
          .pred_goal = preds$.pred_goal
        )
    }
  )
}

safe_roc_auc <- function(truth, estimate) {
  tryCatch(
    yardstick::roc_auc_vec(truth = truth, estimate = estimate, event_level = "second"),
    error = function(e) NA_real_
  )
}

safe_pr_auc <- function(truth, estimate) {
  tryCatch(
    yardstick::pr_auc_vec(truth = truth, estimate = estimate, event_level = "second"),
    error = function(e) NA_real_
  )
}

summarize_metrics <- function(predictions, group_cols) {
  eps <- 1e-15

  predictions %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::group_modify(
      ~ {
        truth <- factor(.x$isGoal, levels = c(FALSE, TRUE), labels = c("no_goal", "goal"))
        outcome <- as.integer(.x$isGoal)
        prob <- pmin(pmax(.x$.pred_goal, eps), 1 - eps)
        total_goals <- sum(outcome)
        total_xg <- sum(prob)

        tibble::tibble(
          rows = nrow(.x),
          goals = total_goals,
          total_xg = total_xg,
          goal_rate = mean(outcome),
          xg_rate = mean(prob),
          log_loss = mean(-(outcome * log(prob) + (1 - outcome) * log(1 - prob))),
          brier = mean((prob - outcome)^2),
          roc_auc = safe_roc_auc(truth, prob),
          pr_auc = safe_pr_auc(truth, prob),
          calibration_ratio = if (total_goals == 0L) NA_real_ else total_xg / total_goals,
          calibration_error = if (total_goals == 0L) NA_real_ else abs((total_xg / total_goals) - 1)
        )
      }
    ) %>%
    dplyr::ungroup()
}

dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

shots <- prepare_xg_shots(EVAL_SEASONS)
partitions <- build_xg_partitions(shots)
models <- load_models()
predictions <- score_partitions(partitions, models)

metrics_by_dataset <- summarize_metrics(
  predictions,
  group_cols = c("season", "dataset", "model")
)

metrics_overall <- summarize_metrics(
  predictions,
  group_cols = c("season", "model")
)

readr::write_csv(
  metrics_by_dataset,
  file.path(RESULTS_DIR, "test_by_dataset.csv")
)
readr::write_csv(
  metrics_overall,
  file.path(RESULTS_DIR, "test_overall.csv")
)
saveRDS(
  predictions,
  file.path(RESULTS_DIR, "test_predictions.rds")
)

print(metrics_by_dataset)
print(metrics_overall)
