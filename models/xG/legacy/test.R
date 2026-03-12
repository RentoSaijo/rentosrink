## ----- Legacy Ridge xG Test ----- ##

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(yardstick))

source(file.path("models", "xG", "legacy", "clean.R"))

LEGACY_TEST_SEASON_MAP <- list(
  `1` = c(20122013L, 20142015L, 20162017L),
  `2` = c(20182019L, 20202021L, 20222023L)
)
RESULTS_DIR <- file.path("models", "xG", "legacy", "results")
MODEL_DIR <- file.path("models", "xG", "legacy")
MODEL_LABEL <- "ridge_glmnet"

load_models <- function(version) {
  datasets <- c("sd", "ev", "pp", "sh", "en", "so")

  purrr::map(
    datasets,
    function(dataset) {
      readRDS(file.path(MODEL_DIR, paste0(dataset, version, ".rds")))
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
      season = season_id_from_game_id(gameId),
      dplyr::across(
        dplyr::all_of(logical_predictors),
        ~ factor(dplyr::if_else(.x, "yes", "no"), levels = c("no", "yes"))
      )
    )
}

score_partitions <- function(partitions, models, version) {
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
          version = as.integer(version),
          model = paste0(dataset, version),
          engine = MODEL_LABEL,
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

predictions <- purrr::map_dfr(
  names(LEGACY_TEST_SEASON_MAP),
  function(version) {
    shots <- prepare_legacy_xg_shots(LEGACY_TEST_SEASON_MAP[[version]])
    partitions <- build_xg_partitions(shots)
    models <- load_models(version)
    score_partitions(partitions, models, version)
  }
)

metrics_by_dataset <- summarize_metrics(
  predictions,
  group_cols = c("version", "season", "dataset", "model", "engine")
)

metrics_overall <- summarize_metrics(
  predictions,
  group_cols = c("version", "season", "engine")
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
