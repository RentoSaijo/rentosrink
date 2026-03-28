## ----- Setup ----- ##

suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))
suppressMessages(library(tidymodels))
suppressMessages(library(yardstick))

source(file.path("models", "xG", "prepare.R"))

EVAL_SEASONS <- c(20112012, 20252026L)
RESULTS_DIR <- file.path("models", "xG", "results")
MODEL_DIR <- file.path("models", "xG")
V3_SELECTION_SEASON <- 20252026L

## ----- Model Comparison ----- ##

load_models <- function() {
  datasets <- c("sd", "ev", "pp", "sh", "en", "ps")

  purrr::map(
    datasets,
    function(dataset) {
      list(
        dataset = dataset,
        xgboost = readRDS(file.path(MODEL_DIR, paste0(dataset, "1.rds"))),
        lightgbm = readRDS(file.path(MODEL_DIR, paste0(dataset, "2.rds")))
      )
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
      model_pair <- models[[dataset]]
      scoring_data <- prepare_scoring_data(data)

      purrr::imap_dfr(
        list(
          xgboost = model_pair$xgboost,
          lightgbm = model_pair$lightgbm
        ),
        function(model, engine) {
          preds <- predict(model, new_data = scoring_data, type = "prob")

          data %>%
            dplyr::select(gameId, eventId, isGoal) %>%
            dplyr::mutate(
              dataset = dataset,
              engine = engine,
              season = season_id_from_game_id(gameId),
              .pred_goal = preds$.pred_goal
            )
        }
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

select_v3_models <- function(metrics_by_dataset, selection_season = V3_SELECTION_SEASON) {
  metrics_by_dataset %>%
    dplyr::filter(season == selection_season) %>%
    dplyr::arrange(dataset, log_loss, brier, dplyr::desc(roc_auc), engine) %>%
    dplyr::group_by(dataset) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      dataset,
      engine,
      model = paste0(dataset, dplyr::if_else(engine == "xgboost", "1", "2")),
      selection = "v3",
      selection_season = selection_season,
      selection_metric = "log_loss"
    )
}

select_preferred_predictions <- function(predictions, preferred_tbl) {
  preferred_tbl <- preferred_tbl %>%
    dplyr::select(dataset, engine, model, selection, selection_season, selection_metric)

  predictions %>%
    dplyr::inner_join(preferred_tbl, by = c("dataset", "engine"))
}

print_v3_selection <- function(preferred_tbl) {
  preferred_tbl %>%
    dplyr::arrange(dataset) %>%
    print()
}

write_v3_selection <- function(preferred_tbl) {
  readr::write_csv(
    preferred_tbl,
    file.path(RESULTS_DIR, "compare_v3_selected_models.csv")
  )

  readr::write_csv(
    preferred_tbl %>%
      dplyr::select(dataset, engine),
    file.path(RESULTS_DIR, "compare_v3_selected_engines.csv")
  )

  invisible(preferred_tbl)
}

select_and_save_v3_models <- function(metrics_by_dataset) {
  preferred_tbl <- select_v3_models(metrics_by_dataset)

  write_v3_selection(preferred_tbl)
  print_v3_selection(preferred_tbl)

  preferred_tbl
}

## ----- Run Comparison ----- ##

dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

shots <- prepare_xg_shots(EVAL_SEASONS)
partitions <- build_xg_partitions(shots)
models <- load_models()
predictions <- score_partitions(partitions, models)

metrics_by_dataset <- summarize_metrics(
  predictions,
  group_cols = c("season", "dataset", "engine")
)

metrics_overall <- summarize_metrics(
  predictions,
  group_cols = c("season", "engine")
)

readr::write_csv(
  metrics_by_dataset,
  file.path(RESULTS_DIR, "compare_by_dataset.csv")
)
readr::write_csv(
  metrics_overall,
  file.path(RESULTS_DIR, "compare_overall.csv")
)
saveRDS(
  predictions,
  file.path(RESULTS_DIR, "compare_predictions.rds")
)

print(metrics_by_dataset)
print(metrics_overall)

## ----- V3 Selected Models ----- ##

preferred_models <- select_and_save_v3_models(metrics_by_dataset)

selected_predictions <- select_preferred_predictions(predictions, preferred_models)

metrics_v3_by_dataset <- summarize_metrics(
  selected_predictions,
  group_cols = c("season", "dataset", "model", "engine", "selection")
)

metrics_v3_overall <- summarize_metrics(
  selected_predictions,
  group_cols = c("season", "selection")
)

readr::write_csv(
  metrics_v3_by_dataset,
  file.path(RESULTS_DIR, "compare_v3_by_dataset.csv")
)
readr::write_csv(
  metrics_v3_overall,
  file.path(RESULTS_DIR, "compare_v3_overall.csv")
)
saveRDS(
  selected_predictions,
  file.path(RESULTS_DIR, "compare_v3_predictions.rds")
)

print(metrics_v3_by_dataset)
print(metrics_v3_overall)
