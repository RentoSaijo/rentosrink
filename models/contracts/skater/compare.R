## ----- Setup ----- ##

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(yardstick))

tidymodels::tidymodels_prefer()

source(file.path("models", "contracts", "skater", "train.R"))

VALIDATE_PATH <- file.path("models", "contracts", "data", "skater_validate.csv")
EPS <- 1e-15

## ----- Data / Model Loading ----- ##

load_compare_models <- function() {
  list(
    term = list(
      xgboost = readRDS(file.path(MODEL_DIR, "term1.rds")),
      lightgbm = readRDS(file.path(MODEL_DIR, "term2.rds"))
    ),
    aavPerc = list(
      xgboost = readRDS(file.path(MODEL_DIR, "aavPerc1.rds")),
      lightgbm = readRDS(file.path(MODEL_DIR, "aavPerc2.rds"))
    )
  )
}

## ----- Scoring ----- ##

score_term_models <- function(partitions, models) {
  purrr::imap_dfr(
    partitions,
    function(data, split_name) {
      purrr::imap_dfr(
        models,
        function(model, engine) {
          prob_preds <- predict(model, new_data = data, type = "prob")

          data %>%
            dplyr::transmute(
              split = split_name,
              task = "term",
              model = paste0("term", dplyr::if_else(engine == "xgboost", "1", "2")),
              engine = engine,
              playerId = playerId,
              startSeasonId = startSeasonId,
              isResign = isResign,
              contractNumber = contractNumber,
              dateOfSigning = dateOfSigning,
              truth = as.character(term)
            ) %>%
            dplyr::bind_cols(prob_preds) %>%
            normalize_term_probabilities(is_resign_col = "isResign")
        }
      )
    }
  )
}

score_aav_perc_models <- function(partitions, models) {
  purrr::imap_dfr(
    partitions,
    function(data, split_name) {
      purrr::imap_dfr(
        models,
        function(model, engine) {
          preds <- predict(model, new_data = data)

          data %>%
            dplyr::transmute(
              split = split_name,
              task = "aavPerc",
              model = paste0("aavPerc", dplyr::if_else(engine == "xgboost", "1", "2")),
              engine = engine,
              playerId = playerId,
              startSeasonId = startSeasonId,
              contractNumber = contractNumber,
              dateOfSigning = dateOfSigning,
              truth = aavPerc
            ) %>%
            dplyr::bind_cols(preds)
        }
      )
    }
  )
}

## ----- Metrics ----- ##

summarize_term_metrics <- function(predictions) {
  prob_cols <- names(predictions)[
    startsWith(names(predictions), ".pred_") &
      names(predictions) != ".pred_class"
  ]
  class_levels <- sub("^\\.pred_", "", prob_cols)

  predictions %>%
    dplyr::group_by(split, task, model, engine) %>%
    dplyr::group_modify(
      ~ {
        prob_matrix <- as.matrix(.x[, prob_cols, drop = FALSE])
        truth_index <- match(.x$truth, class_levels)
        true_prob <- prob_matrix[cbind(seq_len(nrow(.x)), truth_index)]
        true_prob <- pmin(pmax(true_prob, EPS), 1 - EPS)

        tibble::tibble(
          rows = nrow(.x),
          accuracy = mean(.x$.pred_class == .x$truth),
          mnLogLoss = mean(-log(true_prob))
        )
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(split, task, model, engine)
}

summarize_aav_perc_metrics <- function(predictions) {
  predictions %>%
    dplyr::group_by(split, task, model, engine) %>%
    dplyr::group_modify(
      ~ {
        tibble::tibble(
          rows = nrow(.x),
          rmse = yardstick::rmse_vec(truth = .x$truth, estimate = .x$.pred),
          mae = yardstick::mae_vec(truth = .x$truth, estimate = .x$.pred),
          rsq = yardstick::rsq_vec(truth = .x$truth, estimate = .x$.pred)
        )
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(split, task, model, engine)
}

## ----- Run Comparison ----- ##

dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

partitions <- list(
  seen = load_contract_model_data("term", path = TRAIN_PATH, require_outcomes = TRUE),
  unseenFuture = load_contract_model_data("term", path = VALIDATE_PATH, require_outcomes = TRUE)
)
partitions_aav <- list(
  seen = load_contract_model_data("aavPerc", path = TRAIN_PATH, require_outcomes = TRUE),
  unseenFuture = load_contract_model_data("aavPerc", path = VALIDATE_PATH, require_outcomes = TRUE)
)
models <- load_compare_models()

term_predictions <- score_term_models(partitions, models$term)
aav_perc_predictions <- score_aav_perc_models(partitions_aav, models$aavPerc)

term_metrics <- summarize_term_metrics(term_predictions)
aav_perc_metrics <- summarize_aav_perc_metrics(aav_perc_predictions)

readr::write_csv(
  term_metrics,
  file.path(RESULTS_DIR, "compare_term.csv")
)
readr::write_csv(
  aav_perc_metrics,
  file.path(RESULTS_DIR, "compare_aavPerc.csv")
)
saveRDS(
  list(
    term = term_predictions,
    aavPerc = aav_perc_predictions
  ),
  file.path(RESULTS_DIR, "compare_predictions.rds")
)

print(term_metrics)
print(aav_perc_metrics)
