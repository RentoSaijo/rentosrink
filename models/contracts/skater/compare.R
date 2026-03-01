# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))

# Define constants.
TERM_VERSIONS <- 1:3
AAVP_VERSIONS <- 1:3
VALIDATE_PATH <- 'models/contracts/data/skater_contracts_validate.csv'

# ----- Helpers ----- #

load_model <- function(path) {
  if (!file.exists(path)) {
    stop(paste0('Model file not found: ', path))
  }
  readRDS(path)
}

parse_prob_term_values <- function(prob_tbl) {
  prob_cols <- colnames(prob_tbl)
  suppressWarnings(as.integer(stringr::str_extract(prob_cols, '[0-9]+$')))
}

predict_term_argmax <- function(prob_tbl) {
  prob_mat <- as.matrix(prob_tbl)
  prob_term_values <- parse_prob_term_values(prob_tbl)
  max_idx <- max.col(prob_mat, ties.method = 'first')
  prob_term_values[max_idx]
}

pull_pterm <- function(prob_tbl, term_values) {
  prob_mat <- as.matrix(prob_tbl)
  prob_term_values <- parse_prob_term_values(prob_tbl)
  col_idx <- match(term_values, prob_term_values)
  out <- rep(NA_real_, nrow(prob_mat))
  valid <- !is.na(col_idx)
  out[valid] <- prob_mat[cbind(which(valid), col_idx[valid])]
  out
}

safe_prob <- function(x, eps = 1e-15) {
  pmax(x, eps)
}

# ----- Load Validate Data ----- #

validate_eval <- readr::read_csv(VALIDATE_PATH, show_col_types = FALSE) %>%
  dplyr::filter(!is.na(term), !is.na(aavP), !is.na(isResign)) %>%
  dplyr::mutate(term = suppressWarnings(as.integer(as.numeric(term))))

# ----- Compare Term Models ----- #

term_results <- purrr::map_dfr(TERM_VERSIONS, function(version) {
  model_path <- paste0('models/contracts/skater/term', version, '.rds')
  model <- load_model(model_path)

  prob_tbl <- predict(model, new_data = validate_eval, type = 'prob')
  pred_term <- predict_term_argmax(prob_tbl)
  p_true <- pull_pterm(prob_tbl, validate_eval$term)

  tibble::tibble(
    version = version,
    model_file = basename(model_path),
    accuracy = mean(pred_term == validate_eval$term, na.rm = TRUE),
    mn_log_loss = -mean(log(safe_prob(p_true)), na.rm = TRUE)
  )
}) %>%
  dplyr::arrange(mn_log_loss, dplyr::desc(accuracy), version)

best_term <- term_results %>%
  dplyr::slice_head(n = 1)

# ----- Compare AAV% Models ----- #

aavp_results <- purrr::map_dfr(AAVP_VERSIONS, function(version) {
  model_path <- paste0('models/contracts/skater/aavP', version, '.rds')
  model <- load_model(model_path)

  pred <- predict(model, new_data = validate_eval) %>%
    dplyr::pull(.pred)
  err <- pred - validate_eval$aavP

  tibble::tibble(
    version = version,
    model_file = basename(model_path),
    mse = mean(err ^ 2, na.rm = TRUE),
    rmse = sqrt(mean(err ^ 2, na.rm = TRUE)),
    mae = mean(abs(err), na.rm = TRUE)
  )
}) %>%
  dplyr::arrange(mse, rmse, mae, version)

best_aavp <- aavp_results %>%
  dplyr::slice_head(n = 1)

# ----- Report ----- #

cat('\nTerm model comparison (sorted best first):\n')
print(term_results)
cat('\nBest term model:\n')
print(best_term)

cat('\nAAVP model comparison (sorted best first):\n')
print(aavp_results)
cat('\nBest aavP model:\n')
print(best_aavp)
