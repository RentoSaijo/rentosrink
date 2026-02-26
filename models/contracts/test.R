# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))

# Define skater model versions to use.
TERM_VERSION <- 1L
AAVP_VERSION <- 1L
FREE_AGENT_DIR <- 'data'

# ----- Helpers ----- #

load_model <- function(path) {
  if (!file.exists(path)) {
    stop(paste0('Model file not found: ', path))
  }
  readRDS(path)
}

resolve_test_path <- function() {
  'models/contracts/data/skater_contracts_test.csv'
}

pull_pterm <- function(prob_tbl, term_values) {
  prob_mat <- as.matrix(prob_tbl)
  prob_cols <- colnames(prob_tbl)
  prob_term_values <- suppressWarnings(as.integer(sub('^\\.pred_X?', '', prob_cols)))
  col_idx <- match(term_values, prob_term_values)
  out <- rep(NA_real_, nrow(prob_mat))
  valid <- !is.na(col_idx)
  out[valid] <- prob_mat[cbind(which(valid), col_idx[valid])]
  out
}

build_free_agent_wide <- function(scored_df, season_id) {
  base_df <- scored_df %>%
    dplyr::filter(startSeasonId == season_id) %>%
    dplyr::mutate(
      age = ageAtSigning,
      term = suppressWarnings(as.integer(as.numeric(term))),
      scenario = dplyr::if_else(isResign, 'resign', 'nosign'),
      xAAV = xAAVP * cap
    ) %>%
    dplyr::select(playerId, cap, age, term, scenario, pTerm, xAAV)

  wide_df <- base_df %>%
    tidyr::pivot_wider(
      id_cols = c(playerId, cap, age),
      names_from = c(term, scenario),
      values_from = c(pTerm, xAAV),
      names_glue = '{.value}_{term}_{scenario}'
    )

  resign_cols <- unlist(lapply(1:8, function(k) c(paste0('pTerm_', k, '_resign'), paste0('xAAV_', k, '_resign'))))
  nosign_cols <- unlist(lapply(1:7, function(k) c(paste0('pTerm_', k, '_nosign'), paste0('xAAV_', k, '_nosign'))))
  target_cols <- c('playerId', 'cap', 'age', resign_cols, nosign_cols)
  missing_cols <- setdiff(target_cols, names(wide_df))
  if (length(missing_cols) > 0L) {
    for (nm in missing_cols) {
      wide_df[[nm]] <- NA_real_
    }
  }

  wide_df %>%
    dplyr::select(dplyr::all_of(target_cols)) %>%
    dplyr::arrange(playerId)
}

# ----- Test ----- #

term_model_path <- paste0('models/contracts/skater/term', TERM_VERSION, '.rds')
aavp_model_path <- paste0('models/contracts/skater/aavP', AAVP_VERSION, '.rds')
test_path <- resolve_test_path()

# Load models and data.
term_model <- load_model(term_model_path)
aavp_model <- load_model(aavp_model_path)
skater_contracts <- readr::read_csv(test_path, show_col_types = FALSE)

# Predict term probabilities and AAV%.
term_prob <- predict(term_model, new_data = skater_contracts, type = 'prob')
term_values <- suppressWarnings(as.integer(as.numeric(skater_contracts[['term']])))
p_term <- pull_pterm(term_prob, term_values)
x_aavp <- predict(aavp_model, new_data = skater_contracts) %>%
  dplyr::pull(.pred)

# Write predictions.
out <- skater_contracts %>%
  dplyr::mutate(
    pTerm = p_term,
    xAAVP = x_aavp
  )

# Write tidy free-agent outputs by start season.
start_seasons <- sort(unique(out[['startSeasonId']]))
for (season_id in start_seasons) {
  free_agents_wide <- build_free_agent_wide(out, season_id)
  out_path <- file.path(FREE_AGENT_DIR, paste0('skater_free_agents_', season_id, '.csv'))
  readr::write_csv(free_agents_wide, out_path)
}
