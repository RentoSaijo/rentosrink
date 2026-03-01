# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))

# Define skater model versions to use.
TERM_VERSION <- 3L
AAVP_VERSION <- 2L
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

parse_prob_term_values <- function(prob_tbl) {
  prob_cols <- colnames(prob_tbl)
  suppressWarnings(as.integer(stringr::str_extract(prob_cols, '[0-9]+$')))
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

# ----- Load Models And Data ----- #

term_model_path <- paste0('models/contracts/skater/term', TERM_VERSION, '.rds')
aavp_model_path <- paste0('models/contracts/skater/aavP', AAVP_VERSION, '.rds')
test_path <- resolve_test_path()

term_model <- load_model(term_model_path)
aavp_model <- load_model(aavp_model_path)
skater_contracts_test <- readr::read_csv(test_path, show_col_types = FALSE)

# ----- Score Test Scenarios (Free Agents) ----- #

term_prob_test <- predict(term_model, new_data = skater_contracts_test, type = 'prob')
term_values_test <- suppressWarnings(as.integer(as.numeric(skater_contracts_test[['term']])))
p_term_test <- pull_pterm(term_prob_test, term_values_test)
x_aavp_test <- predict(aavp_model, new_data = skater_contracts_test) %>%
  dplyr::pull(.pred)

scored_test <- skater_contracts_test %>%
  dplyr::mutate(
    pTerm = p_term_test,
    xAAVP = x_aavp_test
  )

start_seasons <- sort(unique(scored_test[['startSeasonId']]))
for (season_id in start_seasons) {
  free_agents_wide <- build_free_agent_wide(scored_test, season_id)
  out_path <- file.path(FREE_AGENT_DIR, paste0('skater_free_agents_', season_id, '.csv'))
  readr::write_csv(free_agents_wide, out_path)
}
