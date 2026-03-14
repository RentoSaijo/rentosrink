## ----- Contract Test Scoring ----- ##

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(nhlscraper))

tidymodels::tidymodels_prefer()

source(file.path("models", "contracts", "skater", "train.R"))

APP_FREE_AGENTS_PATH <- file.path("data", "skater_free_agents_20262027.csv")
APP_CONTRACTS_PATH <- file.path("data", "skater_contracts.csv")

CAPS <- tibble::tibble(
  seasonId = c(
    20052006L, 20062007L, 20072008L, 20082009L, 20092010L,
    20102011L, 20112012L, 20122013L, 20132014L, 20142015L,
    20152016L, 20162017L, 20172018L, 20182019L, 20192020L,
    20202021L, 20212022L, 20222023L, 20232024L, 20242025L,
    20252026L
  ),
  cap = c(
    39000000, 44000000, 50300000, 56700000, 56800000,
    59400000, 64300000, 60000000, 64300000, 69000000,
    71400000, 73000000, 75000000, 79500000, 81500000,
    81500000, 81500000, 82500000, 83500000, 88000000,
    95500000
  )
)

## ----- Model Loading ----- ##

load_test_models <- function() {
  list(
    term = readRDS(file.path(MODEL_DIR, "term2.rds")),
    aavPerc = readRDS(file.path(MODEL_DIR, "aavPerc1.rds"))
  )
}

## ----- Scoring ----- ##

build_term_scenarios <- function(test_data) {
  test_data %>%
    dplyr::group_by(playerId, isResign) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()
}

score_term_scenarios <- function(test_scenarios, model) {
  prob_preds <- predict(model, new_data = test_scenarios, type = "prob")

  test_scenarios %>%
    dplyr::transmute(
      playerId,
      isResign,
      age = ageAtSigning,
      dateOfSigning,
      contractNumber,
      cap,
      startSeasonId,
      termPrev,
      aavPrev,
      aavPercPrev
    ) %>%
    dplyr::bind_cols(prob_preds) %>%
    normalize_term_probabilities(is_resign_col = "isResign") %>%
    dplyr::mutate(
      topTerm = as.integer(.pred_class)
    )
}

score_aav_perc_options <- function(test_data, test_reference, model) {
  preds <- predict(model, new_data = test_data)

  test_reference %>%
    dplyr::transmute(
      playerId,
      isResign,
      age = ageAtSigning,
      term = as.integer(as.character(term)),
      dateOfSigning,
      contractNumber,
      cap,
      startSeasonId,
      termPrev,
      aavPrev,
      aavPercPrev
    ) %>%
    dplyr::bind_cols(preds) %>%
    dplyr::transmute(
      playerId,
      isResign,
      age,
      term,
      dateOfSigning,
      contractNumber,
      cap,
      startSeasonId,
      termPrev,
      aavPrev,
      aavPercPrev,
      aavPerc = .pred,
      aav = cap * .pred
    )
}

term_probabilities_long <- function(term_scenario_predictions) {
  term_scenario_predictions %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("^\\.pred_[0-9]+$"),
      names_to = "term",
      values_to = "termProb"
    ) %>%
    dplyr::mutate(
      term = as.integer(sub("^\\.pred_", "", term))
    ) %>%
    dplyr::select(
      playerId,
      isResign,
      age,
      term,
      dateOfSigning,
      contractNumber,
      cap,
      startSeasonId,
      termPrev,
      aavPrev,
      aavPercPrev,
      topTerm,
      termProb
    )
}

build_contract_options <- function(term_probabilities, aav_predictions) {
  join_keys <- c(
    "playerId",
    "isResign",
    "age",
    "term",
    "dateOfSigning",
    "contractNumber",
    "cap",
    "startSeasonId",
    "termPrev",
    "aavPrev",
    "aavPercPrev"
  )

  term_probabilities %>%
    dplyr::left_join(
      aav_predictions,
      by = join_keys,
      relationship = "one-to-one"
    ) %>%
    dplyr::arrange(dplyr::desc(termProb), playerId, dplyr::desc(isResign), term)
}

## ----- App Exports ----- ##

build_free_agent_export <- function(contract_options) {
  suffix_map <- c(yes = "resign", no = "nosign")

  probability_wide <- contract_options %>%
    dplyr::mutate(
      suffix = unname(suffix_map[as.character(isResign)])
    ) %>%
    dplyr::transmute(
      playerId,
      cap,
      age,
      column = sprintf("pTerm_%d_%s", term, suffix),
      value = termProb
    ) %>%
    tidyr::pivot_wider(
      names_from = column,
      values_from = value
    )

  aav_wide <- contract_options %>%
    dplyr::mutate(
      suffix = unname(suffix_map[as.character(isResign)])
    ) %>%
    dplyr::transmute(
      playerId,
      cap,
      age,
      column = sprintf("xAAV_%d_%s", term, suffix),
      value = aav
    ) %>%
    tidyr::pivot_wider(
      names_from = column,
      values_from = value
    )

  ordered_cols <- c(
    "playerId",
    "cap",
    "age",
    unlist(purrr::map(1:8, ~ c(sprintf("pTerm_%d_resign", .x), sprintf("xAAV_%d_resign", .x)))),
    unlist(purrr::map(1:7, ~ c(sprintf("pTerm_%d_nosign", .x), sprintf("xAAV_%d_nosign", .x))))
  )

  probability_wide %>%
    dplyr::left_join(aav_wide, by = c("playerId", "cap", "age"), relationship = "one-to-one") %>%
    dplyr::select(dplyr::any_of(ordered_cols)) %>%
    dplyr::arrange(playerId)
}

build_contract_history_export <- function(player_ids) {
  nhlscraper::contracts() %>%
    dplyr::filter(
      playerId %in% player_ids,
      positionCode != "G"
    ) %>%
    dplyr::arrange(playerId, startSeasonId, endSeasonId, signedWithTeamId, aav) %>%
    dplyr::group_by(playerId) %>%
    dplyr::mutate(
      number = dplyr::row_number(),
      first = number == 1L,
      last = number == dplyr::n(),
      resign = dplyr::if_else(
        number == 1L,
        FALSE,
        dplyr::coalesce(signedWithTeamId == dplyr::lag(signedWithTeamId), FALSE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      CAPS %>% dplyr::rename(startSeasonId = seasonId),
      by = "startSeasonId",
      relationship = "many-to-one"
    ) %>%
    dplyr::transmute(
      playerId = as.integer(playerId),
      number = as.integer(number),
      teamId = as.integer(signedWithTeamId),
      seasonId_start = as.integer(startSeasonId),
      seasonId_end = as.integer(endSeasonId),
      first,
      last,
      resign,
      cap,
      age = ageAtSigning,
      term = as.integer(term),
      aav
    ) %>%
    dplyr::arrange(playerId, number)
}

## ----- Run Test Scoring ----- ##

dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

term_test_data <- load_contract_model_data("term", path = TEST_PATH, require_outcomes = FALSE)
aav_test_data <- load_contract_model_data("aavPerc", path = TEST_PATH, require_outcomes = FALSE)
term_scenarios <- build_term_scenarios(term_test_data)
models <- load_test_models()

term_scenario_predictions <- score_term_scenarios(term_scenarios, models$term)
term_probabilities <- term_probabilities_long(term_scenario_predictions)
aav_predictions <- score_aav_perc_options(aav_test_data, term_test_data, models$aavPerc)
contract_options <- build_contract_options(term_probabilities, aav_predictions)

free_agents_export <- build_free_agent_export(contract_options)
contract_history_export <- build_contract_history_export(unique(free_agents_export$playerId))

readr::write_csv(
  term_probabilities,
  file.path(RESULTS_DIR, "test_term_probabilities.csv")
)
readr::write_csv(
  aav_predictions,
  file.path(RESULTS_DIR, "test_aavPerc_options.csv")
)
readr::write_csv(
  contract_options,
  file.path(RESULTS_DIR, "test_contract_options.csv")
)
readr::write_csv(
  free_agents_export,
  APP_FREE_AGENTS_PATH
)
readr::write_csv(
  contract_history_export,
  APP_CONTRACTS_PATH
)
saveRDS(
  list(
    termScenarios = term_scenario_predictions,
    termProbabilities = term_probabilities,
    aavPercOptions = aav_predictions,
    contractOptions = contract_options,
    freeAgents = free_agents_export,
    contractHistory = contract_history_export
  ),
  file.path(RESULTS_DIR, "test_predictions.rds")
)

print(
  free_agents_export %>%
    dplyr::slice_head(n = 20)
)
