# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Set constant.
END_SEASON_ID <- 20252026
VALIDATE_SEASON_ID <- END_SEASON_ID + 10001L

# Flag first and last contracts, note previous contract's term and AAV, and count which'th contract.
contracts <- nhlscraper::contracts() %>%
  dplyr::select(-playerFullName, -positionCode, -signedWithTeamTriCode, -value, -bonus, -twoYearCash, -threeYearCash) %>% 
  dplyr::group_by(playerId) %>%
  dplyr::arrange(startSeasonId, .by_group = TRUE) %>%
  dplyr::mutate(
    contractNumber = dplyr::row_number(),
    isFirst = startSeasonId == min(startSeasonId, na.rm = TRUE),
    isLast = startSeasonId == max(startSeasonId, na.rm = TRUE),
    prevStartSeasonId = lag(startSeasonId),
    prevTerm = lag(term),
    prevAAV = lag(aav)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(ageAtSigning) & !is.na(aav))

# Create data.frame of salary caps.
caps <- tibble::tibble(
  season = c(
    '20132014',
    '20142015',
    '20152016',
    '20162017',
    '20172018',
    '20182019',
    '20192020',
    '20202021',
    '20212022',
    '20222023',
    '20232024',
    '20242025',
    '20252026',
    '20262027',
    '20272028'
  ),
  cap_millions = c(
    64.3,
    69.0,
    71.4,
    73.0,
    75.0,
    79.5,
    81.5,
    81.5,
    81.5,
    82.5,
    83.5,
    88.0,
    95.5,
    104.0,
    113.5
  )
) %>%
  dplyr::mutate(cap = cap_millions * 1e6) %>%
  as.data.frame() %>%
  dplyr::select(season, cap)

# Load supplemental data.
bios <- dplyr::filter(nhlscraper::players(), playerId %in% contracts$playerId)
sss <- nhlscraper::skater_season_stats() %>%
  dplyr::filter(playerId %in% contracts$playerId & gameTypeId %in% c(2L, 3L)) %>%
  dplyr::mutate(sssRow = dplyr::row_number())
ssa <- readr::read_csv('models/contracts/data/skater_shot_analysis.csv', show_col_types = FALSE) %>%
  dplyr::select(playerId, seasonId, dplyr::contains('_2_') & !dplyr::contains('all'))

# ----- Helpers ----- #

# Convert single season stat to per-60.
calc_single_p60 <- function(stat_values, minutes_values) {
  dplyr::if_else(
    !is.na(minutes_values) & minutes_values > 0,
    60 * tidyr::replace_na(stat_values, 0) / minutes_values,
    0
  )
}

# Compute weighted average of two values, handling missing inputs.
calc_weighted_average <- function(
  recent_value,
  prior_value,
  recent_valid,
  prior_valid,
  recent_weight = 2,
  prior_weight = 1
) {
  weight_sum <- dplyr::if_else(recent_valid, recent_weight, 0) +
    dplyr::if_else(prior_valid, prior_weight, 0)
  weighted_sum <- dplyr::if_else(recent_valid, recent_weight * recent_value, 0) +
    dplyr::if_else(prior_valid, prior_weight * prior_value, 0)
  dplyr::if_else(weight_sum > 0, weighted_sum / weight_sum, 0)
}

# Convert two seasons of stats to a 2:1 weighted per-60 average.
calc_avg_p60 <- function(stat_recent, minutes_recent, stat_prior, minutes_prior) {
  valid_recent <- !is.na(minutes_recent) & minutes_recent > 0
  valid_prior  <- !is.na(minutes_prior) & minutes_prior > 0
  p60_recent <- calc_single_p60(stat_recent, minutes_recent)
  p60_prior <- calc_single_p60(stat_prior, minutes_prior)
  calc_weighted_average(
    recent_value = p60_recent,
    prior_value = p60_prior,
    recent_valid = valid_recent,
    prior_valid = valid_prior
  )
}

# Average minutes across two prior seasons with 2:1 weighting.
calc_avg_minutes <- function(minutes_recent, minutes_prior) {
  valid_recent <- !is.na(minutes_recent) & minutes_recent > 0
  valid_prior  <- !is.na(minutes_prior) & minutes_prior > 0
  calc_weighted_average(
    recent_value = tidyr::replace_na(minutes_recent, 0),
    prior_value = tidyr::replace_na(minutes_prior, 0),
    recent_valid = valid_recent,
    prior_valid = valid_prior
  )
}

# Capitalize first letter of a string.
cap_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# ----- Create Training Set ----- #

# Remove ELCs and convert raw AAV to percentages.
contracts_train <- contracts %>% 
  dplyr::filter(!isFirst & !is.na(prevAAV) & prevStartSeasonId >= 20132014) %>% 
  dplyr::left_join(
    caps %>% dplyr::transmute(startSeasonId = as.integer(season), capStart = cap),
    by = 'startSeasonId'
  ) %>%
  dplyr::left_join(
    caps %>% dplyr::transmute(prevStartSeasonId = as.integer(season), capPrev = cap),
    by = 'prevStartSeasonId'
  ) %>%
  dplyr::mutate(
    aavP     = aav / capStart,
    prevAAVP = prevAAV / capPrev
  ) %>%
  dplyr::select(-prevStartSeasonId, -prevAAV, -aav, -capPrev)

# Add biographies.
contracts_train <- contracts_train %>%
  dplyr::left_join(
    bios %>% dplyr::select(playerId, positionCode, birthDate, height, weight, handCode)
    , by = 'playerId'
  ) %>% 
  dplyr::filter(positionCode != 'G') %>% 
  dplyr::select(
    # IDs
    playerId,
    signedWithTeamId,
    startSeasonId,
    endSeasonId,
    isFirst,
    isLast,
    birthDate,
    cap = capStart,
    # Predictors
    ageAtSigning,
    contractNumber,
    prevTerm,
    prevAAVP,
    positionCode,
    height,
    weight,
    handCode,
    # Responses
    term,
    aavP
    # More predictors to be added below.
  )

# Add re-signing flag.
sss_last_team <- sss %>%
  dplyr::filter(!is.na(teamId)) %>%
  dplyr::arrange(sssRow) %>%
  dplyr::group_by(playerId, seasonId, gameTypeId) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(playerId, seasonId, gameTypeId, lastPlayedTeamId = teamId)
last_team_before_contract <- contracts_train %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::select(contractRowId, playerId, startSeasonId) %>%
  dplyr::left_join(sss_last_team, by = 'playerId', relationship = 'many-to-many') %>%
  dplyr::filter(seasonId < startSeasonId) %>%
  dplyr::mutate(gameTypePriority = dplyr::if_else(gameTypeId == 3L, 1L, 2L)) %>%
  dplyr::arrange(contractRowId, dplyr::desc(seasonId), gameTypePriority) %>%
  dplyr::group_by(contractRowId) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(contractRowId, lastPlayedTeamId)
contracts_train <- contracts_train %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::left_join(last_team_before_contract, by = 'contractRowId', relationship = 'one-to-one') %>%
  dplyr::mutate(isResign = signedWithTeamId == lastPlayedTeamId) %>%
  dplyr::select(-contractRowId, -lastPlayedTeamId) %>% 
  dplyr::filter(!is.na(isResign))

# Build clean contract-level output for project scope data directory.
contracts_clean <- contracts %>%
  dplyr::left_join(
    caps %>% dplyr::transmute(startSeasonId = as.integer(season), cap = cap),
    by = 'startSeasonId',
    relationship = 'many-to-one'
  ) %>%
  dplyr::left_join(
    bios %>% dplyr::select(playerId, positionCode),
    by = 'playerId',
    relationship = 'many-to-one'
  ) %>%
  dplyr::filter(positionCode != 'G' & !is.na(term) & !is.na(aav))
last_team_before_contract_clean <- contracts_clean %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::select(contractRowId, playerId, startSeasonId) %>%
  dplyr::left_join(sss_last_team, by = 'playerId', relationship = 'many-to-many') %>%
  dplyr::filter(seasonId < startSeasonId) %>%
  dplyr::mutate(gameTypePriority = dplyr::if_else(gameTypeId == 3L, 1L, 2L)) %>%
  dplyr::arrange(contractRowId, dplyr::desc(seasonId), gameTypePriority) %>%
  dplyr::group_by(contractRowId) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(contractRowId, lastPlayedTeamId)
contracts_clean <- contracts_clean %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::left_join(last_team_before_contract_clean, by = 'contractRowId', relationship = 'one-to-one') %>%
  dplyr::mutate(
    isResign = dplyr::if_else(
      isFirst,
      FALSE,
      signedWithTeamId == lastPlayedTeamId,
      missing = FALSE
    )
  ) %>%
  dplyr::transmute(
    playerId,
    number = contractNumber,
    teamId = signedWithTeamId,
    seasonId_start = startSeasonId,
    seasonId_end = endSeasonId,
    first = isFirst,
    last = isLast,
    resign = dplyr::coalesce(isResign, FALSE),
    cap,
    age = ageAtSigning,
    term,
    aav
  )

# Add basic and advanced statistics.
situations <- c('ev', 'pp', 'sh')
basic_stat_cols <- list(
  assists = c(ev = 'evenStrengthAssists', pp = 'powerplayAssists', sh = 'shorthandedAssists'),
  blocks = c(ev = 'evenStrengthBlockedShots', pp = 'powerplayBlockedShots', sh = 'shorthandedBlockedShots'),
  giveaways = c(ev = 'evenStrengthGiveaways', pp = 'powerplayGiveaways', sh = 'shorthandedGiveaways'),
  hits = c(ev = 'evenStrengthHits', pp = 'powerplayHits', sh = 'shorthandedHits'),
  takeaways = c(ev = 'evenStrengthTakeaways', pp = 'powerplayTakeaways', sh = 'shorthandedTakeaways')
)
advanced_stats <- c(
  'x', 'y', 'iCorsiF', 'iFenwickF', 'iSOGF', 'iGF', 'ixGF',
  'oCorsiF', 'oFenwickF', 'oSOGF', 'oGF', 'oxGF',
  'oCorsiA', 'oFenwickA', 'oSOGA', 'oGA', 'oxGA'
)
contract_season_rows <- contracts_train %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::select(contractRowId, playerId, startSeasonId) %>%
  tidyr::expand_grid(lookback = c(2L, 3L)) %>%
  dplyr::mutate(seasonId = startSeasonId - (lookback * 10001L))
basic_source_cols <- unique(unname(unlist(basic_stat_cols)))
minutes_cols <- paste0('mP_2_', situations)
advanced_source_cols <- as.vector(outer(advanced_stats, situations, paste, sep = '_2_'))
value_cols <- c(basic_source_cols, minutes_cols, advanced_source_cols)
sss_lookup <- sss %>%
  dplyr::filter(gameTypeId == 2L) %>%
  dplyr::select(playerId, seasonId, dplyr::all_of(basic_source_cols)) %>%
  dplyr::group_by(playerId, seasonId) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(basic_source_cols), ~ sum(tidyr::replace_na(.x, 0), na.rm = TRUE)),
    .groups = 'drop'
  )
ssa_lookup <- ssa %>%
  dplyr::select(playerId, seasonId, dplyr::all_of(c(minutes_cols, advanced_source_cols))) %>%
  dplyr::group_by(playerId, seasonId) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(c(minutes_cols, advanced_source_cols)), ~ sum(tidyr::replace_na(.x, 0), na.rm = TRUE)),
    .groups = 'drop'
  )
contract_stats_long <- contract_season_rows %>%
  dplyr::left_join(sss_lookup, by = c('playerId', 'seasonId'), relationship = 'many-to-one') %>%
  dplyr::left_join(ssa_lookup, by = c('playerId', 'seasonId'), relationship = 'many-to-one')

recent_stats <- contract_stats_long %>%
  dplyr::filter(lookback == 2L) %>%
  dplyr::select(contractRowId, dplyr::all_of(value_cols)) %>%
  dplyr::rename_with(~ paste0(.x, '_recent'), -contractRowId)
prior_stats <- contract_stats_long %>%
  dplyr::filter(lookback == 3L) %>%
  dplyr::select(contractRowId, dplyr::all_of(value_cols)) %>%
  dplyr::rename_with(~ paste0(.x, '_prior'), -contractRowId)
contract_stats_wide <- recent_stats %>%
  dplyr::left_join(prior_stats, by = 'contractRowId', relationship = 'one-to-one')
feature_exprs <- list()
for (stat in names(basic_stat_cols)) {
  for (situation in situations) {
    feature_name <- paste0(stat, '_2_', situation, '_p60_avg')
    delta_name <- paste0('d', cap_first(stat), '_2_', situation, '_p60_avg')
    stat_col <- basic_stat_cols[[stat]][[situation]]
    mp_col <- paste0('mP_2_', situation)
    stat_col_recent <- paste0(stat_col, '_recent')
    stat_col_prior <- paste0(stat_col, '_prior')
    mp_col_recent <- paste0(mp_col, '_recent')
    mp_col_prior <- paste0(mp_col, '_prior')
    feature_exprs[[feature_name]] <- rlang::expr(
      calc_avg_p60(
        .data[[!!stat_col_recent]],
        .data[[!!mp_col_recent]],
        .data[[!!stat_col_prior]],
        .data[[!!mp_col_prior]]
      )
    )
    feature_exprs[[delta_name]] <- rlang::expr(
      calc_single_p60(.data[[!!stat_col_recent]], .data[[!!mp_col_recent]]) -
        calc_single_p60(.data[[!!stat_col_prior]], .data[[!!mp_col_prior]])
    )
  }
}
for (stat in advanced_stats) {
  for (situation in situations) {
    feature_name <- paste0(stat, '_2_', situation, '_p60_avg')
    delta_name <- paste0('d', cap_first(stat), '_2_', situation, '_p60_avg')
    stat_col <- paste0(stat, '_2_', situation)
    mp_col <- paste0('mP_2_', situation)
    stat_col_recent <- paste0(stat_col, '_recent')
    stat_col_prior <- paste0(stat_col, '_prior')
    mp_col_recent <- paste0(mp_col, '_recent')
    mp_col_prior <- paste0(mp_col, '_prior')
    feature_exprs[[feature_name]] <- rlang::expr(
      calc_avg_p60(
        .data[[!!stat_col_recent]],
        .data[[!!mp_col_recent]],
        .data[[!!stat_col_prior]],
        .data[[!!mp_col_prior]]
      )
    )
    feature_exprs[[delta_name]] <- rlang::expr(
      calc_single_p60(.data[[!!stat_col_recent]], .data[[!!mp_col_recent]]) -
        calc_single_p60(.data[[!!stat_col_prior]], .data[[!!mp_col_prior]])
    )
  }
}
for (situation in situations) {
  mp_col_recent <- paste0('mP_2_', situation, '_recent')
  mp_col_prior <- paste0('mP_2_', situation, '_prior')
  mp_avg_name <- paste0('mP_2_', situation, '_avg')
  mp_delta_name <- paste0('dMP_2_', situation, '_avg')
  feature_exprs[[mp_avg_name]] <- rlang::expr(
    calc_avg_minutes(.data[[!!mp_col_recent]], .data[[!!mp_col_prior]])
  )
  feature_exprs[[mp_delta_name]] <- rlang::expr(
    tidyr::replace_na(.data[[!!mp_col_recent]], 0) - tidyr::replace_na(.data[[!!mp_col_prior]], 0)
  )
}
feature_names <- names(feature_exprs)
stats_2y_features <- contract_stats_wide %>%
  dplyr::transmute(contractRowId, !!!feature_exprs)
contracts_train <- contracts_train %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::left_join(stats_2y_features, by = 'contractRowId') %>%
  dplyr::select(-contractRowId) %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(feature_names), ~ tidyr::replace_na(.x, 0))
  )

# ----- Create Testing Set ----- #

age_ref_date <- as.Date(sprintf('%d-09-15', (END_SEASON_ID %% 1e4) + 1L))
last_contracts <- contracts %>%
  dplyr::filter(isLast & endSeasonId == END_SEASON_ID) %>%
  dplyr::left_join(
    caps %>% dplyr::transmute(startSeasonId = as.integer(season), capLast = cap),
    by = 'startSeasonId',
    relationship = 'many-to-one'
  ) %>%
  dplyr::transmute(
    playerId,
    contractNumber = contractNumber + 1L,
    prevTerm = term,
    prevAAVP = aav / capLast
  )
contracts_test <- last_contracts %>%
  dplyr::mutate(
    signedWithTeamId = NA_integer_,
    startSeasonId = END_SEASON_ID + 10001L,
    endSeasonId = NA_integer_,
    isFirst = FALSE,
    isLast = TRUE
  ) %>%
  dplyr::left_join(
    caps %>% dplyr::transmute(startSeasonId = as.integer(season), cap = cap),
    by = 'startSeasonId',
    relationship = 'many-to-one'
  ) %>%
  dplyr::left_join(
    bios %>% dplyr::select(playerId, positionCode, birthDate, height, weight, handCode),
    by = 'playerId',
    relationship = 'many-to-one'
  ) %>%
  dplyr::filter(positionCode != 'G') %>%
  dplyr::mutate(
    ageAtSigning = floor(as.numeric(difftime(age_ref_date, as.Date(birthDate), units = 'days')) / 365.25),
    aavP = NA_real_
  ) %>%
  tidyr::expand_grid(
    tibble::tibble(
      isResign = c(rep(TRUE, 8L), rep(FALSE, 7L)),
      term = c(1L:8L, 1L:7L)
    )
  ) %>%
  dplyr::select(
    playerId,
    signedWithTeamId,
    startSeasonId,
    endSeasonId,
    isFirst,
    isLast,
    birthDate,
    cap,
    ageAtSigning,
    contractNumber,
    prevTerm,
    prevAAVP,
    positionCode,
    height,
    weight,
    handCode,
    isResign,
    term,
    aavP
  )
contract_season_rows_test <- contracts_test %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::select(contractRowId, playerId, startSeasonId) %>%
  tidyr::expand_grid(lookback = c(2L, 3L)) %>%
  dplyr::mutate(seasonId = startSeasonId - (lookback * 10001L))
contract_stats_long_test <- contract_season_rows_test %>%
  dplyr::left_join(sss_lookup, by = c('playerId', 'seasonId'), relationship = 'many-to-one') %>%
  dplyr::left_join(ssa_lookup, by = c('playerId', 'seasonId'), relationship = 'many-to-one')
recent_stats_test <- contract_stats_long_test %>%
  dplyr::filter(lookback == 2L) %>%
  dplyr::select(contractRowId, dplyr::all_of(value_cols)) %>%
  dplyr::rename_with(~ paste0(.x, '_recent'), -contractRowId)
prior_stats_test <- contract_stats_long_test %>%
  dplyr::filter(lookback == 3L) %>%
  dplyr::select(contractRowId, dplyr::all_of(value_cols)) %>%
  dplyr::rename_with(~ paste0(.x, '_prior'), -contractRowId)
contract_stats_wide_test <- recent_stats_test %>%
  dplyr::left_join(prior_stats_test, by = 'contractRowId', relationship = 'one-to-one')
stats_2y_features_test <- contract_stats_wide_test %>%
  dplyr::transmute(contractRowId, !!!feature_exprs)
contracts_test <- contracts_test %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::left_join(stats_2y_features_test, by = 'contractRowId', relationship = 'one-to-one') %>%
  dplyr::select(-contractRowId) %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(feature_names), ~ tidyr::replace_na(.x, 0))
  )

# Split model data into train and validate sets.
contracts_validate <- contracts_train %>%
  dplyr::filter(startSeasonId == VALIDATE_SEASON_ID)
contracts_train <- contracts_train %>%
  dplyr::filter(startSeasonId != VALIDATE_SEASON_ID)

# Write to CSV.
readr::write_csv(contracts_train, 'models/contracts/data/skater_contracts_train.csv')
readr::write_csv(contracts_validate, 'models/contracts/data/skater_contracts_validate.csv')
readr::write_csv(contracts_test, 'models/contracts/data/skater_contracts_test.csv')
readr::write_csv(contracts_clean, 'data/skater_contracts.csv')
