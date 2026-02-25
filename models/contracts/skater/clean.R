# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Set constant.
END_SEASON_ID <- 20252026

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
  dplyr::filter(!is.na(ageAtSigning) & !is.na(aav) & startSeasonId >= 20132014)

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

# Convert two seasons of stats to minutes-weighted per-60 average.
calc_avg_p60 <- function(stat_recent, minutes_recent, stat_prior, minutes_prior) {
  valid_recent <- !is.na(minutes_recent) & minutes_recent > 0
  valid_prior  <- !is.na(minutes_prior) & minutes_prior > 0
  total_minutes <- dplyr::if_else(valid_recent, minutes_recent, 0) +
    dplyr::if_else(valid_prior, minutes_prior, 0)
  total_stat <- dplyr::if_else(valid_recent, tidyr::replace_na(stat_recent, 0), 0) +
    dplyr::if_else(valid_prior, tidyr::replace_na(stat_prior, 0), 0)
  dplyr::if_else(total_minutes > 0, 60 * total_stat / total_minutes, 0)
}

# Average minutes across two prior seasons.
calc_avg_minutes <- function(minutes_recent, minutes_prior) {
  valid_recent <- !is.na(minutes_recent) & minutes_recent > 0
  valid_prior  <- !is.na(minutes_prior) & minutes_prior > 0
  total_minutes <- dplyr::if_else(valid_recent, minutes_recent, 0) +
    dplyr::if_else(valid_prior, minutes_prior, 0)
  n_valid <- as.numeric(valid_recent) + as.numeric(valid_prior)
  dplyr::if_else(n_valid > 0, total_minutes / n_valid, 0)
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
  tidyr::expand_grid(lookback = c(1L, 2L)) %>%
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
  dplyr::filter(lookback == 1L) %>%
  dplyr::select(contractRowId, dplyr::all_of(value_cols)) %>%
  dplyr::rename_with(~ paste0(.x, '_recent'), -contractRowId)
prior_stats <- contract_stats_long %>%
  dplyr::filter(lookback == 2L) %>%
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

age_ref_date <- as.Date(sprintf('%d-10-01', (END_SEASON_ID %% 1e4) + 1L))
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
    term = NA_integer_,
    aavP = NA_real_
  ) %>%
  tidyr::expand_grid(isResign = c(TRUE, FALSE)) %>%
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
  tidyr::expand_grid(lookback = c(1L, 2L)) %>%
  dplyr::mutate(seasonId = startSeasonId - (lookback * 10001L))
contract_stats_long_test <- contract_season_rows_test %>%
  dplyr::left_join(sss_lookup, by = c('playerId', 'seasonId'), relationship = 'many-to-one') %>%
  dplyr::left_join(ssa_lookup, by = c('playerId', 'seasonId'), relationship = 'many-to-one')
recent_stats_test <- contract_stats_long_test %>%
  dplyr::filter(lookback == 1L) %>%
  dplyr::select(contractRowId, dplyr::all_of(value_cols)) %>%
  dplyr::rename_with(~ paste0(.x, '_recent'), -contractRowId)
prior_stats_test <- contract_stats_long_test %>%
  dplyr::filter(lookback == 2L) %>%
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

# Write to CSV.
readr::write_csv(contracts_train, 'models/contracts/data/skater_contracts_train.csv')
readr::write_csv(contracts_test, 'models/contracts/data/skater_contracts_test.csv')
