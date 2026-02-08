# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(Matrix))
suppressMessages(library(stringr))
suppressMessages(library(data.table))
suppressMessages(library(lightgbm))
suppressMessages(library(nhlscraper))

# Define constant.
SEASON <- 20252026

# Define xG model versions to use.
STANDARD_VERSION <- 4L
SPECIAL_VERSION  <- 4L
EMPTY_VERSION    <- 4L
SHOOTOUT_VERSION <- 4L

# ----- Helpers ----- #

`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (length(x) == 1L && is.na(x)) return(y)
  x
}

safe_skater_summary <- function(season, game_type) {
  out <- tryCatch(
    nhlscraper::skater_season_report(
      season    = season,
      game_type = game_type,
      category  = 'summary'
    ),
    error = function(e) tibble::tibble()
  )
  if (nrow(out) == 0) {
    return(tibble::tibble(
      playerId = integer(),
      !!paste0('gP_', game_type) := integer(),
      !!paste0('mP_', game_type) := double()
    ))
  }
  out %>%
    dplyr::mutate(mP = timeOnIcePerGame * gamesPlayed / 60) %>%
    dplyr::select(
      playerId,
      !!paste0('gP_', game_type) := gamesPlayed,
      !!paste0('mP_', game_type) := mP
    )
}

safe_goalie_summary <- function(season, game_type) {
  out <- tryCatch(
    nhlscraper::goalie_season_report(
      season    = season,
      game_type = game_type,
      category  = 'summary'
    ),
    error = function(e) tibble::tibble()
  )
  if (nrow(out) == 0) {
    return(tibble::tibble(
      playerId = integer(),
      !!paste0('gP_', game_type) := integer(),
      !!paste0('mP_', game_type) := double()
    ))
  }
  out %>%
    dplyr::mutate(mP = timeOnIce / 60) %>%
    dplyr::select(
      playerId,
      !!paste0('gP_', game_type) := gamesPlayed,
      !!paste0('mP_', game_type) := mP
    )
}

na_playoff_cols_if_absent <- function(df, playoffs_present) {
  if (isTRUE(playoffs_present)) return(df)
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::matches('(^gP_3$|^mP_3$|^(d|a)_3$|_3_std$|_3_all$)'),
        \(x) { x[] <- NA; x }
      )
    )
}

load_model_bundle <- function(path) {
  if (!base::file.exists(path)) {
    base::stop(paste0('Model file not found: ', path))
  }
  obj <- base::readRDS(path)
  if (!base::is.list(obj) || !all(c('model', 'rec_prep') %in% base::names(obj))) {
    base::stop(paste0('Invalid model bundle at: ', path, ' (expected a list with $model and $rec_prep).'))
  }
  obj
}

predict_xg_bundle <- function(obj, new_data) {
  booster  <- obj$model
  rec_prep <- obj$rec_prep

  x <- recipes::bake(rec_prep, new_data = new_data, recipes::all_predictors()) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0)))

  m <- Matrix::sparse.model.matrix(~ . - 1, data = x, na.action = stats::na.pass)

  base::as.numeric(stats::predict(booster, m))
}

make_std_all_cols <- function(prefixes, season_suffix) {
  out <- character()
  for (p in prefixes) {
    out <- c(out, paste0(p, season_suffix, '_std'), paste0(p, season_suffix, '_all'))
  }
  out
}

# ----- Test ----- #

# Build model paths.
STANDARD_PATH <- paste0('models/xG/standard/model', STANDARD_VERSION, '.rds')
SPECIAL_PATH  <- paste0('models/xG/special/model',  SPECIAL_VERSION,  '.rds')
SHOOTOUT_PATH <- paste0('models/xG/shootout/model', SHOOTOUT_VERSION, '.rds')
EMPTY_PATH    <- paste0('models/xG/empty/model',    EMPTY_VERSION,    '.rds')

# Load model bundles.
obj_standard <- load_model_bundle(STANDARD_PATH)
obj_special  <- load_model_bundle(SPECIAL_PATH)
obj_shootout <- load_model_bundle(SHOOTOUT_PATH)
obj_empty    <- load_model_bundle(EMPTY_PATH)

# Load data.
pbps <- nhlscraper::gc_pbps(SEASON)

# Build feature-enriched pbps for xG scoring (only compute what we need).
pbps_xg <- pbps
NEED_SPEED <- (
  STANDARD_VERSION %in% c(2L, 3L, 4L) ||
    SPECIAL_VERSION %in% c(2L, 3L, 4L) ||
    EMPTY_VERSION %in% c(2L, 3L, 4L) ||
    SHOOTOUT_VERSION %in% c(3L, 4L)
)
NEED_SHOOTER_BIO <- base::any(c(STANDARD_VERSION, SPECIAL_VERSION, SHOOTOUT_VERSION, EMPTY_VERSION) == 4L)
NEED_GOALIE_BIO  <- base::any(c(STANDARD_VERSION, SPECIAL_VERSION, SHOOTOUT_VERSION) == 4L)
if (isTRUE(NEED_SPEED)) {
  pbps_xg <- nhlscraper::calculate_speed(pbps_xg)
}
if (isTRUE(NEED_SHOOTER_BIO)) {
  pbps_xg <- nhlscraper::add_shooter_biometrics(pbps_xg)
}
if (isTRUE(NEED_GOALIE_BIO)) {
  pbps_xg <- nhlscraper::add_goalie_biometrics(pbps_xg)
}

# Create testing set.
shots <- pbps_xg %>%
  dplyr::filter(
    # Keep only regular season and playoffs.
    gameTypeId %in% 2:3,
    # Keep only shots.
    typeDescKey %in% c(
      'goal',
      'shot-on-goal',
      'missed-shot',
      'blocked-shot'
    )
  ) %>%
  dplyr::mutate(
    shootingPlayerId  = dplyr::coalesce(shootingPlayerId, scoringPlayerId),
    situationCode     = base::as.character(situationCode),
    isEmptyNetFor     = dplyr::coalesce(isEmptyNetFor, FALSE),
    isEmptyNetAgainst = dplyr::coalesce(isEmptyNetAgainst, FALSE),
    isShootout        = (gameTypeId == 2 & period == 5),
    shotType          = tidyr::replace_na(shotType, 'wrist'),
    shotType          = base::factor(shotType),
    isPlayoff         = gameTypeId == 3,
    isGoal            = typeDescKey == 'goal',
    isGoal            = base::factor(
      isGoal,
      levels = c(FALSE, TRUE),
      labels = c('no', 'yes')
    )
  ) %>%
  dplyr::select(
    # IDs
    gameId,
    eventId,
    eventOwnerTeamId,
    shootingPlayerId,
    goalieInNetId,
    typeDescKey,
    situationCode,
    isShootout,
    # Predictors
    isHome,
    isPlayoff,
    period,
    secondsElapsedInPeriod,
    secondsElapsedInGame,
    isEmptyNetFor,
    isEmptyNetAgainst,
    skaterCountFor,
    skaterCountAgainst,
    distance,
    angle,
    dplyr::any_of(c('dDdT', 'dAdT')),
    shotType,
    isRush,
    isRebound,
    goalsFor,
    goalsAgainst,
    SOGFor,
    SOGAgainst,
    fenwickFor,
    fenwickAgainst,
    corsiFor,
    corsiAgainst,
    dplyr::any_of(c(
      'shooterHeight',
      'shooterWeight',
      'shooterAge',
      'shooterSide',
      'shooterPositionCode',
      'goalieHeight',
      'goalieWeight',
      'goalieAge',
      'goalieSide'
    )),
    # Response
    isGoal
  )

rm(pbps_xg, NEED_SPEED, NEED_SHOOTER_BIO, NEED_GOALIE_BIO)

# Normalize biometrics factor columns if present (mirrors training mutates).
if ('shooterSide' %in% base::names(shots)) {
  shots <- shots %>%
    dplyr::mutate(
      shooterSide = tidyr::replace_na(shooterSide, 'neutral'),
      shooterSide = base::factor(shooterSide)
    )
}
if ('shooterPositionCode' %in% base::names(shots)) {
  shots <- shots %>%
    dplyr::mutate(shooterPositionCode = base::factor(shooterPositionCode))
}
if ('goalieSide' %in% base::names(shots)) {
  shots <- shots %>%
    dplyr::mutate(
      goalieSide = tidyr::replace_na(goalieSide, 'neutral'),
      goalieSide = base::factor(goalieSide)
    )
}

# Detect playoffs.
PLAYOFFS_PRESENT <- base::any(shots$isPlayoff, na.rm = TRUE)

# Add rowId for safe merge-back after scoring.
shots <- shots %>%
  dplyr::mutate(rowId = dplyr::row_number())

# ----- Predict ----- #

# Separate blocks from rest.
shots_block <- shots %>%
  dplyr::filter(typeDescKey == 'blocked-shot') %>%
  dplyr::mutate(xG = 0)
shots_score <- shots %>%
  dplyr::filter(typeDescKey != 'blocked-shot') %>%
  dplyr::mutate(
    bucket = dplyr::case_when(
      isEmptyNetAgainst ~ 'empty',
      situationCode %in% c('1010', '0101') ~ 'shootout',
      situationCode == '1551' ~ 'standard',
      TRUE ~ 'special'
    )
  )

# Predict xG.
pred_standard <- shots_score %>%
  dplyr::filter(bucket == 'standard')
pred_special <- shots_score %>%
  dplyr::filter(bucket == 'special')
pred_shootout <- shots_score %>%
  dplyr::filter(bucket == 'shootout')
pred_empty <- shots_score %>%
  dplyr::filter(bucket == 'empty')
preds <- dplyr::bind_rows(
  if (nrow(pred_standard) > 0) {
    tibble::tibble(
      rowId = pred_standard$rowId,
      xG    = predict_xg_bundle(obj_standard, pred_standard)
    )
  } else tibble::tibble(rowId = integer(), xG = double()),
  if (nrow(pred_special) > 0) {
    tibble::tibble(
      rowId = pred_special$rowId,
      xG    = predict_xg_bundle(obj_special, pred_special)
    )
  } else tibble::tibble(rowId = integer(), xG = double()),
  if (nrow(pred_shootout) > 0) {
    tibble::tibble(
      rowId = pred_shootout$rowId,
      xG    = predict_xg_bundle(obj_shootout, pred_shootout)
    )
  } else tibble::tibble(rowId = integer(), xG = double()),
  if (nrow(pred_empty) > 0) {
    tibble::tibble(
      rowId = pred_empty$rowId,
      xG    = predict_xg_bundle(obj_empty, pred_empty)
    )
  } else tibble::tibble(rowId = integer(), xG = double())
)
shots_score <- shots_score %>%
  dplyr::left_join(preds, by = 'rowId') %>%
  dplyr::select(-bucket)
shots <- dplyr::bind_rows(
  shots_score,
  shots_block
) %>%
  dplyr::arrange(gameId, period, secondsElapsedInPeriod)
rm(
  obj_standard, obj_special, obj_shootout, obj_empty,
  pred_standard, pred_special, pred_shootout, pred_empty,
  preds, shots_score, shots_block
)

# Pull shifts, then merge with pbps.
shifts <- nhlscraper::shift_charts(SEASON)
pbps_onice <- nhlscraper::add_on_ice_players(pbps, shifts) %>%
  dplyr::select(
    gameId,
    eventId,
    playerIdsFor,
    playerIdsAgainst
  )
rm(pbps, shifts)

# Join on-ice lists.
shots <- shots %>%
  dplyr::left_join(pbps_onice, by = c('gameId', 'eventId')) %>%
  dplyr::mutate(
    playerIdsFor = purrr::map2(
      playerIdsFor, shootingPlayerId,
      \(ids, shooter) base::sort(base::unique(base::c(ids %||% integer(), shooter)))
    ),
    playerIdsAgainst = purrr::map2(
      playerIdsAgainst, goalieInNetId,
      \(ids, goalie) {
        if (base::is.na(goalie)) ids else base::sort(base::unique(base::c(ids %||% integer(), goalie)))
      }
    ),
    playerIdsFor = dplyr::if_else(
      skaterCountFor == 1L,
      purrr::map(shootingPlayerId, \(x) if (base::is.na(x)) integer() else base::as.integer(x)),
      playerIdsFor
    ),
    playerIdsAgainst = dplyr::if_else(
      skaterCountFor == 1L,
      purrr::map(goalieInNetId, \(x) if (base::is.na(x)) integer() else base::as.integer(x)),
      playerIdsAgainst
    )
  )
rm(pbps_onice)

# Drop rowId now that xG is merged.
shots <- shots %>%
  dplyr::select(-rowId)

# Remove shootouts.
shots_out <- shots %>%
  dplyr::filter(!isShootout)

# ----- Analysis ----- #

# Calculate skater metrics.
skater_shots <- shots_out %>%
  dplyr::mutate(
    playerId  = shootingPlayerId,
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot',
    isStd     = dplyr::coalesce(situationCode == '1551', FALSE),
    xG        = dplyr::coalesce(xG, 0)
  ) %>%
  dplyr::filter(!is.na(playerId)) %>%
  dplyr::group_by(playerId) %>%
  dplyr::summarise(
    d_2 = dplyr::if_else(
      base::sum(!isPlayoff & !is.na(distance)) > 0,
      base::mean(distance[!isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    a_2 = dplyr::if_else(
      base::sum(!isPlayoff & !is.na(angle)) > 0,
      base::mean(angle[!isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    d_3 = dplyr::if_else(
      base::sum(isPlayoff & !is.na(distance)) > 0,
      base::mean(distance[isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    a_3 = dplyr::if_else(
      base::sum(isPlayoff & !is.na(angle)) > 0,
      base::mean(angle[isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    iCorsiF_2_std   = base::sum(dplyr::if_else(!isPlayoff & isStd, 1L, 0L), na.rm = TRUE),
    iCorsiF_2_all   = base::sum(dplyr::if_else(!isPlayoff,        1L, 0L), na.rm = TRUE),
    iFenwickF_2_std = base::sum(dplyr::if_else(!isPlayoff & isStd & isFenwick, 1L, 0L), na.rm = TRUE),
    iFenwickF_2_all = base::sum(dplyr::if_else(!isPlayoff & isFenwick,        1L, 0L), na.rm = TRUE),
    iSOGF_2_std     = base::sum(dplyr::if_else(!isPlayoff & isStd & isSOG,     1L, 0L), na.rm = TRUE),
    iSOGF_2_all     = base::sum(dplyr::if_else(!isPlayoff & isSOG,             1L, 0L), na.rm = TRUE),
    iGF_2_std       = base::sum(dplyr::if_else(!isPlayoff & isStd & (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    iGF_2_all       = base::sum(dplyr::if_else(!isPlayoff &        (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    ixGF_2_std      = base::sum(dplyr::if_else(!isPlayoff & isStd, xG, 0), na.rm = TRUE),
    ixGF_2_all      = base::sum(dplyr::if_else(!isPlayoff,        xG, 0), na.rm = TRUE),
    iCorsiF_3_std   = base::sum(dplyr::if_else(isPlayoff & isStd, 1L, 0L), na.rm = TRUE),
    iCorsiF_3_all   = base::sum(dplyr::if_else(isPlayoff,        1L, 0L), na.rm = TRUE),
    iFenwickF_3_std = base::sum(dplyr::if_else(isPlayoff & isStd & isFenwick, 1L, 0L), na.rm = TRUE),
    iFenwickF_3_all = base::sum(dplyr::if_else(isPlayoff & isFenwick,        1L, 0L), na.rm = TRUE),
    iSOGF_3_std     = base::sum(dplyr::if_else(isPlayoff & isStd & isSOG,     1L, 0L), na.rm = TRUE),
    iSOGF_3_all     = base::sum(dplyr::if_else(isPlayoff & isSOG,             1L, 0L), na.rm = TRUE),
    iGF_3_std       = base::sum(dplyr::if_else(isPlayoff & isStd & (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    iGF_3_all       = base::sum(dplyr::if_else(isPlayoff &        (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    ixGF_3_std      = base::sum(dplyr::if_else(isPlayoff & isStd, xG, 0), na.rm = TRUE),
    ixGF_3_all      = base::sum(dplyr::if_else(isPlayoff,        xG, 0), na.rm = TRUE),
    .groups = 'drop'
  )
skater_onice <- shots_out %>%
  dplyr::mutate(
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot',
    isStd     = dplyr::coalesce(situationCode == '1551', FALSE),
    xG        = dplyr::coalesce(xG, 0)
  ) %>%
  dplyr::filter(!is.na(playerIdsFor)) %>%
  tidyr::unnest_longer(playerIdsFor, values_to = 'playerId') %>%
  dplyr::filter(!is.na(playerId)) %>%
  dplyr::group_by(playerId) %>%
  dplyr::summarise(
    oCorsiF_2_std   = base::sum(dplyr::if_else(!isPlayoff & isStd, 1L, 0L), na.rm = TRUE),
    oCorsiF_2_all   = base::sum(dplyr::if_else(!isPlayoff,        1L, 0L), na.rm = TRUE),
    oFenwickF_2_std = base::sum(dplyr::if_else(!isPlayoff & isStd & isFenwick, 1L, 0L), na.rm = TRUE),
    oFenwickF_2_all = base::sum(dplyr::if_else(!isPlayoff & isFenwick,        1L, 0L), na.rm = TRUE),
    oSOGF_2_std     = base::sum(dplyr::if_else(!isPlayoff & isStd & isSOG,     1L, 0L), na.rm = TRUE),
    oSOGF_2_all     = base::sum(dplyr::if_else(!isPlayoff & isSOG,             1L, 0L), na.rm = TRUE),
    oGF_2_std       = base::sum(dplyr::if_else(!isPlayoff & isStd & (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    oGF_2_all       = base::sum(dplyr::if_else(!isPlayoff &        (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    oxGF_2_std      = base::sum(dplyr::if_else(!isPlayoff & isStd, xG, 0), na.rm = TRUE),
    oxGF_2_all      = base::sum(dplyr::if_else(!isPlayoff,        xG, 0), na.rm = TRUE),
    oCorsiF_3_std   = base::sum(dplyr::if_else(isPlayoff & isStd, 1L, 0L), na.rm = TRUE),
    oCorsiF_3_all   = base::sum(dplyr::if_else(isPlayoff,        1L, 0L), na.rm = TRUE),
    oFenwickF_3_std = base::sum(dplyr::if_else(isPlayoff & isStd & isFenwick, 1L, 0L), na.rm = TRUE),
    oFenwickF_3_all = base::sum(dplyr::if_else(isPlayoff & isFenwick,        1L, 0L), na.rm = TRUE),
    oSOGF_3_std     = base::sum(dplyr::if_else(isPlayoff & isStd & isSOG,     1L, 0L), na.rm = TRUE),
    oSOGF_3_all     = base::sum(dplyr::if_else(isPlayoff & isSOG,             1L, 0L), na.rm = TRUE),
    oGF_3_std       = base::sum(dplyr::if_else(isPlayoff & isStd & (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    oGF_3_all       = base::sum(dplyr::if_else(isPlayoff &        (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    oxGF_3_std      = base::sum(dplyr::if_else(isPlayoff & isStd, xG, 0), na.rm = TRUE),
    oxGF_3_all      = base::sum(dplyr::if_else(isPlayoff,        xG, 0), na.rm = TRUE),
    .groups = 'drop'
  )
goalie_ids <- shots_out %>%
  dplyr::distinct(goalieInNetId) %>%
  dplyr::filter(!is.na(goalieInNetId)) %>%
  dplyr::pull(goalieInNetId)
skater_onice_again <- shots_out %>%
  dplyr::mutate(
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot',
    isStd     = dplyr::coalesce(situationCode == '1551', FALSE),
    xG        = dplyr::coalesce(xG, 0)
  ) %>%
  dplyr::filter(!is.na(playerIdsAgainst)) %>%
  tidyr::unnest_longer(playerIdsAgainst, values_to = 'playerId') %>%
  dplyr::filter(!is.na(playerId)) %>%
  dplyr::filter(!playerId %in% goalie_ids) %>%
  dplyr::group_by(playerId) %>%
  dplyr::summarise(
    oCorsiA_2_std   = base::sum(dplyr::if_else(!isPlayoff & isStd, 1L, 0L), na.rm = TRUE),
    oCorsiA_2_all   = base::sum(dplyr::if_else(!isPlayoff,        1L, 0L), na.rm = TRUE),
    oFenwickA_2_std = base::sum(dplyr::if_else(!isPlayoff & isStd & isFenwick, 1L, 0L), na.rm = TRUE),
    oFenwickA_2_all = base::sum(dplyr::if_else(!isPlayoff & isFenwick,        1L, 0L), na.rm = TRUE),
    oSOGA_2_std     = base::sum(dplyr::if_else(!isPlayoff & isStd & isSOG,     1L, 0L), na.rm = TRUE),
    oSOGA_2_all     = base::sum(dplyr::if_else(!isPlayoff & isSOG,             1L, 0L), na.rm = TRUE),
    oGA_2_std       = base::sum(dplyr::if_else(!isPlayoff & isStd & (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    oGA_2_all       = base::sum(dplyr::if_else(!isPlayoff &        (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    oxGA_2_std      = base::sum(dplyr::if_else(!isPlayoff & isStd, xG, 0), na.rm = TRUE),
    oxGA_2_all      = base::sum(dplyr::if_else(!isPlayoff,        xG, 0), na.rm = TRUE),
    oCorsiA_3_std   = base::sum(dplyr::if_else(isPlayoff & isStd, 1L, 0L), na.rm = TRUE),
    oCorsiA_3_all   = base::sum(dplyr::if_else(isPlayoff,        1L, 0L), na.rm = TRUE),
    oFenwickA_3_std = base::sum(dplyr::if_else(isPlayoff & isStd & isFenwick, 1L, 0L), na.rm = TRUE),
    oFenwickA_3_all = base::sum(dplyr::if_else(isPlayoff & isFenwick,        1L, 0L), na.rm = TRUE),
    oSOGA_3_std     = base::sum(dplyr::if_else(isPlayoff & isStd & isSOG,     1L, 0L), na.rm = TRUE),
    oSOGA_3_all     = base::sum(dplyr::if_else(isPlayoff & isSOG,             1L, 0L), na.rm = TRUE),
    oGA_3_std       = base::sum(dplyr::if_else(isPlayoff & isStd & (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    oGA_3_all       = base::sum(dplyr::if_else(isPlayoff &        (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    oxGA_3_std      = base::sum(dplyr::if_else(isPlayoff & isStd, xG, 0), na.rm = TRUE),
    oxGA_3_all      = base::sum(dplyr::if_else(isPlayoff,        xG, 0), na.rm = TRUE),
    .groups = 'drop'
  )
skater_shots <- skater_shots %>%
  dplyr::full_join(skater_onice,       by = 'playerId') %>%
  dplyr::full_join(skater_onice_again, by = 'playerId')

rm(skater_onice, skater_onice_again, goalie_ids)

# Calculate goalie metrics.
goalie_shots <- shots_out %>%
  dplyr::filter(!is.na(goalieInNetId)) %>%
  dplyr::mutate(
    playerId  = goalieInNetId,
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot',
    isStd     = dplyr::coalesce(situationCode == '1551', FALSE),
    xG        = dplyr::coalesce(xG, 0)
  ) %>%
  dplyr::group_by(playerId) %>%
  dplyr::summarise(
    d_2 = dplyr::if_else(
      sum(!isPlayoff & !is.na(distance)) > 0,
      mean(distance[!isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    a_2 = dplyr::if_else(
      sum(!isPlayoff & !is.na(angle)) > 0,
      mean(angle[!isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    d_3 = dplyr::if_else(
      sum(isPlayoff & !is.na(distance)) > 0,
      mean(distance[isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    a_3 = dplyr::if_else(
      sum(isPlayoff & !is.na(angle)) > 0,
      mean(angle[isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    FenwickA_2_std = sum(dplyr::if_else(!isPlayoff & isStd & isFenwick, 1L, 0L), na.rm = TRUE),
    FenwickA_2_all = sum(dplyr::if_else(!isPlayoff & isFenwick,        1L, 0L), na.rm = TRUE),
    SOGA_2_std     = sum(dplyr::if_else(!isPlayoff & isStd & isSOG,     1L, 0L), na.rm = TRUE),
    SOGA_2_all     = sum(dplyr::if_else(!isPlayoff & isSOG,             1L, 0L), na.rm = TRUE),
    GA_2_std       = sum(dplyr::if_else(!isPlayoff & isStd & (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    GA_2_all       = sum(dplyr::if_else(!isPlayoff &        (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    xGA_2_std      = sum(dplyr::if_else(!isPlayoff & isStd, xG, 0), na.rm = TRUE),
    xGA_2_all      = sum(dplyr::if_else(!isPlayoff,        xG, 0), na.rm = TRUE),
    FenwickA_3_std = sum(dplyr::if_else(isPlayoff & isStd & isFenwick, 1L, 0L), na.rm = TRUE),
    FenwickA_3_all = sum(dplyr::if_else(isPlayoff & isFenwick,        1L, 0L), na.rm = TRUE),
    SOGA_3_std     = sum(dplyr::if_else(isPlayoff & isStd & isSOG,     1L, 0L), na.rm = TRUE),
    SOGA_3_all     = sum(dplyr::if_else(isPlayoff & isSOG,             1L, 0L), na.rm = TRUE),
    GA_3_std       = sum(dplyr::if_else(isPlayoff & isStd & (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    GA_3_all       = sum(dplyr::if_else(isPlayoff &        (isGoal == 'yes'), 1L, 0L), na.rm = TRUE),
    xGA_3_std      = sum(dplyr::if_else(isPlayoff & isStd, xG, 0), na.rm = TRUE),
    xGA_3_all      = sum(dplyr::if_else(isPlayoff,        xG, 0), na.rm = TRUE),
    .groups = 'drop'
  )
goalie_corsi <- shots_out %>%
  dplyr::filter(!is.na(playerIdsAgainst)) %>%
  tidyr::unnest_longer(playerIdsAgainst, values_to = 'playerId') %>%
  dplyr::group_by(playerId) %>%
  dplyr::summarise(
    CorsiA_2 = base::sum(dplyr::if_else(!isPlayoff, 1L, 0L), na.rm = TRUE),
    CorsiA_3 = base::sum(dplyr::if_else( isPlayoff, 1L, 0L), na.rm = TRUE),
    .groups = 'drop'
  )
goalie_shots <- goalie_shots %>%
  dplyr::left_join(goalie_corsi, by = 'playerId')
rm(goalie_corsi)

# Scrape supplemental data.
skater_season_summary_2 <- safe_skater_summary(SEASON, 2)
skater_season_summary_3 <- safe_skater_summary(SEASON, 3)
goalie_season_summary_2 <- safe_goalie_summary(SEASON, 2)
goalie_season_summary_3 <- safe_goalie_summary(SEASON, 3)
rm(safe_skater_summary, safe_goalie_summary)

# Merge skater data.frames.
skater_shot_analysis <- base::list(
  skater_shots,
  skater_season_summary_2,
  skater_season_summary_3
) %>%
  purrr::reduce(dplyr::full_join, by = 'playerId') %>%
  dplyr::filter(playerId %in% base::union(
    skater_season_summary_2$playerId,
    skater_season_summary_3$playerId
  )) %>%
  dplyr::mutate(
    dplyr::across(!dplyr::matches('^(d|a)_[23]$'), ~ tidyr::replace_na(.x, 0))
  )
rm(skater_shots, skater_season_summary_2, skater_season_summary_3)

# Re-order columns.
i_prefix <- c('iCorsiF', 'iFenwickF', 'iSOGF', 'iGF', 'ixGF')
of_prefix <- c('oCorsiF', 'oFenwickF', 'oSOGF', 'oGF', 'oxGF')
oa_prefix <- c('oCorsiA', 'oFenwickA', 'oSOGA', 'oGA', 'oxGA')
skater_keep_cols <- c(
  'playerId',
  'gP_2',
  'mP_2',
  'd_2',
  'a_2',
  make_std_all_cols(i_prefix,  '_2'),
  make_std_all_cols(of_prefix, '_2'),
  make_std_all_cols(oa_prefix, '_2'),
  'gP_3',
  'mP_3',
  'd_3',
  'a_3',
  make_std_all_cols(i_prefix,  '_3'),
  make_std_all_cols(of_prefix, '_3'),
  make_std_all_cols(oa_prefix, '_3')
)
skater_shot_analysis <- skater_shot_analysis %>%
  dplyr::select(dplyr::all_of(skater_keep_cols))

# Merge goalie data.frames.
goalie_shot_analysis <- base::list(
  goalie_shots,
  goalie_season_summary_2,
  goalie_season_summary_3
) %>%
  purrr::reduce(dplyr::full_join, by = 'playerId') %>%
  dplyr::filter(playerId %in% base::union(
    goalie_season_summary_2$playerId,
    goalie_season_summary_3$playerId
  )) %>%
  dplyr::mutate(
    dplyr::across(!dplyr::matches('^(d|a)_[23]$'), ~ tidyr::replace_na(.x, 0))
  )
rm(goalie_shots, goalie_season_summary_2, goalie_season_summary_3)
goalie_keep_cols <- c(
  'playerId',
  'gP_2',
  'mP_2',
  'd_2',
  'a_2',
  make_std_all_cols(c('FenwickA', 'SOGA', 'GA', 'xGA'), '_2'),
  'gP_3',
  'mP_3',
  'd_3',
  'a_3',
  make_std_all_cols(c('FenwickA', 'SOGA', 'GA', 'xGA'), '_3')
)
goalie_shot_analysis <- goalie_shot_analysis %>%
  dplyr::select(dplyr::all_of(goalie_keep_cols))

# Remove playoff data if not present.
skater_shot_analysis <- na_playoff_cols_if_absent(
  skater_shot_analysis,
  PLAYOFFS_PRESENT
)
goalie_shot_analysis <- na_playoff_cols_if_absent(
  goalie_shot_analysis,
  PLAYOFFS_PRESENT
)

# Write to CSV.
readr::write_csv(skater_shot_analysis, paste0(
  'data/skater_shot_analysis_',
  SEASON,
  '.csv'
))
readr::write_csv(goalie_shot_analysis, paste0(
  'data/goalie_shot_analysis_',
  SEASON,
  '.csv'
))
rm(SEASON, PLAYOFFS_PRESENT, na_playoff_cols_if_absent, EMPTY_PATH, EMPTY_VERSION, goalie_keep_cols, i_prefix, oa_prefix, of_prefix, SHOOTOUT_PATH, SHOOTOUT_VERSION, skater_keep_cols, SPECIAL_PATH, SPECIAL_VERSION, STANDARD_PATH, STANDARD_VERSION, load_model_bundle, make_std_all_cols, predict_xg_bundle, `%||%`)
