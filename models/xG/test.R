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

pull_numeric_or_na <- function(df, col) {
  if (col %in% base::names(df)) {
    return(base::as.numeric(df[[col]]))
  }
  rep(NA_real_, nrow(df))
}

safe_skater_timeonice <- function(season, game_type) {
  out <- tryCatch(
    nhlscraper::skater_season_report(
      season    = season,
      game_type = game_type,
      category  = 'timeonice'
    ),
    error = function(e) tibble::tibble()
  )
  if (nrow(out) == 0) {
    return(tibble::tibble(
      playerId = integer(),
      !!paste0('mP_', game_type, '_ev') := double(),
      !!paste0('mP_', game_type, '_pp') := double(),
      !!paste0('mP_', game_type, '_sh') := double(),
      !!paste0('mP_', game_type, '_all') := double()
    ))
  }
  out %>%
    dplyr::transmute(
      playerId,
      !!paste0('mP_', game_type, '_ev')  := pull_numeric_or_na(out, 'evTimeOnIce') / 60,
      !!paste0('mP_', game_type, '_pp')  := pull_numeric_or_na(out, 'ppTimeOnIce') / 60,
      !!paste0('mP_', game_type, '_sh')  := pull_numeric_or_na(out, 'shTimeOnIce') / 60,
      !!paste0('mP_', game_type, '_all') := pull_numeric_or_na(out, 'timeOnIce') / 60
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
      !!paste0('mP_', game_type, '_all') := double()
    ))
  }
  out %>%
    dplyr::mutate(mP = timeOnIce / 60) %>%
    dplyr::select(
      playerId,
      !!paste0('mP_', game_type, '_all') := mP
    )
}

na_playoff_cols_if_absent <- function(df, playoffs_present) {
  if (isTRUE(playoffs_present)) return(df)
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::matches('_3_(ev|pp|sh|all)$'),
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

make_strength_cols <- function(prefixes, season_suffix, strengths = c('ev', 'pp', 'sh', 'all')) {
  out <- character()
  for (p in prefixes) {
    out <- c(out, paste0(p, season_suffix, '_', strengths))
  }
  out
}

normalize_strength_state <- function(x) {
  out <- stringr::str_to_lower(base::as.character(x))
  out <- stringr::str_trim(out)
  dplyr::case_when(
    out == 'even-strength' ~ 'ev',
    out == 'power-play' ~ 'pp',
    out == 'penalty-kill' ~ 'sh',
    stringr::str_detect(out, '^ev') | stringr::str_detect(out, 'even') ~ 'ev',
    stringr::str_detect(out, '^pp') | stringr::str_detect(out, 'power') ~ 'pp',
    stringr::str_detect(out, '^sh') |
      stringr::str_detect(out, '^pk') |
      stringr::str_detect(out, 'short') |
      stringr::str_detect(out, 'penalty\\s*-?\\s*kill') ~ 'sh',
    TRUE ~ NA_character_
  )
}

flip_strength_code <- function(x) {
  dplyr::case_when(
    x == 'pp' ~ 'sh',
    x == 'sh' ~ 'pp',
    x == 'ev' ~ 'ev',
    TRUE ~ NA_character_
  )
}

expand_strength_categories <- function(df, strength_col) {
  dplyr::bind_rows(
    df %>% dplyr::mutate(strengthCategory = 'all'),
    df %>%
      dplyr::filter(!is.na(.data[[strength_col]])) %>%
      dplyr::mutate(strengthCategory = .data[[strength_col]])
  )
}

safe_mean <- function(x) {
  if (base::sum(!is.na(x)) > 0) {
    return(base::mean(x, na.rm = TRUE))
  }
  NA_real_
}

ensure_columns <- function(df, cols) {
  missing_cols <- base::setdiff(cols, base::names(df))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      df[[col]] <- NA_real_
    }
  }
  df
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
    strengthState     = base::as.character(strengthState),
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
    strengthState,
    isShootout,
    xCoordNorm,
    yCoordNorm,
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
  dplyr::filter(!isShootout) %>%
  dplyr::mutate(
    seasonType      = dplyr::if_else(isPlayoff, '3', '2'),
    isPenaltyShot   = situationCode %in% c('1010', '0101'),
    strengthFor     = normalize_strength_state(strengthState),
    strengthFor     = dplyr::if_else(isPenaltyShot, NA_character_, strengthFor),
    strengthAgainst = flip_strength_code(strengthFor),
    isSOG           = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick       = typeDescKey != 'blocked-shot',
    isGoalLogical   = (isGoal == 'yes'),
    xG              = dplyr::coalesce(xG, 0)
  )

# ----- Analysis ----- #

# Calculate skater metrics.
skater_shots <- shots_out %>%
  dplyr::mutate(
    playerId   = shootingPlayerId,
    iCorsiF    = 1L,
    iFenwickF  = base::as.integer(isFenwick),
    iSOGF      = base::as.integer(isSOG),
    iGF        = base::as.integer(isGoalLogical),
    ixGF       = xG
  ) %>%
  dplyr::filter(!is.na(playerId)) %>%
  expand_strength_categories('strengthFor') %>%
  dplyr::group_by(playerId, seasonType, strengthCategory) %>%
  dplyr::summarise(
    x = safe_mean(xCoordNorm),
    y = safe_mean(yCoordNorm),
    dplyr::across(
      dplyr::all_of(c('iCorsiF', 'iFenwickF', 'iSOGF', 'iGF', 'ixGF')),
      ~ base::sum(.x, na.rm = TRUE)
    ),
    .groups = 'drop'
  ) %>%
  tidyr::pivot_wider(
    names_from  = c(seasonType, strengthCategory),
    values_from = c(x, y, iCorsiF, iFenwickF, iSOGF, iGF, ixGF),
    names_glue  = '{.value}_{seasonType}_{strengthCategory}'
  )

skater_onice <- shots_out %>%
  dplyr::mutate(
    oCorsiF   = 1L,
    oFenwickF = base::as.integer(isFenwick),
    oSOGF     = base::as.integer(isSOG),
    oGF       = base::as.integer(isGoalLogical),
    oxGF      = xG
  ) %>%
  dplyr::filter(!is.na(playerIdsFor)) %>%
  tidyr::unnest_longer(playerIdsFor, values_to = 'playerId') %>%
  dplyr::filter(!is.na(playerId)) %>%
  expand_strength_categories('strengthFor') %>%
  dplyr::group_by(playerId, seasonType, strengthCategory) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(c('oCorsiF', 'oFenwickF', 'oSOGF', 'oGF', 'oxGF')),
      ~ base::sum(.x, na.rm = TRUE)
    ),
    .groups = 'drop'
  ) %>%
  tidyr::pivot_wider(
    names_from  = c(seasonType, strengthCategory),
    values_from = c(oCorsiF, oFenwickF, oSOGF, oGF, oxGF),
    names_glue  = '{.value}_{seasonType}_{strengthCategory}'
  )

goalie_ids <- shots_out %>%
  dplyr::distinct(goalieInNetId) %>%
  dplyr::filter(!is.na(goalieInNetId)) %>%
  dplyr::pull(goalieInNetId)

skater_onice_again <- shots_out %>%
  dplyr::mutate(
    oCorsiA   = 1L,
    oFenwickA = base::as.integer(isFenwick),
    oSOGA     = base::as.integer(isSOG),
    oGA       = base::as.integer(isGoalLogical),
    oxGA      = xG
  ) %>%
  dplyr::filter(!is.na(playerIdsAgainst)) %>%
  tidyr::unnest_longer(playerIdsAgainst, values_to = 'playerId') %>%
  dplyr::filter(!is.na(playerId)) %>%
  dplyr::filter(!playerId %in% goalie_ids) %>%
  expand_strength_categories('strengthAgainst') %>%
  dplyr::group_by(playerId, seasonType, strengthCategory) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(c('oCorsiA', 'oFenwickA', 'oSOGA', 'oGA', 'oxGA')),
      ~ base::sum(.x, na.rm = TRUE)
    ),
    .groups = 'drop'
  ) %>%
  tidyr::pivot_wider(
    names_from  = c(seasonType, strengthCategory),
    values_from = c(oCorsiA, oFenwickA, oSOGA, oGA, oxGA),
    names_glue  = '{.value}_{seasonType}_{strengthCategory}'
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
    CorsiA    = 1L,
    FenwickA  = base::as.integer(isFenwick),
    SOGA      = base::as.integer(isSOG),
    GA        = base::as.integer(isGoalLogical),
    xGA       = xG
  ) %>%
  dplyr::group_by(playerId, seasonType) %>%
  dplyr::summarise(
    x = safe_mean(xCoordNorm),
    y = safe_mean(yCoordNorm),
    dplyr::across(
      dplyr::all_of(c('CorsiA', 'FenwickA', 'SOGA', 'GA', 'xGA')),
      ~ base::sum(.x, na.rm = TRUE)
    ),
    .groups = 'drop'
  ) %>%
  tidyr::pivot_wider(
    names_from  = seasonType,
    values_from = c(x, y, CorsiA, FenwickA, SOGA, GA, xGA),
    names_glue  = '{.value}_{seasonType}_all'
  )

# Scrape supplemental data.
skater_season_toi_2 <- safe_skater_timeonice(SEASON, 2)
skater_season_toi_3 <- safe_skater_timeonice(SEASON, 3)
goalie_season_summary_2 <- safe_goalie_summary(SEASON, 2)
goalie_season_summary_3 <- safe_goalie_summary(SEASON, 3)
rm(
  safe_skater_timeonice,
  safe_goalie_summary,
  pull_numeric_or_na
)

# Merge skater data.frames.
skater_shot_analysis <- base::list(
  skater_shots,
  skater_season_toi_2,
  skater_season_toi_3
) %>%
  purrr::reduce(dplyr::full_join, by = 'playerId') %>%
  dplyr::filter(playerId %in% base::union(
    skater_season_toi_2$playerId,
    skater_season_toi_3$playerId
  )) %>%
  dplyr::mutate(
    dplyr::across(!dplyr::matches('^(x|y)_[23]_(ev|pp|sh|all)$'), ~ tidyr::replace_na(.x, 0))
  )
rm(skater_shots, skater_season_toi_2, skater_season_toi_3)

# Re-order columns.
i_prefix <- c('iCorsiF', 'iFenwickF', 'iSOGF', 'iGF', 'ixGF')
of_prefix <- c('oCorsiF', 'oFenwickF', 'oSOGF', 'oGF', 'oxGF')
oa_prefix <- c('oCorsiA', 'oFenwickA', 'oSOGA', 'oGA', 'oxGA')
skater_keep_cols <- c(
  'playerId',
  'mP_2_ev',
  'mP_2_pp',
  'mP_2_sh',
  'mP_2_all',
  'x_2_ev',
  'x_2_pp',
  'x_2_sh',
  'x_2_all',
  'y_2_ev',
  'y_2_pp',
  'y_2_sh',
  'y_2_all',
  make_strength_cols(i_prefix,  '_2'),
  make_strength_cols(of_prefix, '_2'),
  make_strength_cols(oa_prefix, '_2'),
  'mP_3_ev',
  'mP_3_pp',
  'mP_3_sh',
  'mP_3_all',
  'x_3_ev',
  'x_3_pp',
  'x_3_sh',
  'x_3_all',
  'y_3_ev',
  'y_3_pp',
  'y_3_sh',
  'y_3_all',
  make_strength_cols(i_prefix,  '_3'),
  make_strength_cols(of_prefix, '_3'),
  make_strength_cols(oa_prefix, '_3')
)
skater_shot_analysis <- skater_shot_analysis %>%
  ensure_columns(skater_keep_cols) %>%
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
    dplyr::across(!dplyr::matches('^(x|y)_[23]_(ev|pp|sh|all)$'), ~ tidyr::replace_na(.x, 0))
  )
rm(goalie_shots, goalie_season_summary_2, goalie_season_summary_3)
goalie_keep_cols <- c(
  'playerId',
  'mP_2_all',
  'x_2_all',
  'y_2_all',
  'CorsiA_2_all',
  'FenwickA_2_all',
  'SOGA_2_all',
  'GA_2_all',
  'xGA_2_all',
  'mP_3_all',
  'x_3_all',
  'y_3_all',
  'CorsiA_3_all',
  'FenwickA_3_all',
  'SOGA_3_all',
  'GA_3_all',
  'xGA_3_all'
)
goalie_shot_analysis <- goalie_shot_analysis %>%
  ensure_columns(goalie_keep_cols) %>%
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
rm(
  PLAYOFFS_PRESENT,
  na_playoff_cols_if_absent,
  EMPTY_PATH,
  EMPTY_VERSION,
  goalie_keep_cols,
  i_prefix,
  oa_prefix,
  of_prefix,
  SHOOTOUT_PATH,
  SHOOTOUT_VERSION,
  skater_keep_cols,
  SPECIAL_PATH,
  SPECIAL_VERSION,
  STANDARD_PATH,
  STANDARD_VERSION,
  ensure_columns,
  expand_strength_categories,
  flip_strength_code,
  load_model_bundle,
  make_strength_cols,
  normalize_strength_state,
  predict_xg_bundle,
  safe_mean,
  `%||%`
)
