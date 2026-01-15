# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(stringr))
suppressMessages(library(data.table))
suppressMessages(library(nhlscraper))

# Define constant.
SEASON <- 20252026

# Define helpers.
load_shifts <- function(season) {
  tryCatch(
    expr = {
      utils::read.csv(paste0(
        'https://media.githubusercontent.com/media/RentoSaijo/NHL_DB/refs/',
        'heads/main/data/game/shifts/NHL_SHIFTS_',
        season,
        '.csv'
      )) %>% 
        filter(is.na(eventDescription)) %>% 
        select(
          gameId, 
          period, 
          timeInPeriod = startTime, 
          endTime, 
          playerId, 
          eventOwnerTeamId = teamId
        ) %>% 
        flag_is_home() %>% 
        strip_time_period() %>% 
        mutate(
          startSecondsElapsedInPeriod = secondsElapsedInPeriod,
          timeInPeriod                = endTime
        ) %>% 
        select(
          -eventOwnerTeamId,
          -secondsElapsedInPeriod, 
          -secondsElapsedInGame,
          -endTime
        ) %>% 
        strip_time_period() %>% 
        mutate(endSecondsElapsedInPeriod = secondsElapsedInPeriod) %>% 
        select(-secondsElapsedInPeriod, -secondsElapsedInGame, -timeInPeriod)
    },
    error = function(e) {
      message('Invalid argument(s); refer to help file.')
      data.frame()
    }
  )
}

safe_skater_summary <- function(season, game_type) {
  out <- tryCatch(
    nhlscraper::skater_season_report(
      season    = season,
      game_type = game_type,
      category  = 'summary'
    ),
    error = function(e) tibble()
  )
  if (nrow(out) == 0) {
    return(tibble(
      playerId = integer(),
      !!paste0('gamesPlayed_', game_type) := integer(),
      !!paste0('minsPlayed_',  game_type) := double()
    ))
  }
  out %>%
    mutate(minsPlayed = timeOnIcePerGame * gamesPlayed / 60) %>%
    select(
      playerId,
      !!paste0('gamesPlayed_', game_type) := gamesPlayed,
      !!paste0('minsPlayed_',  game_type) := minsPlayed
    )
}

safe_goalie_summary <- function(season, game_type) {
  out <- tryCatch(
    nhlscraper::goalie_season_report(
      season    = season,
      game_type = game_type,
      category  = 'summary'
    ),
    error = function(e) tibble()
  )
  if (nrow(out) == 0) {
    return(tibble(
      playerId = integer(),
      !!paste0('gamesPlayed_', game_type) := integer(),
      !!paste0('minsPlayed_',  game_type) := double()
    ))
  }
  out %>%
    mutate(minsPlayed = timeOnIce / 60) %>%
    select(
      playerId,
      !!paste0('gamesPlayed_', game_type) := gamesPlayed,
      !!paste0('minsPlayed_',  game_type) := minsPlayed
    )
}

na_playoff_cols_if_absent <- function(df, playoffs_present) {
  if (isTRUE(playoffs_present)) return(df)
  df %>%
    mutate(
      across(
        matches('(_3$|_3_per82$|_3_per60$|_3_pct$)'),
        ~ NA_real_
      )
    )
}

infer_home_def_side_legacy <- function(pbp) {
  pbp %>%
    group_by(gameId) %>%
    group_modify(function(df, key) {
      if ('homeTeamDefendingSide' %in% names(df) && any(!is.na(df$homeTeamDefendingSide))) {
        return(df)
      }
      ref <- df %>%
        filter(!is.na(xCoord), zoneCode == 'O', !is.na(isHome)) %>%
        arrange(period, secondsElapsedInPeriod) %>%
        slice(1)
      if (nrow(ref) == 0) {
        df$homeTeamDefendingSide <- NA_character_
        return(df)
      }
      ref_period <- ref$period[1]
      x          <- ref$xCoord[1]
      ref_isHome <- ref$isHome[1]
      home_attacks_right_in_ref_period <- (ref_isHome && x > 0) || (!ref_isHome && x < 0)
      home_def_left_in_ref_period <- home_attacks_right_in_ref_period
      df %>%
        mutate(
          homeTeamDefendingSide = if_else(
            ((period - ref_period) %% 2) == 0,
            if_else(home_def_left_in_ref_period, 'left', 'right'),
            if_else(!home_def_left_in_ref_period, 'left', 'right')
          )
        )
    }) %>%
    ungroup()
}

# Load data.
pbps <- nhlscraper::gc_pbps(SEASON)

# Clean data.
pbps <- pbps %>% 
  nhlscraper::flag_is_home() %>% 
  nhlscraper::strip_game_id() %>% 
  filter(gameTypeId %in% 2:3) %>% 
  nhlscraper::strip_time_period() %>% 
  nhlscraper::strip_situation_code() %>% 
  nhlscraper::flag_is_rebound() %>% 
  nhlscraper::flag_is_rush() %>% 
  nhlscraper::count_goals_shots() %>%
  infer_home_def_side_legacy() %>% 
  nhlscraper::normalize_coordinates() %>% 
  nhlscraper::calculate_distance() %>% 
  nhlscraper::calculate_angle()

# Create testing set.
shots <- pbps %>% 
  filter(
    # Keep only shots.
    typeDescKey %in% c(
      'goal', 
      'shot-on-goal', 
      'missed-shot',
      'blocked-shot'
    ),
    # Remove shootouts.
    !(gameTypeId == 2 & period == 5)
  ) %>% 
  mutate(
    shootingPlayerId = coalesce(shootingPlayerId, scoringPlayerId),
    shotType         = replace_na(shotType, 'wrist'),
    shotType         = factor(shotType),
    isPlayoff        = gameTypeId == 3,
    isGoal           = typeDescKey == 'goal',
    isGoal           = factor(
      isGoal, 
      levels = c(FALSE, TRUE), 
      labels = c('no', 'yes')
    )
  ) %>% 
  select(
    # IDs
    gameId,
    eventId,
    eventOwnerTeamId,
    shootingPlayerId,
    goalieInNetId,
    typeDescKey,
    # Predictors
    isHome,
    isPlayoff,
    period,
    secondsElapsedInPeriod,
    isEmptyNetAgainst,
    skaterCountFor,
    skaterCountAgainst,
    isRebound,
    isRush,
    goalsFor,
    goalsAgainst,
    distance,
    angle,
    shotType,
    # Response
    isGoal
  )
rm(pbps)

# Detect playoffs.
PLAYOFFS_PRESENT <- any(shots$isPlayoff, na.rm = TRUE)

# Load model.
model <- readRDS('models/xG/model1.rds')

# Predict xG.
shots_score <- shots %>%
  filter(typeDescKey != 'blocked-shot')
shots_block <- shots %>%
  filter(typeDescKey == 'blocked-shot')
probs <- predict(model, shots_score, type = 'prob')
shots_score <- shots_score %>%
  mutate(xG = probs$.pred_yes)
shots_block <- shots_block %>%
  mutate(xG = 0)
shots <- bind_rows(shots_score, shots_block) %>%
  arrange(gameId, period, secondsElapsedInPeriod)
rm(model, shots_score, shots_block, probs)

# Merge shots and shifts.
shifts <- load_shifts(SEASON)
shots  <- shots %>%
  mutate(rowId = row_number())
shifts_dt <- as.data.table(shifts) %>%
  .[, `:=`(start = startSecondsElapsedInPeriod,
           end   = endSecondsElapsedInPeriod)] %>%
  .[!is.na(start)] %>%
  .[is.na(end), end := start] %>%
  .[end < start, end := start] %>%
  .[end > start] %>%
  .[, start_adj := ifelse(start == 0, 0, start + 1e-6)] %>%  # (start, end]
  .[, .(gameId, period, playerId, isHome, start = start_adj, end)]
shots_dt <- as.data.table(shots) %>%
  .[, `:=`(
    t0 = secondsElapsedInPeriod,
    t1 = secondsElapsedInPeriod,
    shotIsHome = isHome
  )] %>%
  .[, .(rowId, gameId, period, eventId, shotIsHome, t0, t1)]
setkey(shifts_dt, gameId, period, start, end)
setkey(shots_dt,  gameId, period, t0, t1)
onice <- foverlaps(
  shots_dt,
  shifts_dt,
  by.x = c('gameId', 'period', 't0', 't1'),
  by.y = c('gameId', 'period', 'start', 'end'),
  type = 'within',
  nomatch = 0L
)
onice_lists <- onice[
  ,
  .(
    playerIdsFor     = list(unique(playerId[isHome == shotIsHome])),
    playerIdsAgainst = list(unique(playerId[isHome != shotIsHome]))
  ),
  by = .(rowId)
]
shots <- shots %>%
  left_join(as_tibble(onice_lists), by = 'rowId') %>%
  mutate(
    playerIdsFor = map2(
      playerIdsFor, shootingPlayerId,
      \(ids, shooter) sort(unique(c(ids %||% integer(), shooter)))
    ),
    playerIdsAgainst = map2(
      playerIdsAgainst, goalieInNetId,
      \(ids, goalie) {
        if (is.na(goalie)) ids else sort(unique(c(ids %||% integer(), goalie)))
      }
    ),
    playerIdsFor = if_else(
      skaterCountFor == 1L,
      map(shootingPlayerId, \(x) if (is.na(x)) integer() else as.integer(x)),
      playerIdsFor
    ),
    playerIdsAgainst = if_else(
      skaterCountFor == 1L,
      map(goalieInNetId, \(x) if (is.na(x)) integer() else as.integer(x)),
      playerIdsAgainst
    )
  ) %>%
  select(-rowId)
rm(shifts, shifts_dt, shots_dt, onice, onice_lists)

# Calculate skater shot metrics.
skater_shots <- shots %>%
  mutate(
    playerId  = shootingPlayerId,
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot'
  ) %>% 
  group_by(playerId) %>%
  summarise(
    iCorsiF_2 = sum(if_else(!isPlayoff, 1L, 0L), na.rm = TRUE),
    iCorsiF_3 = sum(if_else(isPlayoff,  1L, 0L), na.rm = TRUE),
    iFenwickF_2 = sum(if_else(isFenwick & !isPlayoff, 1L, 0L), na.rm = TRUE),
    iFenwickF_3 = sum(if_else(isFenwick &  isPlayoff, 1L, 0L), na.rm = TRUE),
    iSOGF_2 = sum(if_else(isSOG & !isPlayoff, 1L, 0L), na.rm = TRUE),
    iSOGF_3 = sum(if_else(isSOG &  isPlayoff, 1L, 0L), na.rm = TRUE),
    iGF_2 = sum(if_else(isGoal == 'yes' & !isPlayoff, 1L, 0L), na.rm = TRUE),
    iGF_3 = sum(if_else(isGoal == 'yes' &  isPlayoff, 1L, 0L), na.rm = TRUE),
    ixGF_2 = sum(if_else(!isPlayoff, xG, 0), na.rm = TRUE),
    ixGF_3 = sum(if_else( isPlayoff, xG, 0), na.rm = TRUE),
    iGFaX_2 = iGF_2 - ixGF_2,
    iGFaX_3 = iGF_3 - ixGF_3,
    d_2 = if_else(
      sum(!isPlayoff & !is.na(distance)) > 0,
      mean(distance[!isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    d_3 = if_else(
      sum(isPlayoff & !is.na(distance)) > 0,
      mean(distance[isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    a_2 = if_else(
      sum(!isPlayoff & !is.na(angle)) > 0,
      mean(angle[!isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    a_3 = if_else(
      sum(isPlayoff & !is.na(angle)) > 0,
      mean(angle[isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    .groups = 'drop'
  )
skater_onice <- shots %>%
  mutate(
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot'
  ) %>%
  filter(!is.na(playerIdsFor)) %>%
  unnest_longer(playerIdsFor, values_to = 'playerId') %>%
  group_by(playerId) %>%
  summarise(
    oCorsiF_2 = sum(if_else(!isPlayoff, 1L, 0L), na.rm = TRUE),
    oCorsiF_3 = sum(if_else( isPlayoff, 1L, 0L), na.rm = TRUE),
    oFenwickF_2 = sum(if_else(isFenwick & !isPlayoff, 1L, 0L), na.rm = TRUE),
    oFenwickF_3 = sum(if_else(isFenwick &  isPlayoff, 1L, 0L), na.rm = TRUE),
    oSOGF_2 = sum(if_else(isSOG & !isPlayoff, 1L, 0L), na.rm = TRUE),
    oSOGF_3 = sum(if_else(isSOG &  isPlayoff, 1L, 0L), na.rm = TRUE),
    oGF_2 = sum(if_else(isGoal == 'yes' & !isPlayoff, 1L, 0L), na.rm = TRUE),
    oGF_3 = sum(if_else(isGoal == 'yes' &  isPlayoff, 1L, 0L), na.rm = TRUE),
    oxGF_2 = sum(if_else(!isPlayoff, xG, 0), na.rm = TRUE),
    oxGF_3 = sum(if_else( isPlayoff, xG, 0), na.rm = TRUE),
    oGFaX_2 = oGF_2 - oxGF_2,
    oGFaX_3 = oGF_3 - oxGF_3,
    .groups = 'drop'
  )
goalie_ids <- shots %>%
  distinct(goalieInNetId) %>%
  filter(!is.na(goalieInNetId)) %>%
  pull(goalieInNetId)
skater_onice_again <- shots %>%
  mutate(
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot'
  ) %>%
  filter(!is.na(playerIdsAgainst)) %>%
  unnest_longer(playerIdsAgainst, values_to = 'playerId') %>%
  filter(!playerId %in% goalie_ids) %>%
  group_by(playerId) %>%
  summarise(
    oCorsiA_2 = sum(if_else(!isPlayoff, 1L, 0L), na.rm = TRUE),
    oCorsiA_3 = sum(if_else( isPlayoff, 1L, 0L), na.rm = TRUE),
    oFenwickA_2 = sum(if_else(isFenwick & !isPlayoff, 1L, 0L), na.rm = TRUE),
    oFenwickA_3 = sum(if_else(isFenwick &  isPlayoff, 1L, 0L), na.rm = TRUE),
    oSOGA_2 = sum(if_else(isSOG & !isPlayoff, 1L, 0L), na.rm = TRUE),
    oSOGA_3 = sum(if_else(isSOG &  isPlayoff, 1L, 0L), na.rm = TRUE),
    oGA_2 = sum(if_else(isGoal == 'yes' & !isPlayoff, 1L, 0L), na.rm = TRUE),
    oGA_3 = sum(if_else(isGoal == 'yes' &  isPlayoff, 1L, 0L), na.rm = TRUE),
    oxGA_2 = sum(if_else(!isPlayoff, xG, 0), na.rm = TRUE),
    oxGA_3 = sum(if_else( isPlayoff, xG, 0), na.rm = TRUE),
    oGAaX_2 = oGA_2 - oxGA_2,
    oGAaX_3 = oGA_3 - oxGA_3,
    .groups = 'drop'
  )
skater_shots <- skater_shots %>%
  full_join(skater_onice,       by = 'playerId') %>%
  full_join(skater_onice_again, by = 'playerId')
rm(skater_onice, skater_onice_again, goalie_ids)

# Calculate goalie shot metrics.
goalie_shots <- shots %>%
  filter(!is.na(goalieInNetId)) %>% 
  mutate(
    playerId  = goalieInNetId,
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot'
  ) %>% 
  group_by(playerId) %>%
  summarise(
    FenwickA_2 = sum(if_else(isFenwick & !isPlayoff, 1L, 0L), na.rm = TRUE),
    FenwickA_3 = sum(if_else(isFenwick &  isPlayoff, 1L, 0L), na.rm = TRUE),
    SOGA_2 = sum(if_else(isSOG & !isPlayoff, 1L, 0L), na.rm = TRUE),
    SOGA_3 = sum(if_else(isSOG &  isPlayoff, 1L, 0L), na.rm = TRUE),
    GA_2 = sum(if_else(isGoal == 'yes' & !isPlayoff, 1L, 0L), na.rm = TRUE),
    GA_3 = sum(if_else(isGoal == 'yes' &  isPlayoff, 1L, 0L), na.rm = TRUE),
    xGA_2 = sum(if_else(!isPlayoff, xG, 0), na.rm = TRUE),
    xGA_3 = sum(if_else( isPlayoff, xG, 0), na.rm = TRUE),
    GSaX_2 = xGA_2 - GA_2,
    GSaX_3 = xGA_3 - GA_3,
    d_2 = if_else(
      sum(!isPlayoff & !is.na(distance)) > 0,
      mean(distance[!isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    d_3 = if_else(
      sum(isPlayoff & !is.na(distance)) > 0,
      mean(distance[isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    a_2 = if_else(
      sum(!isPlayoff & !is.na(angle)) > 0,
      mean(angle[!isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    a_3 = if_else(
      sum(isPlayoff & !is.na(angle)) > 0,
      mean(angle[isPlayoff], na.rm = TRUE),
      NA_real_
    ),
    .groups = 'drop'
  )
goalie_corsi <- shots %>%
  filter(!is.na(playerIdsAgainst)) %>%
  unnest_longer(playerIdsAgainst, values_to = 'playerId') %>%
  group_by(playerId) %>%
  summarise(
    CorsiA_2 = sum(if_else(!isPlayoff, 1L, 0L), na.rm = TRUE),
    CorsiA_3 = sum(if_else( isPlayoff, 1L, 0L), na.rm = TRUE),
    .groups = 'drop'
  )
goalie_shots <- goalie_shots %>%
  left_join(goalie_corsi, by = 'playerId')
rm(goalie_corsi)

# Scrape supplemental data.
skater_season_summary_2 <- safe_skater_summary(SEASON, 2)
skater_season_summary_3 <- safe_skater_summary(SEASON, 3)
goalie_season_summary_2 <- safe_goalie_summary(SEASON, 2)
goalie_season_summary_3 <- safe_goalie_summary(SEASON, 3)
rm(safe_skater_summary, safe_goalie_summary)
season <- nhlscraper::seasons() %>% 
  filter(id == SEASON)

# Merge skater data.frames.
skater_shot_analysis <- list(
  skater_shots,
  skater_season_summary_2,
  skater_season_summary_3
) %>%
  reduce(full_join, by = 'playerId') %>%
  filter(playerId %in% union(
    skater_season_summary_2$playerId,
    skater_season_summary_3$playerId
  ))
rm(skater_shots, skater_season_summary_2, skater_season_summary_3)

# Merge goalie data.frames.
goalie_shot_analysis <- list(
  goalie_shots,
  goalie_season_summary_2,
  goalie_season_summary_3
) %>%
  reduce(full_join, by = 'playerId') %>%
  filter(playerId %in% union(
    goalie_season_summary_2$playerId,
    goalie_season_summary_3$playerId
  ))
rm(goalie_shots, goalie_season_summary_2, goalie_season_summary_3)

# Calculate skater pace metrics.
metric_cols_2 <- names(skater_shot_analysis) %>%
  str_subset('(^[io].*[FA]_2)|(^[io]x?G[FA]_2)|(^[io]G[FA]aX_2)')
metric_cols_3 <- names(skater_shot_analysis) %>%
  str_subset('(^[io].*[FA]_3)|(^[io]x?G[FA]_3)|(^[io]G[FA]aX_3)')
skater_shot_analysis <- skater_shot_analysis %>%
  mutate(
    across(
      all_of(metric_cols_2),
      \(x) if_else(gamesPlayed_2 > 0, as.numeric(x) / gamesPlayed_2 * 82, NA_real_),
      .names = '{.col}_per82'
    ),
    across(
      all_of(metric_cols_3),
      \(x) if_else(gamesPlayed_3 > 0, as.numeric(x) / gamesPlayed_3 * 82, NA_real_),
      .names = '{.col}_per82'
    ),
    across(
      all_of(metric_cols_2),
      \(x) if_else(minsPlayed_2 > 0, as.numeric(x) / minsPlayed_2 * 60, NA_real_),
      .names = '{.col}_per60'
    ),
    across(
      all_of(metric_cols_3),
      \(x) if_else(minsPlayed_3 > 0, as.numeric(x) / minsPlayed_3 * 60, NA_real_),
      .names = '{.col}_per60'
    ),
    across(!matches('^(d|a)_[23]$'), ~replace_na(.x, 0))
  )
rm(metric_cols_2, metric_cols_3)

# Calculate goalie pace metrics.
metric_cols_2 <- names(goalie_shot_analysis) %>%
  str_subset('_2$') %>%
  setdiff(c('gamesPlayed_2', 'minsPlayed_2', 'd_2', 'a_2'))
metric_cols_3 <- names(goalie_shot_analysis) %>%
  str_subset('_3$') %>%
  setdiff(c('gamesPlayed_3', 'minsPlayed_3', 'd_3', 'a_3'))
goalie_shot_analysis <- goalie_shot_analysis %>%
  mutate(
    across(
      all_of(metric_cols_2),
      \(x) if_else(gamesPlayed_2 > 0, as.numeric(x) / gamesPlayed_2 * 82, NA_real_),
      .names = '{.col}_per82'
    ),
    across(
      all_of(metric_cols_3),
      \(x) if_else(gamesPlayed_3 > 0, as.numeric(x) / gamesPlayed_3 * 82, NA_real_),
      .names = '{.col}_per82'
    ),
    across(
      all_of(metric_cols_2),
      \(x) if_else(minsPlayed_2 > 0, as.numeric(x) / minsPlayed_2 * 60, NA_real_),
      .names = '{.col}_per60'
    ),
    across(
      all_of(metric_cols_3),
      \(x) if_else(minsPlayed_3 > 0, as.numeric(x) / minsPlayed_3 * 60, NA_real_),
      .names = '{.col}_per60'
    ),
    across(!matches('^(d|a)_[23]$'), ~replace_na(.x, 0))
  )
rm(metric_cols_2, metric_cols_3)

# Calculate skater percentiles (NA if below threshold).
skater_min_games_2 <- season$minimumRegularGamesForGoalieStatsLeaders
skater_min_mins_3  <- season$minimumPlayoffMinutesForGoalieStatsLeaders / 5
metric_cols_2 <- names(skater_shot_analysis) %>% 
  str_subset('(^[io].*F_2)|(^[io]x?GF_2)|(^[io]GFaX_2)')
metric_cols_3 <- names(skater_shot_analysis) %>% 
  str_subset('(^[io].*F_3)|(^[io]x?GF_3)|(^[io]GFaX_3)')
skater_shot_analysis <- skater_shot_analysis %>%
  mutate(
    across(
      all_of(metric_cols_2),
      \(x) {
        x2 <- if_else(
          gamesPlayed_2 >= skater_min_games_2,
          as.numeric(x),
          NA_real_
        )
        percent_rank(x2) * 100
      },
      .names = '{.col}_pct'
    ),
    across(
      all_of(metric_cols_3),
      \(x) {
        x3 <- if_else(
          minsPlayed_3 >= skater_min_mins_3,
          as.numeric(x),
          NA_real_
        )
        percent_rank(x3) * 100
      },
      .names = '{.col}_pct'
    )
  )
rm(metric_cols_2, metric_cols_3, skater_min_games_2, skater_min_mins_3)

# Calculate goalie percentiles (NA if below threshold).
goalie_min_games_2 <- season$minimumRegularGamesForGoalieStatsLeaders
goalie_min_mins_3  <- season$minimumPlayoffMinutesForGoalieStatsLeaders
metric_cols_2 <- names(goalie_shot_analysis) %>%
  str_subset('_2') %>%
  setdiff(c('gamesPlayed_2', 'minsPlayed_2', 'd_2', 'a_2'))
metric_cols_3 <- names(goalie_shot_analysis) %>%
  str_subset('_3') %>%
  setdiff(c('gamesPlayed_3', 'minsPlayed_3', 'd_3', 'a_3'))
goalie_shot_analysis <- goalie_shot_analysis %>%
  mutate(
    across(
      all_of(metric_cols_2),
      \(x) {
        x2 <- if_else(
          gamesPlayed_2 >= goalie_min_games_2,
          as.numeric(x),
          NA_real_
        )
        percent_rank(x2) * 100
      },
      .names = '{.col}_pct'
    ),
    across(
      all_of(metric_cols_3),
      \(x) {
        x3 <- if_else(
          minsPlayed_3 >= goalie_min_mins_3,
          as.numeric(x),
          NA_real_
        )
        percent_rank(x3) * 100
      },
      .names = '{.col}_pct'
    )
  )
rm(metric_cols_2, metric_cols_3, goalie_min_games_2, goalie_min_mins_3, season)

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
write_csv(skater_shot_analysis, paste0(
  'data/skater_shot_analysis_', 
  SEASON,
  '.csv'
))
write_csv(goalie_shot_analysis, paste0(
  'data/goalie_shot_analysis_', 
  SEASON,
  '.csv'
))
rm(SEASON, PLAYOFFS_PRESENT, load_shifts, na_playoff_cols_if_absent)
