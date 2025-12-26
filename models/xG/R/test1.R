# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(stringr))
suppressMessages(library(nhlscraper))

# Define constant.
SEASON <- 20242025

# Load data.
pbps <- nhlscraper::gc_pbps(SEASON)

# Clean data.
pbps <- pbps %>% 
  nhlscraper::flag_is_home() %>% 
  nhlscraper::strip_game_id() %>% 
  nhlscraper::strip_time_period() %>% 
  nhlscraper::strip_situation_code() %>% 
  nhlscraper::flag_is_rebound() %>% 
  nhlscraper::flag_is_rush() %>% 
  nhlscraper::count_goals_shots() %>% 
  nhlscraper::normalize_coordinates() %>% 
  nhlscraper::calculate_distance() %>% 
  nhlscraper::calculate_angle()

# Create testing set.
shots <- pbps %>% 
  filter(
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
  mutate(
    # Combine shootingPlayerId and scoringPlayerId.
    shootingPlayerId = coalesce(shootingPlayerId, scoringPlayerId),
    # Make every shot have shotType.
    shotType         = replace_na(shotType, 'wrist'),
    shotType         = factor(shotType),
    # Flag playoff shot.
    isPlayoff        = gameTypeId == 3,
    # Flag goals.
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

# Load model.
model <- readRDS('models/xG/model1.rds')

# Predict xG.
shots_score <- shots %>% filter(typeDescKey != 'blocked-shot')
shots_block <- shots %>% filter(typeDescKey == 'blocked-shot')
probs       <- predict(model, shots_score, type = 'prob')
shots_score <- shots_score %>%
  mutate(xG = probs$.pred_yes)
shots_block <- shots_block %>%
  mutate(xG = 0)
shots       <- bind_rows(shots_score, shots_block) %>%
  arrange(gameId, period, secondsElapsedInPeriod)
rm(model, shots_score, shots_block, probs)

# Calculate skater shot metrics.
skater_shots <- shots %>%
  mutate(
    playerId  = shootingPlayerId,
    isSOG     = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick = typeDescKey != 'blocked-shot'
  ) %>% 
  group_by(playerId) %>%
  summarise(
    iCorsiF_2 = sum(
      if_else(!isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iCorsiF_3 = sum(
      if_else(isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iFenwickF_2 = sum(
      if_else(isFenwick & !isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iFenwickF_3 = sum(
      if_else(isFenwick & isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iSOGF_2 = sum(
      if_else(isSOG & !isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iSOGF_3 = sum(
      if_else(isSOG & isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iGF_2 = sum(
      if_else(isGoal == 'yes' & !isPlayoff, 1L, 0L), 
      na.rm = TRUE
    ),
    iGF_3 = sum(
      if_else(isGoal == 'yes' & isPlayoff, 1L, 0L), 
      na.rm = TRUE
    ),
    ixGF_2 = sum(
      if_else(!isPlayoff, xG, 0), 
      na.rm = TRUE
    ),
    ixGF_3 = sum(
      if_else(isPlayoff, xG, 0), 
      na.rm = TRUE
    ),
    iGFaX_2 = iGF_2 - ixGF_2,
    iGFaX_3 = iGF_3 - ixGF_3,
    .groups = 'drop'
  )

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
    iFenwickA_2 = sum(
      if_else(isFenwick & !isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iFenwickA_3 = sum(
      if_else(isFenwick & isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iSOGA_2 = sum(
      if_else(isSOG & !isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iSOGA_3 = sum(
      if_else(isSOG & isPlayoff, 1L, 0L),
      na.rm = TRUE
    ),
    iGA_2 = sum(
      if_else(isGoal == 'yes' & !isPlayoff, 1L, 0L), 
      na.rm = TRUE
    ),
    iGA_3 = sum(
      if_else(isGoal == 'yes' & isPlayoff, 1L, 0L), 
      na.rm = TRUE
    ),
    ixGA_2 = sum(
      if_else(!isPlayoff, xG, 0), 
      na.rm = TRUE
    ),
    ixGA_3 = sum(
      if_else(isPlayoff, xG, 0), 
      na.rm = TRUE
    ),
    iGSaX_2 = ixGA_2 - iGA_2,
    iGSaX_3 = ixGA_3 - iGA_3,
    .groups = 'drop'
  )

# Scrape supplemental data.
skater_season_summary_2 <- nhlscraper::skater_season_report(
  season    = SEASON,
  game_type = 2,
  category  = 'summary'
) %>% 
  mutate(
    minsPlayed = timeOnIcePerGame * gamesPlayed / 60
  ) %>% 
  select(
    playerId,
    gamesPlayed_2 = gamesPlayed,
    minsPlayed_2 = minsPlayed
  )
skater_season_summary_3 <- nhlscraper::skater_season_report(
  season    = SEASON,
  game_type = 3,
  category  = 'summary'
) %>% 
  mutate(
    minsPlayed = timeOnIcePerGame * gamesPlayed / 60
  ) %>% 
  select(
    playerId,
    gamesPlayed_3 = gamesPlayed,
    minsPlayed_3 = minsPlayed
  )
goalie_season_summary_2 <- nhlscraper::goalie_season_report(
  season    = SEASON,
  game_type = 2,
  category  = 'summary'
) %>% 
  mutate(minsPlayed = timeOnIce / 60) %>% 
  select(
    playerId,
    gamesPlayed_2 = gamesPlayed,
    minsPlayed_2 = minsPlayed,
  )
goalie_season_summary_3 <- nhlscraper::goalie_season_report(
  season    = SEASON,
  game_type = 3,
  category  = 'summary'
) %>% 
  mutate(minsPlayed = timeOnIce / 60) %>% 
  select(
    playerId,
    gamesPlayed_3 = gamesPlayed,
    minsPlayed_3 = minsPlayed,
  )
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
metric_cols_2 <- names(skater_shot_analysis) %>% str_subset('F.*_2$')
metric_cols_3 <- names(skater_shot_analysis) %>% str_subset('F.*_3$')
skater_shot_analysis <- skater_shot_analysis %>%
  mutate(
    across(
      all_of(metric_cols_2),
      \(x) if_else(gamesPlayed_2 > 0, x / gamesPlayed_2 * 82, NA_real_),
      .names = '{.col}_per82'
    ),
    across(
      all_of(metric_cols_3),
      \(x) if_else(gamesPlayed_3 > 0, x / gamesPlayed_3 * 82, NA_real_),
      .names = '{.col}_per82'
    ),
    across(
      all_of(metric_cols_2),
      \(x) if_else(minsPlayed_2 > 0, x / minsPlayed_2 * 60, NA_real_),
      .names = '{.col}_per60'
    ),
    across(
      all_of(metric_cols_3),
      \(x) if_else(minsPlayed_3 > 0, x / minsPlayed_3 * 60, NA_real_),
      .names = '{.col}_per60'
    ),
    across(everything(), ~replace_na(.x, 0))
  )
rm(metric_cols_2, metric_cols_3)

# Calculate goalie pace metrics.
metric_cols_2 <- names(goalie_shot_analysis) %>% str_subset('[AS].*_2$')
metric_cols_3 <- names(goalie_shot_analysis) %>% str_subset('[AS].*_3$')
goalie_shot_analysis <- goalie_shot_analysis %>%
  mutate(
    across(
      all_of(metric_cols_2),
      \(x) if_else(gamesPlayed_2 > 0, x / gamesPlayed_2 * 82, NA_real_),
      .names = '{.col}_per82'
    ),
    across(
      all_of(metric_cols_3),
      \(x) if_else(gamesPlayed_3 > 0, x / gamesPlayed_3 * 82, NA_real_),
      .names = '{.col}_per82'
    ),
    across(
      all_of(metric_cols_2),
      \(x) if_else(minsPlayed_2 > 0, x / minsPlayed_2 * 60, NA_real_),
      .names = '{.col}_per60'
    ),
    across(
      all_of(metric_cols_3),
      \(x) if_else(minsPlayed_3 > 0, x / minsPlayed_3 * 60, NA_real_),
      .names = '{.col}_per60'
    ),
    across(everything(), ~replace_na(.x, 0))
  )
rm(metric_cols_2, metric_cols_3)

# Calculate skater percentiles (NA if below threshold).
skater_min_games_2 <- season$minimumRegularGamesForGoalieStatsLeaders
skater_min_mins_3  <- season$minimumPlayoffMinutesForGoalieStatsLeaders / 5
metric_cols_2 <- names(skater_shot_analysis) %>% str_subset('F.*_2')
metric_cols_3 <- names(skater_shot_analysis) %>% str_subset('F.*_3')
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
metric_cols_2 <- names(goalie_shot_analysis) %>% str_subset('[AS].*_2$')
metric_cols_3 <- names(goalie_shot_analysis) %>% str_subset('[AS].*_3$')
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
