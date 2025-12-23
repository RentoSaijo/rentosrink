# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Load data.
GC_PBPS_20212022 <- nhlscraper::gc_pbps(20212022)
GC_PBPS_20222023 <- nhlscraper::gc_pbps(20222023)
GC_PBPS_20232024 <- nhlscraper::gc_pbps(20232024)
GC_PBPS_20242025 <- nhlscraper::gc_pbps(20242025)

# Merge data.
common_cols <- Reduce(intersect, list(
  names(GC_PBPS_20212022),
  names(GC_PBPS_20222023),
  names(GC_PBPS_20232024),
  names(GC_PBPS_20242025)
))
pbps <- bind_rows(
  GC_PBPS_20212022 %>% select(all_of(common_cols)),
  GC_PBPS_20222023 %>% select(all_of(common_cols)),
  GC_PBPS_20232024 %>% select(all_of(common_cols)),
  GC_PBPS_20242025 %>% select(all_of(common_cols))
)
rm(
  common_cols, 
  GC_PBPS_20212022, 
  GC_PBPS_20222023, 
  GC_PBPS_20232024, 
  GC_PBPS_20242025
)

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

shots <- pbps %>% 
  filter(
    typeDescKey %in% c(
    'goal', 
    'shots-on-goal', 
    'missed-shot', 
    'blocked-shot'
    ),
    !(situationCode %in% c('0101', '1010')),
    !is.na(distance)
  ) %>% 
  mutate(
    shootingPlayerId = coalesce(shootingPlayerId, scoringPlayerId),
    isGoal           = typeDescKey == 'goal',
    isPlayoff        = gameTypeId  == 3
  ) %>% 
  select(
    eventOwnerTeamId,
    shootingPlayerId,
    goalieInNetId,
    typeDescKey,
    seasonId,
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
    isGoal
  )

# Temp fix: seasonId to 8 digits.
shots <- shots %>% 
  mutate(
    seasonId = (seasonId %/% 1e5) * 1e4 + (seasonId %% 1e4)
  )

# Write to CSV.
write_csv(shots, 'models/xG/data/shots.csv')
