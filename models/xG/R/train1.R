# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(xgboost))
suppressMessages(library(nhlscraper))

# Set seed.
set.seed(20060527)

# Load data.
GC_PBPS_20212022 <- nhlscraper::gc_pbps(20212022)
GC_PBPS_20222023 <- nhlscraper::gc_pbps(20222023)
GC_PBPS_20232024 <- nhlscraper::gc_pbps(20232024)

# Merge data.
common_cols <- Reduce(intersect, list(
  names(GC_PBPS_20212022),
  names(GC_PBPS_20222023),
  names(GC_PBPS_20232024)
))
pbps <- bind_rows(
  GC_PBPS_20212022 %>% select(all_of(common_cols)),
  GC_PBPS_20222023 %>% select(all_of(common_cols)),
  GC_PBPS_20232024 %>% select(all_of(common_cols))
)
rm(
  common_cols, 
  GC_PBPS_20212022, 
  GC_PBPS_20222023, 
  GC_PBPS_20232024
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

# Create training set.
shots <- pbps %>% 
  filter(
    # Keep only regular season and playoffs.
    gameTypeId %in% 2:3,
    # Keep only shots.
    typeDescKey %in% c(
      'goal', 
      'shot-on-goal', 
      'missed-shot'
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

# Pre-process.
rec <- recipe(isGoal ~ ., data = shots) %>% 
  update_role(
    gameId, 
    eventId, 
    eventOwnerTeamId, 
    shootingPlayerId, 
    goalieInNetId, 
    typeDescKey,
    new_role = 'id'
  ) %>% 
  step_mutate_at(all_logical_predictors(), fn = ~ as.integer(.)) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors())

# Define XGBoost model.
xgb_spec <- boost_tree(
  mode        = 'classification',
  trees       = 1000,
  tree_depth  = 5,
  learn_rate  = 0.05,
  min_n       = 10,
  sample_size = 0.8
) %>% 
  set_engine('xgboost', eval_metric = 'logloss')

# Fit.
wf    <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(xgb_spec)
model <- fit(wf, data = shots)
rm(rec, wf, xgb_spec)

# See importance.
booster <- extract_fit_engine(model)
imp     <- xgb.importance(model = booster)
xgb.plot.importance(imp)
rm(booster, imp)

# Export to RDS.
saveRDS(model, file = 'models/xG/model1.rds')
