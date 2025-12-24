# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(xgboost))

# Set seed.
set.seed(20060527)

# Read from CSV.
shots <- read_csv('models/xG/data/shots.csv', show_col_types = FALSE)

# Split into training and testing set.
shots_train <- shots %>% 
  filter(seasonId < 20242025 & typeDescKey != 'blocked-shot')
shots_test  <- shots %>% 
  filter(seasonId == 20242025)
rm(shots)

# Define features.
drop_cols <- c(
  'eventOwnerTeamId', 
  'shootingPlayerId', 
  'goalieInNetId', 
  'typeDescKey',
  'seasonId'
)
feature_cols <- setdiff(colnames(shots_train), c(drop_cols, 'isGoal'))

# Prepare train/test matrices.
X_train_df <- shots_train %>%
  select(all_of(feature_cols)) %>%
  mutate(across(where(is.logical), as.integer))
X_test_df  <- shots_test %>%
  select(all_of(feature_cols)) %>%
  mutate(across(where(is.logical), as.integer))
X_train_mat <- model.matrix(~ . - 1, data = X_train_df)
X_test_mat  <- model.matrix(~ . - 1, data = X_test_df)
missing_in_test <- setdiff(colnames(X_train_mat), colnames(X_test_mat))
if (length(missing_in_test) > 0) {
  X_test_mat <- cbind(X_test_mat, matrix(
    0, 
    nrow(X_test_mat), 
    length(missing_in_test), 
    dimnames = list(NULL, missing_in_test)
  ))
}
X_test_mat <- X_test_mat[, colnames(X_train_mat), drop = FALSE]
y_train <- as.integer(shots_train$isGoal)

# Prepare stratified validation split.
idx_train <- shots_train %>%
  mutate(row_id = row_number()) %>%
  group_by(isGoal) %>%
  slice_sample(prop = 0.8) %>%
  pull(row_id)
dtrain <- xgb.DMatrix(
  data = X_train_mat[idx_train, , drop = FALSE],
  label = y_train[idx_train]
)
dvalid <- xgb.DMatrix(
  data = X_train_mat[-idx_train, , drop = FALSE],
  label = y_train[-idx_train]
)
dtest  <- xgb.DMatrix(data = X_test_mat)

# Fit boosted tree with early stopping.
params <- list(
  objective        = 'binary:logistic',
  eval_metric      = 'logloss',
  eta              = 0.05,
  max_depth        = 5,
  min_child_weight = 1,
  subsample        = 0.8,
  colsample_bytree = 0.8
)

xgb_fit <- xgb.train(
  params  = params,
  data    = dtrain,
  nrounds = 5000,
  evals   = list(train = dtrain, valid = dvalid),
  early_stopping_rounds = 50,
  verbose = 1
)

# Rank importance.
importance <- xgb.importance(
  feature_names = colnames(X_train_mat),
  model         = xgb_fit
)

# Predict xG.
preds      <- predict(xgb_fit, dtest)
shots_test <- shots_test %>%
  mutate(xG = if_else(typeDescKey == 'blocked-shot', 0, preds))
rm(
  importance,
  params, 
  shots_train, 
  X_test_df, 
  X_train_df, 
  X_test_mat,
  X_train_mat,
  xgb_fit,
  drop_cols, 
  feature_cols, 
  dtrain,
  dtest,
  dvalid,
  y_train,
  missing_in_test,
  idx_train,
  preds
)

# Calculate shooter xGF.
skaters <- shots_test %>%
  mutate(
    playerId     = shootingPlayerId,
    is20242025_2 = seasonId == 20242025 & !isPlayoff,
    is20242025_3 = seasonId == 20242025 & isPlayoff,
    isSOG        = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick    = typeDescKey != 'blocked-shot'
  ) %>% 
  group_by(playerId) %>%
  summarise(
    iSOGF_20242025_2     = sum(
      if_else((is20242025_2 & isSOG), 1L, 0L), 
      na.rm = TRUE
    ),
    iFenwickF_20242025_2 = sum(
      if_else((is20242025_2 & isFenwick), 1L, 0L), 
      na.rm = TRUE
    ),
    iCorsiF_20242025_2   = sum(
      if_else((is20242025_2), 1L, 0L), 
      na.rm = TRUE
    ),
    ixGF_20242025_2      = sum(
      if_else((is20242025_2), xG, 0), 
      na.rm = TRUE
    ),
    iSOGF_20242025_3     = sum(
      if_else((is20242025_2 & isSOG), 1L, 0L), 
      na.rm = TRUE
    ),
    iFenwickF_20242025_3 = sum(
      if_else((is20242025_3 & isFenwick), 1L, 0L), 
      na.rm = TRUE
    ),
    iCorsiF_20242025_3   = sum(
      if_else((is20242025_3), 1L, 0L), 
      na.rm = TRUE
    ),
    ixGF_20242025_3      = sum(
      if_else((is20242025_3), xG, 0), 
      na.rm = TRUE
    ),
    .groups = 'drop'
  )

# Calculate goalie xGA.
goalies <- shots_test %>%
  filter(!is.na(goalieInNetId)) %>% 
  mutate(
    playerId     = goalieInNetId,
    is20242025_2 = seasonId == 20242025 & !isPlayoff,
    is20242025_3 = seasonId == 20242025 & isPlayoff,
    isSOG        = typeDescKey %in% c('goal', 'shot-on-goal'),
    isFenwick    = typeDescKey != 'blocked-shot'
  ) %>% 
  group_by(playerId) %>%
  summarise(
    iSOGA_20242025_2     = sum(
      if_else((is20242025_2 & isSOG), 1L, 0L), 
      na.rm = TRUE
    ),
    iFenwickA_20242025_2 = sum(
      if_else((is20242025_2 & isFenwick), 1L, 0L), 
      na.rm = TRUE
    ),
    ixGA_20242025_2      = sum(
      if_else((is20242025_2), xG, 0), 
      na.rm = TRUE
    ),
    iSOGA_20242025_3     = sum(
      if_else((is20242025_2 & isSOG), 1L, 0L), 
      na.rm = TRUE
    ),
    iFenwickA_20242025_3 = sum(
      if_else((is20242025_3 & isFenwick), 1L, 0L), 
      na.rm = TRUE
    ),
    ixGA_20242025_3      = sum(
      if_else((is20242025_3), xG, 0), 
      na.rm = TRUE
    ),
    .groups = 'drop'
  )
rm(shots_test)

# Write to CSV.
write_csv(skaters, 'models/xG/data/skaters.csv')
write_csv(goalies, 'models/xG/data/goalies.csv')
