# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(xgboost))

# Set seed.
set.seed(20060527)

# Read from CSV.
shots <- read_csv('models/xG/data/shots.csv', show_col_types = FALSE)

# Split into training and testing set.
shots_train <- shots %>% 
  filter(seasonId < 20242025)
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
y_train <- as.integer(shots_train$isGoal)
dtrain  <- xgb.DMatrix(data = as.matrix(X_train_df), label = y_train)
dtest   <- xgb.DMatrix(data = as.matrix(X_test_df))

# Fit boosted tree.
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
  nrounds = 2000,
  verbose = 0
)

# Rank importance.
importance <- xgb.importance(
  feature_names = feature_cols,
  model         = xgb_fit
)

# Predict xG.
shots_test <- shots_test %>%
  mutate(xG = predict(xgb_fit, dtest))
rm(
  importance,
  params, 
  shots_train, 
  X_test_df, 
  X_train_df, 
  xgb_fit,
  drop_cols, 
  feature_cols, 
  dtrain,
  dtest,
  y_train
)

# Calculate shooter xGF.
shooters <- shots_test %>%
  group_by(shootingPlayerId) %>%
  summarise(xGF = sum(xG, na.rm = TRUE), .groups = 'drop')

# Calculate goalie xGA.
goalies <- shots_test %>%
  filter(!is.na(goalieInNetId)) %>% 
  group_by(goalieInNetId) %>%
  summarise(xGA = sum(xG, na.rm = TRUE), .groups = 'drop')
