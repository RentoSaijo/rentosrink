# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(Matrix))
suppressMessages(library(glue))
suppressMessages(library(lightgbm))
suppressMessages(library(yardstick))
suppressMessages(library(nhlscraper))

# Set seed. 
set.seed(20060527)

# Define constants.
SEASONS    <- c(20222023, 20232024, 20242025)
GRID_SIZE  <- 10

# ----- Helper ----- #

# Rolling-origin folds, but at the GAME level (prevents within-game leakage).
make_game_folds <- function(shots) {
  games <- shots %>%
    dplyr::distinct(gameId) %>%
    dplyr::arrange(gameId)
  ro <- rsample::rolling_origin(
    games,
    initial    = max(1L, floor(0.70 * nrow(games))),
    assess     = max(1L, floor(0.10 * nrow(games))),
    cumulative = TRUE,
    skip       = max(0L, floor(0.05 * nrow(games)))
  )
  splits <- purrr::map(ro$splits, \(sp) {
    g_analysis <- rsample::analysis(sp)$gameId
    g_assess   <- rsample::assessment(sp)$gameId
    idx_analysis <- which(shots$gameId %in% g_analysis)
    idx_assess   <- which(shots$gameId %in% g_assess)
    rsample::make_splits(
      list(analysis = idx_analysis, assessment = idx_assess),
      data = shots
    )
  })
  out <- tibble::tibble(
    splits = splits,
    id     = paste0('Slice', seq_along(splits))
  )
  class(out) <- c('rset', class(out))
  out
}

# ----- Train ----- #

# Load data.
pbps_list <- purrr::map(SEASONS, nhlscraper::gc_pbps)
common_cols <- Reduce(intersect, lapply(pbps_list, names))
pbps <- dplyr::bind_rows(
  pbps_list[[1]] %>% dplyr::select(dplyr::all_of(common_cols)),
  pbps_list[[2]] %>% dplyr::select(dplyr::all_of(common_cols)),
  pbps_list[[3]] %>% dplyr::select(dplyr::all_of(common_cols))
)
rm(pbps_list, common_cols)

# Create training sets.
shots <- pbps %>%
  nhlscraper::calculate_speed() %>% 
  dplyr::filter(
    # Keep only regular season and playoffs.
    gameTypeId %in% 2:3,
    # Keep only shots.
    typeDescKey %in% c(
      'goal',
      'shot-on-goal',
      'missed-shot'
    ),
    # Keep only special teams.
    !(situationCode %in% c('1551', '1010', '0101')),
    !isEmptyNetAgainst
  ) %>%
  dplyr::mutate(
    shootingPlayerId = dplyr::coalesce(shootingPlayerId, scoringPlayerId),
    shotType         = tidyr::replace_na(shotType, 'wrist'),
    shotType         = factor(shotType),
    isPlayoff        = gameTypeId == 3,
    isGoal           = typeDescKey == 'goal',
    isGoal           = factor(
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
    # Predictors
    isEmptyNetFor,
    skaterCountFor,
    skaterCountAgainst,
    distance,
    angle,
    shotType,
    dDdT,
    dAdT,
    isRush,
    isRebound,
    # Response
    isGoal
  )
rm(pbps)

# Define folds.
folds <- make_game_folds(shots)

# Pre-process.
rec <- recipes::recipe(isGoal ~ ., data = shots) %>%
  recipes::update_role(
    gameId,
    eventId,
    eventOwnerTeamId,
    shootingPlayerId,
    goalieInNetId,
    typeDescKey,
    new_role = 'id'
  ) %>%
  recipes::step_mutate_at(recipes::all_logical_predictors(), fn = \(x) as.integer(x)) %>%
  recipes::step_novel(recipes::all_nominal_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors()) %>%
  recipes::step_zv(recipes::all_predictors())

# Set up tuning grid.
tmp <- recipes::prep(rec)
p   <- ncol(recipes::bake(tmp, new_data = shots, recipes::all_predictors()))
min_feat_frac <- max(2 / p, 0.02)
max_feat_frac <- min(min(60, p) / p, 1.0)
rm(tmp, p)
grid <- tibble::tibble(
  trees          = as.integer(round(exp(stats::runif(GRID_SIZE, log(500), log(3000))))),
  tree_depth     = sample(2:8, GRID_SIZE, replace = TRUE),
  learn_rate     = 10^stats::runif(GRID_SIZE, -4, -1),
  min_n          = sample(2:50, GRID_SIZE, replace = TRUE),
  sample_size    = stats::runif(GRID_SIZE, 0.6, 1.0),
  loss_reduction = stats::runif(GRID_SIZE, 0, 10),
  mtry_frac      = stats::runif(GRID_SIZE, min_feat_frac, max_feat_frac)
) %>%
  dplyr::mutate(row = dplyr::row_number())

# Tune.
all_res <- vector('list', nrow(grid))
for (g in seq_len(nrow(grid))) {
  pars_g <- grid[g, ]
  fold_scores <- purrr::map_dfr(seq_len(nrow(folds)), \(k) {
    tr <- rsample::analysis(folds$splits[[k]])
    va <- rsample::assessment(folds$splits[[k]])
    rec_prep <- recipes::prep(rec, training = tr, retain = TRUE)
    x_tr <- recipes::bake(rec_prep, new_data = tr, recipes::all_predictors())
    y_tr <- recipes::bake(rec_prep, new_data = tr, recipes::all_outcomes()) %>%
      dplyr::pull(isGoal)
    y_tr <- as.integer(y_tr == 'yes')
    x_va <- recipes::bake(rec_prep, new_data = va, recipes::all_predictors())
    y_va <- recipes::bake(rec_prep, new_data = va, recipes::all_outcomes()) %>%
      dplyr::pull(isGoal)
    y_va <- as.integer(y_va == 'yes')
    x_tr <- x_tr %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) tidyr::replace_na(x, 0)))
    x_va <- x_va %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) tidyr::replace_na(x, 0)))
    m_tr <- Matrix::sparse.model.matrix(~ . - 1, data = x_tr, na.action = na.pass)
    m_va <- Matrix::sparse.model.matrix(~ . - 1, data = x_va, na.action = na.pass)
    dtrain <- lightgbm::lgb.Dataset(data = m_tr, label = y_tr)
    dvalid <- lightgbm::lgb.Dataset(data = m_va, label = y_va)
    lgb_params <- list(
      objective         = 'binary',
      metric            = 'binary_logloss',
      learning_rate     = as.numeric(pars_g$learn_rate),
      max_depth         = as.integer(pars_g$tree_depth),
      num_leaves        = as.integer(pmax(8L, pmin(255L, 2^pars_g$tree_depth))),
      min_data_in_leaf  = as.integer(pars_g$min_n),
      feature_fraction  = as.numeric(pars_g$mtry_frac),
      bagging_fraction  = as.numeric(pars_g$sample_size),
      bagging_freq      = 1L,
      min_gain_to_split = as.numeric(pars_g$loss_reduction),
      verbosity         = -1L,
      seed              = 20060527L
    )
    model_k <- lightgbm::lgb.train(
      params                = lgb_params,
      data                  = dtrain,
      nrounds               = as.integer(pars_g$trees),
      valids                = list(valid = dvalid),
      early_stopping_rounds = 50L,
      verbose               = -1L
    )
    p_hat <- stats::predict(model_k, m_va)
    eps <- 1e-15
    p2  <- pmin(pmax(p_hat, eps), 1 - eps)
    logloss <- -mean(y_va * log(p2) + (1 - y_va) * log(1 - p2))
    auc <- yardstick::roc_auc_vec(
      truth    = factor(dplyr::if_else(y_va == 1, 'yes', 'no'), levels = c('no', 'yes')),
      estimate = p_hat
    )
    acc <- yardstick::accuracy_vec(
      truth    = factor(dplyr::if_else(y_va == 1, 'yes', 'no'), levels = c('no', 'yes')),
      estimate = factor(dplyr::if_else(p_hat >= 0.5, 'yes', 'no'), levels = c('no', 'yes'))
    )
    tibble::tibble(
      slice    = folds$id[k],
      log_loss = logloss,
      roc_auc  = auc,
      accuracy = acc
    )
  })
  all_res[[g]] <- fold_scores %>%
    dplyr::summarise(
      log_loss = mean(log_loss, na.rm = TRUE),
      roc_auc  = mean(roc_auc,  na.rm = TRUE),
      accuracy = mean(accuracy, na.rm = TRUE),
      .groups  = 'drop'
    ) %>%
    dplyr::mutate(row = pars_g$row)
  message(glue::glue('grid {g}/{nrow(grid)}: log_loss={round(all_res[[g]]$log_loss, 5)}'))
}

# Find best model.
res <- dplyr::bind_rows(all_res) %>%
  dplyr::left_join(grid, by = 'row') %>%
  dplyr::arrange(log_loss)
best <- res %>%
  dplyr::slice(1)
rm(all_res, fold_scores, pars_g, g)

# Fit best model.
rec_prep <- recipes::prep(rec, training = shots, retain = TRUE)
x_all <- recipes::bake(rec_prep, new_data = shots, recipes::all_predictors()) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) tidyr::replace_na(x, 0)))
y_all <- recipes::bake(rec_prep, new_data = shots, recipes::all_outcomes()) %>%
  dplyr::pull(isGoal)
y_all <- as.integer(y_all == 'yes')
m_all <- Matrix::sparse.model.matrix(~ . - 1, data = x_all, na.action = na.pass)
dall  <- lightgbm::lgb.Dataset(data = m_all, label = y_all)
lgb_params_final <- list(
  objective         = 'binary',
  metric            = 'binary_logloss',
  learning_rate     = as.numeric(best$learn_rate),
  max_depth         = as.integer(best$tree_depth),
  num_leaves        = as.integer(pmax(8L, pmin(255L, 2^best$tree_depth))),
  min_data_in_leaf  = as.integer(best$min_n),
  feature_fraction  = as.numeric(best$mtry_frac),
  bagging_fraction  = as.numeric(best$sample_size),
  bagging_freq      = 1L,
  min_gain_to_split = as.numeric(best$loss_reduction),
  verbosity         = -1L,
  seed              = 20060527L
)
model <- lightgbm::lgb.train(
  params  = lgb_params_final,
  data    = dall,
  nrounds = as.integer(best$trees),
  verbose = -1L
)

# See importance.
imp <- lightgbm::lgb.importance(model, percentage = TRUE)
lightgbm::lgb.plot.importance(imp, top_n = 40)

# Export to RDS.
out <- list(
  model    = model,
  recipe   = rec,
  rec_prep = rec_prep,
  best     = best,
  results  = res
)
saveRDS(out, file = 'models/xG/special/model2.rds')
rm(model, out, imp, rec, rec_prep, res, best, folds, shots, SEASONS, make_game_folds, GRID_SIZE, grid, x_all, dall, lgb_params_final, m_all, max_feat_frac, min_feat_frac, y_all)
