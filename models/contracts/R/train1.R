# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(xgboost))
suppressMessages(library(nhlscraper))

# Set seed.
set.seed(20060527)

# Set constants.
START_SEASON = 20142015
END_SEASON   = 20252026

# Define helper.
make_folds <- function(df) {
  rsample::rolling_origin(
    df %>% arrange(startSeason),
    initial    = floor(0.70 * nrow(df)),
    assess     = floor(0.10 * nrow(df)),
    cumulative = TRUE,
    skip       = floor(0.05 * nrow(df))
  )
}

# Read from CSV.
contracts <- read_csv(
  'models/contracts/data/contracts.csv', 
  show_col_types = FALSE
) %>% 
  select(
    # IDs
    playerId,
    isFirst,
    isLast,
    # Predictors
    startSeason,
    age,
    prevTerm,
    prevAAV,
    # Responses
    term,
    AAV
  ) %>% 
  filter(startSeason >= START_SEASON & startSeason <= END_SEASON + 10001) %>%
  # Non-ELCs
  filter(!isFirst)

# Get caps.
caps <- read_csv('models/contracts/data/caps.csv', show_col_types = FALSE) %>% 
  select(startSeason = season, cap)

# Get bios.
bios <- players() %>% 
  select(
    playerId = id,
    fullName,
    position,
    height,
    weight,
    hand     = shootsCatches
  )

# Merge bios and caps.
contracts <- left_join(contracts, bios, by = 'playerId')
contracts <- left_join(contracts, caps, by = 'startSeason')
rm(bios, caps)

# Split data.
skater_contracts <- contracts %>% 
  filter(position != 'G')
goalie_contracts <- contracts %>% 
  filter(position == 'G') %>% 
  select(-position)
rm(contracts)

# Get skater time-on-ice data.
skater_toi_reports <- purrr::map_dfr(
  seq(from = START_SEASON - 30003, to = END_SEASON, by = 10001),
  \(s) {
    nhlscraper::skater_season_report(
      season    = s,
      game_type = 2,
      category  = 'timeonice'
    ) %>%
      dplyr::select(
        playerId,
        seasonId,
        gamesPlayed,
        evTimeOnIcePerGame,
        ppTimeOnIcePerGame,
        shTimeOnIcePerGame
      ) %>%
      dplyr::mutate(
        eTOI_per82 = evTimeOnIcePerGame / 60 * 82,
        pTOI_per82 = ppTimeOnIcePerGame / 60 * 82,
        sTOI_per82 = shTimeOnIcePerGame / 60 * 82
      ) %>%
      dplyr::select(playerId, seasonId, gamesPlayed, eTOI_per82, pTOI_per82, sTOI_per82) %>%
      dplyr::mutate(across(c(eTOI_per82, pTOI_per82, sTOI_per82), \(x) dplyr::coalesce(x, 0)))
  }
) %>%
  as.data.frame()

# Merge skater time-on-ice data.
skater_contracts <- skater_contracts %>%
  mutate(
    season_t1 = startSeason - 10001,
    season_t2 = startSeason - 20002,
    season_t3 = startSeason - 30003
  ) %>%
  left_join(
    skater_toi_reports %>%
      transmute(
        playerId,
        season_t1 = seasonId,
        gp_t1 = gamesPlayed,
        e_t1  = eTOI_per82,
        p_t1  = pTOI_per82,
        s_t1  = sTOI_per82
      ),
    by = c('playerId', 'season_t1')
  ) %>%
  left_join(
    skater_toi_reports %>%
      transmute(
        playerId,
        season_t2 = seasonId,
        gp_t2 = gamesPlayed,
        e_t2  = eTOI_per82,
        p_t2  = pTOI_per82,
        s_t2  = sTOI_per82
      ),
    by = c('playerId', 'season_t2')
  ) %>%
  left_join(
    skater_toi_reports %>%
      transmute(
        playerId,
        season_t3 = seasonId,
        gp_t3 = gamesPlayed,
        e_t3  = eTOI_per82,
        p_t3  = pTOI_per82,
        s_t3  = sTOI_per82
      ),
    by = c('playerId', 'season_t3')
  ) %>%
  mutate(
    gp_t1 = dplyr::coalesce(gp_t1, 0), gp_t2 = dplyr::coalesce(gp_t2, 0), gp_t3 = dplyr::coalesce(gp_t3, 0),
    e_t1  = dplyr::coalesce(e_t1, 0),  e_t2  = dplyr::coalesce(e_t2, 0),  e_t3  = dplyr::coalesce(e_t3, 0),
    p_t1  = dplyr::coalesce(p_t1, 0),  p_t2  = dplyr::coalesce(p_t2, 0),  p_t3  = dplyr::coalesce(p_t3, 0),
    s_t1  = dplyr::coalesce(s_t1, 0),  s_t2  = dplyr::coalesce(s_t2, 0),  s_t3  = dplyr::coalesce(s_t3, 0),
    
    GP_3yr_wavg =
      (3 * gp_t1 + 2 * gp_t2 + 1 * gp_t3) /
      (3 * (gp_t1 != 0) + 2 * (gp_t2 != 0) + 1 * (gp_t3 != 0)),
    
    eTOI_per82_3yr_wavg =
      (3 * e_t1 + 2 * e_t2 + 1 * e_t3) /
      (3 * (e_t1 != 0) + 2 * (e_t2 != 0) + 1 * (e_t3 != 0)),
    pTOI_per82_3yr_wavg =
      (3 * p_t1 + 2 * p_t2 + 1 * p_t3) /
      (3 * (p_t1 != 0) + 2 * (p_t2 != 0) + 1 * (p_t3 != 0)),
    sTOI_per82_3yr_wavg =
      (3 * s_t1 + 2 * s_t2 + 1 * s_t3) /
      (3 * (s_t1 != 0) + 2 * (s_t2 != 0) + 1 * (s_t3 != 0))
  ) %>%
  mutate(
    GP_3yr_wavg           = dplyr::coalesce(GP_3yr_wavg, 0),
    eTOI_per82_3yr_wavg   = dplyr::coalesce(eTOI_per82_3yr_wavg, 0),
    pTOI_per82_3yr_wavg   = dplyr::coalesce(pTOI_per82_3yr_wavg, 0),
    sTOI_per82_3yr_wavg   = dplyr::coalesce(sTOI_per82_3yr_wavg, 0)
  ) %>%
  select(
    -season_t1, -season_t2, -season_t3,
    -gp_t1, -gp_t2, -gp_t3,
    -e_t1,  -e_t2,  -e_t3,
    -p_t1,  -p_t2,  -p_t3,
    -s_t1,  -s_t2,  -s_t3
  )
rm(skater_toi_reports)

# Get skater shot analysis data.
skater_shot_reports <- purrr::map_dfr(
  seq(from = START_SEASON - 30003, to = END_SEASON, by = 10001),
  \(s) {
    readr::read_csv(
      glue::glue('data/skater_shot_analysis_{s}.csv'),
      show_col_types = FALSE
    ) %>%
      dplyr::transmute(
        playerId,
        seasonId    = s,
        ixGF_per82  = dplyr::coalesce(ixGF_2_per82, 0),
        oxGF_per82  = dplyr::coalesce(oxGF_2_per82, 0),
        oxGA_per82  = dplyr::coalesce(oxGA_2_per82, 0)
      )
  }
) %>%
  as.data.frame()

# Merge skater shot analysis data.
skater_contracts <- skater_contracts %>%
  mutate(
    season_t1 = startSeason - 10001,
    season_t2 = startSeason - 20002,
    season_t3 = startSeason - 30003
  ) %>%
  left_join(
    skater_shot_reports %>%
      transmute(
        playerId,
        season_t1 = seasonId,
        ix_t1   = ixGF_per82,
        oxGF_t1 = oxGF_per82,
        oxGA_t1 = oxGA_per82
      ),
    by = c('playerId', 'season_t1')
  ) %>%
  left_join(
    skater_shot_reports %>%
      transmute(
        playerId,
        season_t2 = seasonId,
        ix_t2   = ixGF_per82,
        oxGF_t2 = oxGF_per82,
        oxGA_t2 = oxGA_per82
      ),
    by = c('playerId', 'season_t2')
  ) %>%
  left_join(
    skater_shot_reports %>%
      transmute(
        playerId,
        season_t3 = seasonId,
        ix_t3   = ixGF_per82,
        oxGF_t3 = oxGF_per82,
        oxGA_t3 = oxGA_per82
      ),
    by = c('playerId', 'season_t3')
  ) %>%
  mutate(
    ix_t1 = dplyr::coalesce(ix_t1, 0), ix_t2 = dplyr::coalesce(ix_t2, 0), ix_t3 = dplyr::coalesce(ix_t3, 0),
    oxGF_t1 = dplyr::coalesce(oxGF_t1, 0), oxGF_t2 = dplyr::coalesce(oxGF_t2, 0), oxGF_t3 = dplyr::coalesce(oxGF_t3, 0),
    oxGA_t1 = dplyr::coalesce(oxGA_t1, 0), oxGA_t2 = dplyr::coalesce(oxGA_t2, 0), oxGA_t3 = dplyr::coalesce(oxGA_t3, 0),
    
    ixGF_per82_3yr_wavg =
      (3 * ix_t1 + 2 * ix_t2 + 1 * ix_t3) /
      (3 * (ix_t1 != 0) + 2 * (ix_t2 != 0) + 1 * (ix_t3 != 0)),
    oxGF_per82_3yr_wavg =
      (3 * oxGF_t1 + 2 * oxGF_t2 + 1 * oxGF_t3) /
      (3 * (oxGF_t1 != 0) + 2 * (oxGF_t2 != 0) + 1 * (oxGF_t3 != 0)),
    oxGA_per82_3yr_wavg =
      (3 * oxGA_t1 + 2 * oxGA_t2 + 1 * oxGA_t3) /
      (3 * (oxGA_t1 != 0) + 2 * (oxGA_t2 != 0) + 1 * (oxGA_t3 != 0))
  ) %>%
  mutate(
    ixGF_per82_3yr_wavg = dplyr::coalesce(ixGF_per82_3yr_wavg, 0),
    oxGF_per82_3yr_wavg = dplyr::coalesce(oxGF_per82_3yr_wavg, 0),
    oxGA_per82_3yr_wavg = dplyr::coalesce(oxGA_per82_3yr_wavg, 0)
  ) %>%
  select(
    -season_t1, -season_t2, -season_t3,
    -ix_t1, -ix_t2, -ix_t3,
    -oxGF_t1, -oxGF_t2, -oxGF_t3,
    -oxGA_t1, -oxGA_t2, -oxGA_t3
  )
rm(skater_shot_reports)

# Prepare for models.
skater_contracts <- skater_contracts %>% 
  filter(!is.na(age)) %>% 
  mutate(
    position = factor(position),
    hand     = factor(hand)
  )

# --- TERM MODEL --- #

# Keep relevant columns.
skater_contracts_term <- skater_contracts %>%
  select(
    # IDs
    playerId,
    fullName,
    isFirst,
    isLast,
    # Predictors
    startSeason,
    cap,
    prevTerm,
    prevAAV,
    age,
    position,
    height,
    weight,
    hand,
    GP_3yr_wavg,
    eTOI_per82_3yr_wavg,
    pTOI_per82_3yr_wavg,
    sTOI_per82_3yr_wavg,
    ixGF_per82_3yr_wavg,
    oxGF_per82_3yr_wavg,
    oxGA_per82_3yr_wavg,
    # Response
    term
  )

# Define folds.
folds <- make_folds(skater_contracts_term)

# Define recipe.
rec <- recipe(term ~ ., data = skater_contracts_term) %>%
  update_role(
    playerId,
    fullName,
    isFirst,
    isLast,
    new_role = 'id'
  ) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# Define XGB specs.
xgb_spec <- boost_tree(
  mode          = 'regression',
  trees         = tune(),
  tree_depth    = tune(),
  learn_rate    = tune(),
  min_n         = tune(),
  sample_size   = tune(),
  loss_reduction= tune(),
  mtry          = tune()
) %>%
  set_engine('xgboost', eval_metric = 'rmse')

# Define workflow.
wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec)

# Define parameters.
params <- extract_parameter_set_dials(wf) %>%
  update(
    trees          = trees(c(500L, 3000L)),
    tree_depth     = tree_depth(c(2L, 8L)),
    learn_rate     = learn_rate(range = c(-4, -1)),  # 1e-4 to 1e-1
    min_n          = min_n(c(2L, 50L)),
    sample_size    = sample_prop(c(0.6, 1.0)),
    loss_reduction = loss_reduction(c(0, 10))
  )

# Set mtry bounds based on baked predictors.
tmp <- prep(rec)
p   <- ncol(bake(tmp, new_data = skater_contracts_term) %>% select(-term))
params <- params %>%
  update(mtry = mtry(c(2L, min(60L, p))))
rm(tmp, p)

# Tune.
grid <- grid_space_filling(params, size = 40)
res <- tune_grid(
  wf,
  resamples = folds,
  grid      = grid,
  metrics   = metric_set(rmse, mae),
  control   = control_grid(save_pred = TRUE)
)
best     <- select_best(res, metric = 'rmse')
final_wf <- finalize_workflow(wf, best)
model    <- fit(final_wf, data = skater_contracts_term)
rm(res, best, final_wf, folds, grid, params, rec, wf, xgb_spec)

# See importance.
booster <- extract_fit_engine(model)
imp     <- xgb.importance(model = booster)
xgb.plot.importance(imp)
rm(booster, imp)

# Export to RDS.
saveRDS(model, file = 'models/contracts/skater_term_model1.rds')
rm(model)

# --- AAV MODEL --- #

# Keep relevant columns.
skater_contracts_AAV <- skater_contracts %>%
  select(
    # IDs
    playerId,
    fullName,
    isFirst,
    isLast,
    # Predictors
    startSeason,
    cap,
    prevTerm,
    prevAAV,
    age,
    position,
    height,
    weight,
    hand,
    GP_3yr_wavg,
    eTOI_per82_3yr_wavg,
    pTOI_per82_3yr_wavg,
    sTOI_per82_3yr_wavg,
    ixGF_per82_3yr_wavg,
    oxGF_per82_3yr_wavg,
    oxGA_per82_3yr_wavg,
    term,
    # Response
    AAV
  )

# Define folds.
folds <- make_folds(skater_contracts_AAV)

# Define recipe.
rec <- recipe(AAV ~ ., data = skater_contracts_AAV) %>%
  update_role(
    playerId,
    fullName,
    isFirst,
    isLast,
    new_role = 'id'
  ) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# Define XGB specs.
xgb_spec <- boost_tree(
  mode          = 'regression',
  trees         = tune(),
  tree_depth    = tune(),
  learn_rate    = tune(),
  min_n         = tune(),
  sample_size   = tune(),
  loss_reduction= tune(),
  mtry          = tune()
) %>%
  set_engine('xgboost', eval_metric = 'rmse')

# Define workflow.
wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec)

# Define parameters.
params <- extract_parameter_set_dials(wf) %>%
  update(
    trees          = trees(c(500L, 3000L)),
    tree_depth     = tree_depth(c(2L, 8L)),
    learn_rate     = learn_rate(range = c(-4, -1)),
    min_n          = min_n(c(2L, 50L)),
    sample_size    = sample_prop(c(0.6, 1.0)),
    loss_reduction = loss_reduction(c(0, 10))
  )

# Set mtry bounds based on baked predictors.
tmp <- prep(rec)
p   <- ncol(bake(tmp, new_data = skater_contracts_AAV) %>% select(-AAV))
params <- params %>%
  update(mtry = mtry(c(2L, min(60L, p))))
rm(tmp, p)

# Tune.
grid <- grid_space_filling(params, size = 40)
res <- tune_grid(
  wf,
  resamples = folds,
  grid      = grid,
  metrics   = metric_set(rmse, mae),
  control   = control_grid(save_pred = TRUE)
)
best     <- select_best(res, metric = 'rmse')
final_wf <- finalize_workflow(wf, best)
model    <- fit(final_wf, data = skater_contracts_AAV)

rm(res, best, final_wf, folds, grid, params, rec, wf, xgb_spec)

# See importance.
booster <- extract_fit_engine(model)
imp     <- xgb.importance(model = booster)
xgb.plot.importance(imp)
rm(booster, imp)

# Export to RDS.
saveRDS(model, file = 'models/contracts/skater_AAV_model1.rds')
rm(model, make_folds)
