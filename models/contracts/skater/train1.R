# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(doParallel))

# Read from CSV.
contracts <- readr::read_csv('models/contracts/data/skater_contracts_train.csv', show_col_types = FALSE)

# Set seed.
set.seed(20060527)

# Define constants.
ID_COLS <- c(
  'playerId',
  'signedWithTeamId',
  'startSeasonId',
  'endSeasonId',
  'isFirst',
  'isLast',
  'birthDate',
  'cap',
  'height',
  'weight',
  'handCode'
)
RESPONSE_COLS <- c('term', 'aavP')
TERM_OUT <- 'models/contracts/skater/term1.rds'
AAVP_OUT <- 'models/contracts/skater/aavP1.rds'
N_FOLDS <- 5L
GRID_SIZE <- 15L
DETECTED_CORES <- as.integer(parallel::detectCores(logical = TRUE))
if (is.na(DETECTED_CORES) || DETECTED_CORES < 1L) {
  DETECTED_CORES <- 1L
}
PARALLEL_WORKERS <- max(1L, DETECTED_CORES - 1L)

# Register parallel backend for tune_grid().
doParallel::registerDoParallel(cores = PARALLEL_WORKERS)
message(sprintf(
  'Parallel workers registered: %s (detected cores: %s)',
  foreach::getDoParWorkers(),
  DETECTED_CORES
))
message(sprintf('Tuning config: folds=%s, grid_size=%s', N_FOLDS, GRID_SIZE))

# ----- Helpers ----- #

# Shared pre-processing recipe for both models.
make_recipe <- function(formula, data) {
  recipes::recipe(formula, data = data) %>%
    recipes::update_role(tidyselect::all_of(ID_COLS), new_role = 'id') %>%
    recipes::step_mutate_at(recipes::all_logical_predictors(), fn = \(x) as.integer(x)) %>%
    recipes::step_string2factor(recipes::all_nominal_predictors()) %>%
    recipes::step_unknown(recipes::all_nominal_predictors()) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_zv(recipes::all_predictors())
}

# Get maximum valid mtry after recipe pre-processing.
get_mtry_max <- function(recipe_obj, data) {
  rec_prep <- recipes::prep(recipe_obj, training = data)
  baked <- recipes::bake(rec_prep, new_data = data, recipes::all_predictors())
  ncol(baked)
}

# Tune and fit XGBoost with k-fold CV.
fit_tuned_xgb <- function(data, formula, mode, eval_metric, metrics, select_metric, strata_col = NULL) {
  rec <- make_recipe(formula, data)
  mtry_max <- get_mtry_max(rec, data)
  spec <- parsnip::boost_tree(
    mode = mode,
    trees = tune::tune(),
    tree_depth = tune::tune(),
    min_n = tune::tune(),
    loss_reduction = tune::tune(),
    sample_size = tune::tune(),
    mtry = tune::tune(),
    learn_rate = tune::tune(),
    stop_iter = 50
  ) %>%
    parsnip::set_engine('xgboost', eval_metric = eval_metric, nthread = 1)
  wf <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(spec)
  folds <- if (is.null(strata_col)) {
    rsample::vfold_cv(data, v = N_FOLDS)
  } else {
    rsample::vfold_cv(data, v = N_FOLDS, strata = !!rlang::sym(strata_col))
  }
  params <- hardhat::extract_parameter_set_dials(wf) %>%
    update(
      trees = dials::trees(c(300L, 3000L)),
      tree_depth = dials::tree_depth(c(2L, 10L)),
      min_n = dials::min_n(c(1L, 20L)),
      loss_reduction = dials::loss_reduction(c(-10, 1)),
      sample_size = dials::sample_prop(c(0.5, 1.0)),
      mtry = dials::mtry(c(1L, mtry_max)),
      learn_rate = dials::learn_rate(c(-4, -1))
    )
  grid <- dials::grid_space_filling(params, size = GRID_SIZE)
  tune_res <- tune::tune_grid(
    wf,
    resamples = folds,
    grid      = grid,
    metrics   = metrics,
    control   = tune::control_grid(
      save_pred = TRUE,
      verbose   = TRUE,
      allow_par = TRUE,
      parallel_over = 'everything'
    )
  )
  best <- tune::select_best(tune_res, metric = select_metric)
  final_wf  <- tune::finalize_workflow(wf, best)
  final_fit <- parsnip::fit(final_wf, data = data)
  list(
    fit      = final_fit,
    best     = best,
    tune_res = tune_res
  )
}

# Log selected tuning parameters.
print_best <- function(model_name, best_tbl) {
  best_values <- best_tbl %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'name', values_to = 'value') %>%
    dplyr::mutate(pair = paste0(name, '=', value)) %>%
    dplyr::pull(pair)
  message(sprintf('Best %s parameters: %s', model_name, paste(best_values, collapse = ', ')))
}

# Plot top feature importances from fitted XGBoost model.
plot_importance <- function(fitted_wf, model_name, top_n = 25L) {
  booster <- workflows::extract_fit_parsnip(fitted_wf)$fit
  importance <- xgboost::xgb.importance(model = booster)
  if (nrow(importance) == 0) {
    message(sprintf('No feature importance values found for %s model.', model_name))
    return(invisible(NULL))
  }
  plot_data <- importance %>%
    tibble::as_tibble() %>%
    dplyr::arrange(dplyr::desc(Gain)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(Feature = forcats::fct_reorder(Feature, Gain))
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Gain, y = Feature)) +
    ggplot2::geom_col(fill = '#2F6CA8') +
    ggplot2::labs(
      title = sprintf('%s Model Feature Importance (Gain)', model_name),
      x = 'Gain',
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)
  print(p)
  invisible(p)
}

# ----- Prepare Data ----- #

predictor_cols <- setdiff(names(contracts), c(ID_COLS, RESPONSE_COLS))

contracts <- contracts %>%
  dplyr::mutate(
    term = as.integer(term)
  )
term_data <- contracts %>%
  dplyr::select(dplyr::all_of(c(ID_COLS, predictor_cols, 'term'))) %>%
  dplyr::filter(!is.na(term)) %>%
  dplyr::mutate(term = factor(term, levels = 1:8))
aavp_data <- contracts %>%
  dplyr::select(dplyr::all_of(c(ID_COLS, predictor_cols, 'term', 'aavP'))) %>%
  dplyr::filter(!is.na(term), !is.na(aavP))

# ----- Train Term Model ----- #

term_metrics <- yardstick::metric_set(yardstick::mn_log_loss, yardstick::accuracy)
term_fit_obj <- fit_tuned_xgb(
  data = term_data,
  formula = term ~ .,
  mode = 'classification',
  eval_metric = 'mlogloss',
  metrics = term_metrics,
  select_metric = 'mn_log_loss',
  strata_col = 'term'
)
print_best('term', term_fit_obj$best)

# ----- Train AAV% Model ----- #

aavp_metrics <- yardstick::metric_set(yardstick::rmse, yardstick::mae)
aavp_fit_obj <- fit_tuned_xgb(
  data = aavp_data,
  formula = aavP ~ .,
  mode = 'regression',
  eval_metric = 'rmse',
  metrics = aavp_metrics,
  select_metric = 'rmse'
)
print_best('aavP', aavp_fit_obj$best)

# ----- Save Models ----- #

saveRDS(term_fit_obj$fit, file = TERM_OUT)
saveRDS(aavp_fit_obj$fit, file = AAVP_OUT)

# ----- Importance Plots ----- #

plot_importance(
  fitted_wf = term_fit_obj$fit,
  model_name = 'Term'
)
plot_importance(
  fitted_wf = aavp_fit_obj$fit,
  model_name = 'AAV%'
)
