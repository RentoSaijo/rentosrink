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
TERM_GRID_SIZE <- 15L
AAVP_GRID_SIZE <- 25L
DETECTED_CORES <- as.integer(parallel::detectCores(logical = TRUE))
if (is.na(DETECTED_CORES) || DETECTED_CORES < 1L) {
  DETECTED_CORES <- 1L
}
PARALLEL_WORKERS <- max(1L, DETECTED_CORES - 1L)

# Register parallel backend for tune_grid().
doParallel::registerDoParallel(cores = PARALLEL_WORKERS)
on.exit(doParallel::stopImplicitCluster(), add = TRUE)
message(sprintf(
  'Parallel workers registered: %s (detected cores: %s)',
  foreach::getDoParWorkers(),
  DETECTED_CORES
))
message(sprintf(
  'Tuning config: folds=%s, term_grid=%s, aavP_grid=%s',
  N_FOLDS,
  TERM_GRID_SIZE,
  AAVP_GRID_SIZE
))

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

# Update dials parameter objects with version-safe dispatch.
update_params <- function(param_set, ...) {
  stats::update(param_set, ...)
}

# Build parameter grid with fallback for older dials versions.
build_param_grid <- function(param_set, size) {
  if ('grid_space_filling' %in% getNamespaceExports('dials')) {
    return(dials::grid_space_filling(param_set, size = size))
  }
  if ('grid_max_entropy' %in% getNamespaceExports('dials')) {
    return(dials::grid_max_entropy(param_set, size = size))
  }
  dials::grid_latin_hypercube(param_set, size = size)
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

# Tune, select, and fit final workflow.
tune_and_fit <- function(wf, folds, grid, metrics, select_metric, data) {
  tune_res <- tune::tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metrics,
    control = tune::control_grid(
      save_pred = TRUE,
      verbose = TRUE,
      allow_par = TRUE,
      parallel_over = 'resamples'
    )
  )
  best <- tune::select_best(tune_res, metric = select_metric)
  final_wf <- tune::finalize_workflow(wf, best)
  final_fit <- parsnip::fit(final_wf, data = data)
  list(best = best, fit = final_fit)
}

# Plot top feature importances from fitted random forest model.
plot_rf_importance <- function(fitted_wf, model_name, top_n = 25L) {
  rf_fit <- workflows::extract_fit_engine(fitted_wf)
  importance <- rf_fit$variable.importance
  if (is.null(importance) || length(importance) == 0L) {
    message(sprintf('No feature importance values found for %s model.', model_name))
    return(invisible(NULL))
  }

  plot_data <- tibble::tibble(
    Feature = names(importance),
    Importance = as.numeric(importance)
  ) %>%
    dplyr::arrange(dplyr::desc(Importance)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(Feature = forcats::fct_reorder(Feature, Importance))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Importance, y = Feature)) +
    ggplot2::geom_col(fill = '#2F6CA8') +
    ggplot2::labs(
      title = sprintf('%s Model Feature Importance', model_name),
      x = 'Importance',
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)

  print(p)
  invisible(p)
}

# ----- Prepare Data ----- #

predictor_cols <- setdiff(names(contracts), c(ID_COLS, RESPONSE_COLS))

contracts <- contracts %>%
  dplyr::mutate(term = as.integer(term))

term_data <- contracts %>%
  dplyr::select(dplyr::all_of(c(ID_COLS, predictor_cols, 'term'))) %>%
  dplyr::filter(!is.na(term)) %>%
  dplyr::mutate(term = factor(term, levels = 1:8))

aavp_data <- contracts %>%
  dplyr::select(dplyr::all_of(c(ID_COLS, predictor_cols, 'term', 'aavP'))) %>%
  dplyr::filter(!is.na(term), !is.na(aavP))

# ----- Train Term Model (Random Forest Classification) ----- #

term_recipe <- make_recipe(term ~ ., term_data)
term_mtry_max <- get_mtry_max(term_recipe, term_data)

term_spec <- parsnip::rand_forest(
  mode = 'classification',
  trees = tune::tune(),
  min_n = tune::tune(),
  mtry = tune::tune()
) %>%
  parsnip::set_engine('ranger', probability = TRUE, importance = 'impurity', num.threads = 1)

term_wf <- workflows::workflow() %>%
  workflows::add_recipe(term_recipe) %>%
  workflows::add_model(term_spec)

term_folds <- rsample::vfold_cv(term_data, v = N_FOLDS, strata = term)
term_params <- hardhat::extract_parameter_set_dials(term_wf) %>%
  update_params(
    trees = dials::trees(c(300L, 3000L)),
    min_n = dials::min_n(c(1L, 40L)),
    mtry = dials::mtry(c(1L, term_mtry_max))
  )
term_grid <- build_param_grid(term_params, TERM_GRID_SIZE)
term_metrics <- yardstick::metric_set(yardstick::mn_log_loss, yardstick::accuracy)
term_fit_obj <- tune_and_fit(
  wf = term_wf,
  folds = term_folds,
  grid = term_grid,
  metrics = term_metrics,
  select_metric = 'mn_log_loss',
  data = term_data
)
print_best('term', term_fit_obj$best)

# ----- Train AAV% Model (Random Forest Regression) ----- #

aavp_recipe <- make_recipe(aavP ~ ., aavp_data)
aavp_mtry_max <- get_mtry_max(aavp_recipe, aavp_data)

aavp_spec <- parsnip::rand_forest(
  mode = 'regression',
  trees = tune::tune(),
  min_n = tune::tune(),
  mtry = tune::tune()
) %>%
  parsnip::set_engine('ranger', importance = 'impurity', num.threads = 1)

aavp_wf <- workflows::workflow() %>%
  workflows::add_recipe(aavp_recipe) %>%
  workflows::add_model(aavp_spec)

aavp_folds <- rsample::vfold_cv(aavp_data, v = N_FOLDS)
aavp_params <- hardhat::extract_parameter_set_dials(aavp_wf) %>%
  update_params(
    trees = dials::trees(c(300L, 3000L)),
    min_n = dials::min_n(c(1L, 40L)),
    mtry = dials::mtry(c(1L, aavp_mtry_max))
  )
aavp_grid <- build_param_grid(aavp_params, AAVP_GRID_SIZE)
aavp_metrics <- yardstick::metric_set(yardstick::rmse, yardstick::mae)
aavp_fit_obj <- tune_and_fit(
  wf = aavp_wf,
  folds = aavp_folds,
  grid = aavp_grid,
  metrics = aavp_metrics,
  select_metric = 'rmse',
  data = aavp_data
)
print_best('aavP', aavp_fit_obj$best)

# ----- Save Models ----- #

saveRDS(term_fit_obj$fit, file = TERM_OUT)
saveRDS(aavp_fit_obj$fit, file = AAVP_OUT)

# ----- Importance Plots ----- #

plot_rf_importance(
  fitted_wf = term_fit_obj$fit,
  model_name = 'Term (Random Forest)'
)
plot_rf_importance(
  fitted_wf = aavp_fit_obj$fit,
  model_name = 'AAV% (Random Forest)'
)
