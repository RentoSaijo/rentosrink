## ----- Shared Legacy Ridge xG Training ----- ##

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(doParallel))
suppressMessages(library(glmnet))

tidymodels::tidymodels_prefer()

set.seed(20060527)

LEGACY_SEASON_MAP <- list(
  `1` = c(20102011L, 20112012L),
  `2` = c(20162017L, 20172018L)
)
N_FOLDS <- 5L
GRID_LEVELS <- 30L
PENALTY_RANGE <- c(-7, 2)
MAX_PENALTY_EXPANSIONS <- 8L
LOWER_BOUND_ACCEPT_LOG10 <- -7
LEGACY_MODEL_DIR <- file.path("models", "xG", "legacy")
LEGACY_DATA_DIR <- file.path(LEGACY_MODEL_DIR, "data")
LEGACY_RESULTS_DIR <- file.path(LEGACY_MODEL_DIR, "results")

season_id_from_game_id <- function(game_id) {
  season_start <- as.integer(substr(as.character(game_id), 1, 4))
  as.integer(paste0(season_start, season_start + 1L))
}

get_legacy_seasons <- function(version) {
  version_key <- as.character(as.integer(version))
  seasons <- LEGACY_SEASON_MAP[[version_key]]

  if (is.null(seasons)) {
    stop(sprintf("Unknown legacy version: %s", version))
  }

  seasons
}

legacy_model_key <- function(dataset, version) {
  paste0(dataset, as.integer(version))
}

penalty_boundary_hit <- function(best_params, penalty_range, tol = 1e-12) {
  penalty_value <- best_params$penalty
  lower_value <- 10^penalty_range[[1]]
  upper_value <- 10^penalty_range[[2]]

  lower_hit <- isTRUE(all.equal(penalty_value, lower_value, tolerance = tol))
  upper_hit <- isTRUE(all.equal(penalty_value, upper_value, tolerance = tol))

  if (lower_hit) {
    return("lower")
  }

  if (upper_hit) {
    return("upper")
  }

  NA_character_
}

expand_penalty_range <- function(penalty_range, boundary_hit, step = 1) {
  if (boundary_hit == "lower") {
    return(c(penalty_range[[1]] - step, penalty_range[[2]]))
  }

  if (boundary_hit == "upper") {
    return(c(penalty_range[[1]], penalty_range[[2]] + step))
  }

  penalty_range
}

load_model_data <- function(dataset, version) {
  model_key <- legacy_model_key(dataset, version)
  seasons <- get_legacy_seasons(version)
  data_path <- file.path(LEGACY_DATA_DIR, paste0(model_key, "_train.csv"))

  model_data <- readr::read_csv(data_path, show_col_types = FALSE) %>%
    dplyr::mutate(season = season_id_from_game_id(gameId))

  if (!setequal(sort(unique(model_data$season)), sort(seasons))) {
    stop(
      sprintf(
        "Expected %s to contain only seasons %s.",
        basename(data_path),
        paste(seasons, collapse = ", ")
      )
    )
  }

  logical_predictors <- setdiff(
    names(model_data)[vapply(model_data, is.logical, logical(1))],
    "isGoal"
  )

  model_data %>%
    dplyr::mutate(
      isGoal = dplyr::if_else(isGoal, "goal", "no_goal"),
      isGoal = factor(isGoal, levels = c("no_goal", "goal")),
      dplyr::across(
        dplyr::all_of(logical_predictors),
        ~ factor(dplyr::if_else(.x, "yes", "no"), levels = c("no", "yes"))
      )
    )
}

build_recipe <- function(training_data) {
  recipes::recipe(isGoal ~ ., data = training_data) %>%
    recipes::update_role(gameId, eventId, season, new_role = "id") %>%
    recipes::step_string2factor(recipes::all_nominal_predictors()) %>%
    recipes::step_unknown(recipes::all_nominal_predictors()) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors())
}

goal_roc_auc <- yardstick::new_prob_metric(
  function(data, truth, ..., na_rm = TRUE, case_weights = NULL) {
    yardstick::roc_auc(
      data,
      truth = {{ truth }},
      .pred_goal,
      na_rm = na_rm,
      case_weights = {{ case_weights }},
      event_level = "second"
    ) %>%
      dplyr::mutate(.metric = "goal_roc_auc")
  },
  direction = "maximize"
)

goal_pr_auc <- yardstick::new_prob_metric(
  function(data, truth, ..., na_rm = TRUE, case_weights = NULL) {
    yardstick::pr_auc(
      data,
      truth = {{ truth }},
      .pred_goal,
      na_rm = na_rm,
      case_weights = {{ case_weights }},
      event_level = "second"
    ) %>%
      dplyr::mutate(.metric = "goal_pr_auc")
  },
  direction = "maximize"
)

create_cluster <- function() {
  n_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(n_cores)) {
    n_cores <- parallel::detectCores(logical = TRUE)
  }
  if (is.na(n_cores)) {
    n_cores <- 1L
  }

  worker_cap <- suppressWarnings(
    as.integer(Sys.getenv("XG_MAX_WORKERS", unset = NA_character_))
  )

  n_workers <- if (is.na(worker_cap)) {
    max(1L, n_cores)
  } else {
    max(1L, min(worker_cap, n_cores))
  }

  cl <- NULL
  try(doParallel::stopImplicitCluster(), silent = TRUE)
  foreach::registerDoSEQ()

  if (n_workers > 1L) {
    cl <- parallel::makePSOCKcluster(n_workers)
    doParallel::registerDoParallel(cl)
  }

  list(cluster = cl, n_workers = n_workers)
}

stop_cluster <- function(cluster_state) {
  if (!is.null(cluster_state$cluster)) {
    try(parallel::stopCluster(cluster_state$cluster), silent = TRUE)
  }

  try(doParallel::stopImplicitCluster(), silent = TRUE)
  foreach::registerDoSEQ()
}

extract_final_coefficients <- function(final_workflow, best_penalty) {
  engine_fit <- workflows::extract_fit_parsnip(final_workflow)$fit
  coef_matrix <- as.matrix(stats::coef(engine_fit, s = best_penalty))

  tibble::tibble(
    term = rownames(coef_matrix),
    estimate = as.numeric(coef_matrix[, 1])
  ) %>%
    dplyr::arrange(desc(abs(estimate)))
}

extract_preprocessing_metadata <- function(recipe, training_data) {
  prepped_recipe <- recipes::prep(recipe, training = training_data, retain = TRUE)
  step_classes <- vapply(prepped_recipe$steps, function(step) class(step)[1], character(1))

  tidy_step <- function(step_class) {
    step_index <- match(step_class, step_classes)

    if (is.na(step_index)) {
      return(tibble::tibble())
    }

    recipes::tidy(prepped_recipe, number = step_index)
  }

  unknown_levels <- tidy_step("step_unknown") %>%
    dplyr::transmute(variable = terms, value)

  novel_levels <- tidy_step("step_novel") %>%
    dplyr::transmute(variable = terms, value)

  dummy_levels <- tidy_step("step_dummy") %>%
    dplyr::transmute(
      variable = terms,
      level = columns,
      output_column = make.names(paste(terms, columns, sep = "_"))
    )

  impute_medians <- tidy_step("step_impute_median") %>%
    dplyr::transmute(term = terms, median = value)

  normalize_params <- tidy_step("step_normalize") %>%
    dplyr::transmute(term = terms, statistic, value) %>%
    tidyr::pivot_wider(names_from = statistic, values_from = value) %>%
    dplyr::rename(mean = mean, sd = sd)

  zero_variance_terms <- tidy_step("step_zv") %>%
    dplyr::transmute(term = terms)

  list(
    unknown_levels = unknown_levels,
    novel_levels = novel_levels,
    dummy_levels = dummy_levels,
    impute_medians = impute_medians,
    normalize_params = normalize_params,
    zero_variance_terms = zero_variance_terms
  )
}

run_legacy_ridge_training <- function(dataset, version) {
  model_key <- legacy_model_key(dataset, version)
  dir.create(LEGACY_MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(LEGACY_RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

  model_data <- load_model_data(dataset, version)
  recipe <- build_recipe(model_data)

  spec <- parsnip::logistic_reg(
    mode = "classification",
    penalty = tune::tune(),
    mixture = 0
  ) %>%
    parsnip::set_engine("glmnet")

  workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec)

  fold_count <- max(2L, min(N_FOLDS, dplyr::n_distinct(model_data$gameId)))
  resamples <- rsample::group_vfold_cv(
    model_data,
    group = gameId,
    v = fold_count,
    balance = "observations"
  )
  metrics <- yardstick::metric_set(
    yardstick::mn_log_loss,
    goal_roc_auc,
    goal_pr_auc,
    yardstick::brier_class
  )

  cluster_state <- create_cluster()
  on.exit(stop_cluster(cluster_state), add = TRUE)

  current_penalty_range <- PENALTY_RANGE
  search_history <- tibble::tibble()
  expansion_count <- 0L

  repeat {
    penalty_param <- dials::penalty(range = current_penalty_range)
    tuning_grid <- dials::grid_regular(penalty_param, levels = GRID_LEVELS)

    tuned <- tune::tune_grid(
      workflow,
      resamples = resamples,
      grid = tuning_grid,
      metrics = metrics,
      control = tune::control_grid(
        verbose = TRUE,
        allow_par = cluster_state$n_workers > 1L,
        parallel_over = "resamples",
        save_pred = FALSE
      )
    )

    best_params <- tune::select_best(tuned, metric = "mn_log_loss")
    best_metrics <- tune::show_best(tuned, metric = "mn_log_loss", n = 10)
    best_log_loss <- best_metrics$mean[[1]]
    boundary_hit <- penalty_boundary_hit(best_params, current_penalty_range)

    search_history <- dplyr::bind_rows(
      search_history,
      tibble::tibble(
        iteration = nrow(search_history) + 1L,
        log10_lower = current_penalty_range[[1]],
        log10_upper = current_penalty_range[[2]],
        penalty_lower = 10^current_penalty_range[[1]],
        penalty_upper = 10^current_penalty_range[[2]],
        best_penalty = best_params$penalty,
        best_cv_mn_log_loss = best_log_loss,
        boundary_hit = dplyr::coalesce(boundary_hit, "none")
      )
    )

    if (!is.na(boundary_hit) &&
      boundary_hit == "lower" &&
      current_penalty_range[[1]] <= LOWER_BOUND_ACCEPT_LOG10) {
      search_history$boundary_hit[[nrow(search_history)]] <- "accepted_lower_boundary"
      break
    }

    if (is.na(boundary_hit)) {
      break
    }

    expansion_count <- expansion_count + 1L

    if (expansion_count > MAX_PENALTY_EXPANSIONS) {
      stop(
        sprintf(
          "Best penalty kept hitting the %s boundary after %d expansions for %s.",
          boundary_hit,
          MAX_PENALTY_EXPANSIONS,
          model_key
        )
      )
    }

    current_penalty_range <- expand_penalty_range(current_penalty_range, boundary_hit)
  }

  training_summary <- tibble::tibble(
    seasons = paste(sort(unique(model_data$season)), collapse = ","),
    games = dplyr::n_distinct(model_data$gameId),
    rows = nrow(model_data),
    goal_rate = mean(model_data$isGoal == "goal")
  )

  final_workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec) %>%
    tune::finalize_workflow(best_params) %>%
    parsnip::fit(model_data)

  preprocessing <- extract_preprocessing_metadata(recipe, model_data)
  cv_metrics <- tune::collect_metrics(tuned)
  coefficients <- extract_final_coefficients(final_workflow, best_params$penalty)
  best_params_output <- best_params %>%
    dplyr::mutate(
      log10_lower = current_penalty_range[[1]],
      log10_upper = current_penalty_range[[2]],
      penalty_lower = 10^current_penalty_range[[1]],
      penalty_upper = 10^current_penalty_range[[2]]
    )

  readr::write_csv(best_params_output, file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_best_params.csv")))
  readr::write_csv(cv_metrics, file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_cv_metrics.csv")))
  readr::write_csv(training_summary, file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_training_summary.csv")))
  readr::write_csv(coefficients, file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_coefficients.csv")))
  readr::write_csv(search_history, file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_search_history.csv")))
  readr::write_csv(
    preprocessing$unknown_levels,
    file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_unknown_levels.csv"))
  )
  readr::write_csv(
    preprocessing$novel_levels,
    file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_novel_levels.csv"))
  )
  readr::write_csv(
    preprocessing$dummy_levels,
    file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_dummy_levels.csv"))
  )
  readr::write_csv(
    preprocessing$impute_medians,
    file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_impute_medians.csv"))
  )
  readr::write_csv(
    preprocessing$normalize_params,
    file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_normalize_params.csv"))
  )
  readr::write_csv(
    preprocessing$zero_variance_terms,
    file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_zero_variance_terms.csv"))
  )

  saveRDS(final_workflow, file.path(LEGACY_MODEL_DIR, paste0(model_key, ".rds")))
  saveRDS(
    list(
      dataset = dataset,
      version = as.integer(version),
      seasons = get_legacy_seasons(version),
      engine = "ridge_glmnet",
      training_summary = training_summary,
      best_params = best_params_output,
      best_cv = best_metrics,
      cv_metrics = cv_metrics,
      coefficients = coefficients,
      search_history = search_history,
      preprocessing = preprocessing
    ),
    file.path(LEGACY_RESULTS_DIR, paste0(model_key, "_results.rds"))
  )

  print(training_summary)
  print(best_metrics)
  print(coefficients %>% dplyr::slice_head(n = 25))
  cat(sprintf("Best grouped CV mn_log_loss: %.6f\n", best_log_loss))
  cat(sprintf("Saved fitted workflow to models/xG/legacy/%s.rds\n", model_key))
}
