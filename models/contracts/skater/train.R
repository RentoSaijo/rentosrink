## ----- Shared Contract Boosting Training ----- ##

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(doParallel))
suppressMessages(library(bonsai))

tidymodels::tidymodels_prefer()

set.seed(20060527)

TRAIN_PATH <- file.path("models", "contracts", "data", "skater_train.csv")
TEST_PATH <- file.path("models", "contracts", "data", "skater_test.csv")
MODEL_DIR <- file.path("models", "contracts", "skater")
RESULTS_DIR <- file.path(MODEL_DIR, "results")

ID_COLS <- c(
  "playerId",
  "startSeasonId",
  "startSeasonIdPrev",
  "dateOfSigning",
  "signedWithTeamId",
  "isLast",
  "birthDate"
)

N_FOLDS <- 5L
GRID_SIZE <- 32L
MAX_BOUNDARY_EXPANSIONS <- 8L

XGB_TREE_RANGE <- c(400L, 1600L)
XGB_TREE_DEPTH_RANGE <- c(2L, 8L)
XGB_LEARN_RATE_RANGE <- c(5e-3, 0.05)
XGB_MIN_N_RANGE <- c(15L, 120L)
XGB_LOSS_REDUCTION_RANGE <- c(1e-4, 5)
XGB_SAMPLE_SIZE_RANGE <- c(0.65, 1.0)
XGB_MTRY_LOWER <- 10L
XGB_MTRY_UPPER_CAP <- 80L

LGBM_TREE_RANGE <- c(400L, 1800L)
LGBM_TREE_DEPTH_RANGE <- c(2L, 10L)
LGBM_MAX_TREE_DEPTH_UPPER <- 64L
LGBM_LEARN_RATE_RANGE <- c(5e-3, 0.05)
LGBM_MIN_N_RANGE <- c(20L, 140L)
LGBM_LOSS_REDUCTION_RANGE <- c(1e-4, 10)
LGBM_SAMPLE_SIZE_RANGE <- c(0.6, 0.95)
LGBM_MTRY_RANGE <- c(0.1, 0.5)

detect_boundary_hits <- function(best_params, bounds, checks, tol = 1e-12) {
  purrr::map_dfr(
    names(bounds),
    function(param) {
      value <- best_params[[param]]
      limit <- bounds[[param]]
      check <- checks[[param]]
      lower_hit <- isTRUE(all.equal(value, limit[[1]], tolerance = tol))
      upper_hit <- isTRUE(all.equal(value, limit[[2]], tolerance = tol))

      if (isTRUE(check$lower) && lower_hit) {
        return(
          tibble::tibble(
            param = param,
            direction = "lower",
            value = value,
            lower = limit[[1]],
            upper = limit[[2]]
          )
        )
      }

      if (isTRUE(check$upper) && upper_hit) {
        return(
          tibble::tibble(
            param = param,
            direction = "upper",
            value = value,
            lower = limit[[1]],
            upper = limit[[2]]
          )
        )
      }

      tibble::tibble()
    }
  )
}

format_boundary_hits <- function(hits) {
  paste(
    glue::glue(
      "{hits$param}={hits$value} ({hits$direction} bound {ifelse(hits$direction == 'lower', hits$lower, hits$upper)})"
    ),
    collapse = ", "
  )
}

expand_integer_range <- function(range, direction, lower_step, upper_step, min_value = 1L, max_value = Inf) {
  updated <- range

  if (direction == "lower") {
    updated[[1]] <- max(min_value, as.integer(range[[1]] - lower_step))
  } else {
    updated[[2]] <- min(max_value, as.integer(range[[2]] + upper_step))
  }

  updated
}

expand_numeric_range <- function(range, direction, lower_factor, upper_factor, min_value, max_value) {
  updated <- range

  if (direction == "lower") {
    updated[[1]] <- max(min_value, range[[1]] * lower_factor)
  } else {
    updated[[2]] <- min(max_value, range[[2]] * upper_factor)
  }

  updated
}

expand_bounds <- function(bounds, hits, expanders) {
  updated_bounds <- bounds

  for (i in seq_len(nrow(hits))) {
    param <- hits$param[[i]]
    updated_bounds[[param]] <- expanders[[param]](updated_bounds[[param]], hits$direction[[i]])
  }

  updated_bounds
}

accept_boundary_hits <- function(hits, acceptance_caps, tol = 1e-12) {
  accepted <- purrr::map_lgl(
    seq_len(nrow(hits)),
    function(i) {
      caps <- acceptance_caps[[hits$param[[i]]]]

      if (is.null(caps)) {
        return(FALSE)
      }

      cap_value <- caps[[hits$direction[[i]]]]

      if (is.null(cap_value)) {
        return(FALSE)
      }

      boundary_value <- if (hits$direction[[i]] == "lower") hits$lower[[i]] else hits$upper[[i]]
      isTRUE(all.equal(boundary_value, cap_value, tolerance = tol))
    }
  )

  list(
    accepted = hits[accepted, , drop = FALSE],
    expand = hits[!accepted, , drop = FALSE]
  )
}

tuning_param_columns <- function(metric_results) {
  setdiff(
    names(metric_results),
    c(".metric", ".estimator", "mean", "n", "std_err", ".config", ".boundary_hit_count")
  )
}

select_one_se_candidate <- function(metric_results, bounds, checks, simplicity_preferences) {
  ranked_metrics <- metric_results %>%
    dplyr::arrange(mean, std_err)

  raw_best <- ranked_metrics %>%
    dplyr::slice(1)

  one_se_threshold <- raw_best$mean[[1]] + raw_best$std_err[[1]]
  candidates <- ranked_metrics %>%
    dplyr::filter(mean <= one_se_threshold)

  param_cols <- intersect(tuning_param_columns(candidates), names(bounds))

  candidates$.boundary_hit_count <- purrr::map_int(
    seq_len(nrow(candidates)),
    function(i) {
      nrow(
        detect_boundary_hits(
          as.list(candidates[i, param_cols, drop = FALSE]),
          bounds,
          checks
        )
      )
    }
  )

  order_components <- c(
    list(candidates$.boundary_hit_count),
    purrr::map(
      names(simplicity_preferences),
      function(param) {
        if (!param %in% names(candidates)) {
          return(rep(0, nrow(candidates)))
        }

        if (simplicity_preferences[[param]] == "asc") {
          candidates[[param]]
        } else {
          -candidates[[param]]
        }
      }
    ),
    list(candidates$mean, candidates$std_err, na.last = TRUE)
  )

  selected <- candidates[do.call(order, order_components)[1], , drop = FALSE]

  list(
    raw_best = raw_best,
    selected = selected,
    candidates = candidates,
    one_se_threshold = one_se_threshold
  )
}

count_recipe_predictors <- function(recipe, training_data) {
  prepped_recipe <- recipes::prep(recipe, training = training_data, retain = TRUE)
  predictor_count <- ncol(recipes::juice(prepped_recipe, recipes::all_predictors()))
  rm(prepped_recipe)
  predictor_count
}

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

task_response <- function(task) {
  if (task == "term") {
    return("term")
  }

  if (task == "aavPerc") {
    return("aavPerc")
  }

  stop(sprintf("Unknown task: %s", task))
}

task_metric <- function(task) {
  if (task == "term") {
    return("mn_log_loss")
  }

  if (task == "aavPerc") {
    return("rmse")
  }

  stop(sprintf("Unknown task: %s", task))
}

task_mode <- function(task) {
  if (task == "term") {
    return("classification")
  }

  if (task == "aavPerc") {
    return("regression")
  }

  stop(sprintf("Unknown task: %s", task))
}

task_metrics <- function(task) {
  if (task == "term") {
    return(
      yardstick::metric_set(
        yardstick::mn_log_loss,
        yardstick::accuracy
      )
    )
  }

  if (task == "aavPerc") {
    return(
      yardstick::metric_set(
        yardstick::rmse,
        yardstick::mae,
        yardstick::rsq
      )
    )
  }

  stop(sprintf("Unknown task: %s", task))
}

coerce_contract_logical_to_factor <- function(data, response_col = NULL) {
  logical_predictors <- setdiff(
    names(data)[vapply(data, is.logical, logical(1))],
    response_col
  )

  data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(logical_predictors),
        ~ factor(dplyr::if_else(.x, "yes", "no"), levels = c("no", "yes"))
      )
    )
}

prepare_contract_model_data <- function(model_data, task, require_outcomes = TRUE) {
  response_col <- task_response(task)

  if (task == "term") {
    if (require_outcomes) {
      model_data <- model_data %>%
        dplyr::filter(!is.na(term))
    }

    if ("term" %in% names(model_data)) {
      model_data <- model_data %>%
        dplyr::mutate(term = factor(term, levels = 1:8))
    }
  } else {
    model_data <- model_data %>%
      dplyr::select(-cap) %>%
      dplyr::filter(!is.na(term)) %>%
      dplyr::mutate(term = factor(term, levels = 1:8))

    if (require_outcomes) {
      model_data <- model_data %>%
        dplyr::filter(!is.na(aavPerc))
    }
  }

  coerce_contract_logical_to_factor(model_data, response_col = response_col)
}

load_contract_model_data <- function(task, path = TRAIN_PATH, require_outcomes = TRUE) {
  drop_cols <- c(
    "capPrev",
    "aav"
  )

  if (task == "term") {
    drop_cols <- c(drop_cols, "aavPerc")
  }

  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(
      playerId = as.integer(playerId),
      startSeasonId = as.integer(startSeasonId),
      startSeasonIdPrev = as.integer(startSeasonIdPrev),
      signedWithTeamId = as.integer(signedWithTeamId),
      contractNumber = as.integer(contractNumber),
      dateOfSigning = as.Date(dateOfSigning),
      birthDate = as.Date(birthDate)
    ) %>%
    dplyr::select(-dplyr::any_of(drop_cols)) %>%
    prepare_contract_model_data(task = task, require_outcomes = require_outcomes)
}

coerce_contract_flag <- function(x) {
  if (is.logical(x)) {
    return(dplyr::coalesce(x, FALSE))
  }

  if (inherits(x, "factor")) {
    x <- as.character(x)
  }

  if (is.character(x)) {
    return(dplyr::coalesce(x %in% c("yes", "YES", "true", "TRUE", "1"), FALSE))
  }

  dplyr::coalesce(as.logical(x), FALSE)
}

normalize_term_probabilities <- function(predictions, is_resign_col = "isResign") {
  prob_cols <- grep("^\\.pred_[0-9]+$", names(predictions), value = TRUE)

  if (!length(prob_cols)) {
    stop("No term probability columns found to normalize.")
  }

  class_levels <- as.integer(sub("^\\.pred_", "", prob_cols))
  prob_matrix <- as.matrix(predictions[, prob_cols, drop = FALSE])
  is_resign <- coerce_contract_flag(predictions[[is_resign_col]])
  max_term <- ifelse(is_resign, 8L, 7L)

  for (i in seq_len(nrow(prob_matrix))) {
    infeasible <- class_levels > max_term[[i]]

    if (any(infeasible)) {
      prob_matrix[i, infeasible] <- 0
    }
  }

  row_totals <- rowSums(prob_matrix)

  for (i in which(row_totals <= 0 | is.na(row_totals))) {
    feasible <- class_levels <= max_term[[i]]
    prob_matrix[i, feasible] <- 1 / sum(feasible)
    row_totals[[i]] <- 1
  }

  prob_matrix <- prob_matrix / row_totals

  predictions[, prob_cols] <- as_tibble(prob_matrix)
  predictions$.pred_class <- as.character(class_levels[max.col(prob_matrix, ties.method = "first")])
  predictions
}

build_recipe <- function(task, training_data) {
  response_col <- task_response(task)
  formula <- stats::as.formula(paste(response_col, "~ ."))
  id_cols <- intersect(ID_COLS, names(training_data))

  if (task == "term" && "cap" %in% names(training_data)) {
    id_cols <- union(id_cols, "cap")
  }

  recipes::recipe(formula, data = training_data) %>%
    recipes::update_role(tidyselect::all_of(id_cols), new_role = "id") %>%
    recipes::step_string2factor(recipes::all_nominal_predictors()) %>%
    recipes::step_unknown(recipes::all_nominal_predictors()) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors())
}

plot_xgb_importance <- function(fitted_wf, model_name, top_n = 25L) {
  booster <- workflows::extract_fit_engine(fitted_wf)
  importance <- tryCatch(
    xgboost::xgb.importance(model = booster),
    error = function(e) NULL
  )

  if (is.null(importance) || nrow(importance) == 0L) {
    return(NULL)
  }

  importance %>%
    tibble::as_tibble() %>%
    dplyr::arrange(dplyr::desc(Gain)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(Feature = forcats::fct_reorder(Feature, Gain)) %>%
    ggplot2::ggplot(ggplot2::aes(x = Gain, y = Feature)) +
    ggplot2::geom_col(fill = "#2F6CA8") +
    ggplot2::labs(
      title = sprintf("%s Feature Importance (Gain)", model_name),
      x = "Gain",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

plot_lgbm_importance <- function(fitted_wf, model_name, top_n = 25L) {
  booster <- workflows::extract_fit_engine(fitted_wf)
  importance <- tryCatch(
    lightgbm::lgb.importance(model = booster),
    error = function(e) NULL
  )

  if (is.null(importance) || nrow(importance) == 0L) {
    return(NULL)
  }

  importance %>%
    tibble::as_tibble() %>%
    dplyr::arrange(dplyr::desc(Gain)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(Feature = forcats::fct_reorder(Feature, Gain)) %>%
    ggplot2::ggplot(ggplot2::aes(x = Gain, y = Feature)) +
    ggplot2::geom_col(fill = "#2F6CA8") +
    ggplot2::labs(
      title = sprintf("%s Feature Importance (Gain)", model_name),
      x = "Gain",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

run_contract_xgboost_training <- function(task) {
  model_key <- paste0(task, "1")
  dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

  model_data <- load_contract_model_data(task)
  recipe <- build_recipe(task, model_data)
  predictor_count <- count_recipe_predictors(recipe, model_data)
  model_mode <- task_mode(task)
  primary_metric <- task_metric(task)

  spec <- parsnip::boost_tree(
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune(),
    min_n = tune::tune(),
    loss_reduction = tune::tune(),
    sample_size = tune::tune(),
    mtry = tune::tune()
  ) %>%
    parsnip::set_mode(model_mode) %>%
    parsnip::set_engine(
      "xgboost",
      tree_method = "hist",
      nthread = 1
    )

  workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec)

  mtry_upper <- max(1L, min(XGB_MTRY_UPPER_CAP, predictor_count))
  mtry_lower <- min(XGB_MTRY_LOWER, mtry_upper)
  mtry_max_cap <- max(20L, min(160L, predictor_count))
  fold_count <- max(2L, min(N_FOLDS, dplyr::n_distinct(model_data$playerId)))
  resamples <- rsample::group_vfold_cv(
    model_data,
    group = playerId,
    v = fold_count,
    balance = "observations"
  )
  metrics <- task_metrics(task)

  cluster_state <- create_cluster()
  on.exit(stop_cluster(cluster_state), add = TRUE)

  current_bounds <- list(
    trees = XGB_TREE_RANGE,
    tree_depth = XGB_TREE_DEPTH_RANGE,
    learn_rate = XGB_LEARN_RATE_RANGE,
    min_n = XGB_MIN_N_RANGE,
    loss_reduction = XGB_LOSS_REDUCTION_RANGE,
    sample_size = XGB_SAMPLE_SIZE_RANGE,
    mtry = c(mtry_lower, mtry_upper)
  )
  boundary_checks <- list(
    trees = list(lower = TRUE, upper = TRUE),
    tree_depth = list(lower = FALSE, upper = TRUE),
    learn_rate = list(lower = TRUE, upper = TRUE),
    min_n = list(lower = FALSE, upper = TRUE),
    loss_reduction = list(lower = TRUE, upper = TRUE),
    sample_size = list(lower = TRUE, upper = FALSE),
    mtry = list(lower = TRUE, upper = TRUE)
  )
  boundary_expanders <- list(
    trees = function(range, direction) expand_integer_range(range, direction, lower_step = 250L, upper_step = 500L, min_value = 200L, max_value = 6000L),
    tree_depth = function(range, direction) expand_integer_range(range, direction, lower_step = 2L, upper_step = 6L, min_value = 1L, max_value = 32L),
    learn_rate = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.5, upper_factor = 2, min_value = 1e-4, max_value = 0.3),
    min_n = function(range, direction) expand_integer_range(range, direction, lower_step = 5L, upper_step = 25L, min_value = 2L, max_value = 200L),
    loss_reduction = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.1, upper_factor = 4, min_value = 1e-8, max_value = 1e3),
    sample_size = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.8, upper_factor = 1.05, min_value = 0.3, max_value = 1.0),
    mtry = function(range, direction) expand_integer_range(range, direction, lower_step = 5L, upper_step = 20L, min_value = 2L, max_value = mtry_max_cap)
  )
  accepted_boundary_caps <- list(
    trees = list(lower = 200L, upper = 6000L),
    tree_depth = list(upper = 32L),
    learn_rate = list(lower = 1e-4, upper = 0.3),
    min_n = list(upper = 200L),
    loss_reduction = list(lower = 1e-8, upper = 1e3),
    sample_size = list(lower = 0.3),
    mtry = list(lower = 2L, upper = mtry_max_cap)
  )
  simplicity_preferences <- list(
    tree_depth = "asc",
    trees = "asc",
    mtry = "asc",
    sample_size = "asc",
    min_n = "desc",
    loss_reduction = "desc"
  )

  boundary_expansion_count <- 0L
  search_history <- tibble::tibble()

  repeat {
    params <- hardhat::extract_parameter_set_dials(workflow) %>%
      update(
        trees = dials::trees(current_bounds$trees),
        tree_depth = dials::tree_depth(current_bounds$tree_depth),
        learn_rate = dials::learn_rate(log10(current_bounds$learn_rate)),
        min_n = dials::min_n(current_bounds$min_n),
        loss_reduction = dials::loss_reduction(log10(current_bounds$loss_reduction)),
        sample_size = dials::sample_prop(current_bounds$sample_size),
        mtry = dials::mtry(current_bounds$mtry)
      )

    tuning_grid <- dials::grid_space_filling(params, size = GRID_SIZE)

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

    metric_results <- tune::collect_metrics(tuned) %>%
      dplyr::filter(.metric == primary_metric)
    selection <- select_one_se_candidate(
      metric_results,
      current_bounds,
      boundary_checks,
      simplicity_preferences
    )
    raw_best_metrics <- selection$raw_best
    selected_metrics <- selection$selected
    best_params <- selected_metrics %>%
      dplyr::select(dplyr::all_of(tuning_param_columns(selected_metrics)))
    raw_best_params <- raw_best_metrics %>%
      dplyr::select(dplyr::all_of(tuning_param_columns(raw_best_metrics)))

    raw_boundary_hits <- detect_boundary_hits(as.list(raw_best_params), current_bounds, boundary_checks)
    boundary_hits <- detect_boundary_hits(as.list(best_params), current_bounds, boundary_checks)
    boundary_decision <- accept_boundary_hits(boundary_hits, accepted_boundary_caps)
    accepted_hits <- boundary_decision$accepted
    expansion_hits <- boundary_decision$expand
    search_action <- if (nrow(expansion_hits) == 0L) {
      if (nrow(accepted_hits) > 0L) "accepted_boundary_cap" else "break_no_boundary"
    } else {
      "expand_bounds"
    }

    search_history <- dplyr::bind_rows(
      search_history,
      tibble::tibble(
        iteration = nrow(search_history) + 1L,
        trees_lower = current_bounds$trees[[1]],
        trees_upper = current_bounds$trees[[2]],
        tree_depth_lower = current_bounds$tree_depth[[1]],
        tree_depth_upper = current_bounds$tree_depth[[2]],
        learn_rate_lower = current_bounds$learn_rate[[1]],
        learn_rate_upper = current_bounds$learn_rate[[2]],
        min_n_lower = current_bounds$min_n[[1]],
        min_n_upper = current_bounds$min_n[[2]],
        loss_reduction_lower = current_bounds$loss_reduction[[1]],
        loss_reduction_upper = current_bounds$loss_reduction[[2]],
        sample_size_lower = current_bounds$sample_size[[1]],
        sample_size_upper = current_bounds$sample_size[[2]],
        mtry_lower = current_bounds$mtry[[1]],
        mtry_upper = current_bounds$mtry[[2]],
        raw_best_config = raw_best_metrics$.config[[1]],
        raw_best_metric = raw_best_metrics$mean[[1]],
        raw_best_std_err = raw_best_metrics$std_err[[1]],
        raw_best_boundary_hits = if (nrow(raw_boundary_hits) == 0L) "none" else format_boundary_hits(raw_boundary_hits),
        selected_config = selected_metrics$.config[[1]],
        selected_metric = selected_metrics$mean[[1]],
        selected_std_err = selected_metrics$std_err[[1]],
        selected_boundary_hits = if (nrow(boundary_hits) == 0L) "none" else format_boundary_hits(boundary_hits),
        selected_boundary_hit_count = nrow(boundary_hits),
        one_se_threshold = selection$one_se_threshold[[1]],
        one_se_candidate_count = nrow(selection$candidates),
        accepted_boundary_hits = if (nrow(accepted_hits) == 0L) "none" else format_boundary_hits(accepted_hits),
        expansion_boundary_hits = if (nrow(expansion_hits) == 0L) "none" else format_boundary_hits(expansion_hits),
        action = search_action
      )
    )

    if (nrow(expansion_hits) == 0L) {
      break
    }

    boundary_expansion_count <- boundary_expansion_count + 1L
    if (boundary_expansion_count > MAX_BOUNDARY_EXPANSIONS) {
      break
    }

    next_bounds <- expand_bounds(current_bounds, expansion_hits, boundary_expanders)
    if (identical(next_bounds, current_bounds)) {
      break
    }

    current_bounds <- next_bounds
  }

  final_workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec) %>%
    tune::finalize_workflow(best_params) %>%
    parsnip::fit(model_data)

  cv_metrics <- tune::collect_metrics(tuned)
  training_summary <- tibble::tibble(
    model = model_key,
    task = task,
    engine = "xgboost",
    mode = model_mode,
    rows = nrow(model_data),
    players = dplyr::n_distinct(model_data$playerId),
    folds = fold_count,
    predictors = predictor_count,
    boundary_expansions = boundary_expansion_count
  )
  selection_summary <- best_params %>%
    dplyr::mutate(
      selected_config = selected_metrics$.config[[1]],
      selected_metric = selected_metrics$mean[[1]],
      selected_std_err = selected_metrics$std_err[[1]],
      one_se_threshold = selection$one_se_threshold[[1]],
      one_se_candidate_count = nrow(selection$candidates)
    )

  importance_plot <- plot_xgb_importance(final_workflow, model_key)

  readr::write_csv(best_params, file.path(RESULTS_DIR, paste0(model_key, "_best_params.csv")))
  readr::write_csv(cv_metrics, file.path(RESULTS_DIR, paste0(model_key, "_cv_metrics.csv")))
  readr::write_csv(training_summary, file.path(RESULTS_DIR, paste0(model_key, "_training_summary.csv")))
  readr::write_csv(selection_summary, file.path(RESULTS_DIR, paste0(model_key, "_selection_summary.csv")))
  readr::write_csv(search_history, file.path(RESULTS_DIR, paste0(model_key, "_search_history.csv")))

  if (!is.null(importance_plot)) {
    ggplot2::ggsave(
      filename = file.path(RESULTS_DIR, paste0(model_key, "_importance.png")),
      plot = importance_plot,
      width = 8,
      height = 6,
      dpi = 180
    )
  }

  saveRDS(final_workflow, file.path(MODEL_DIR, paste0(model_key, ".rds")))
  saveRDS(
    list(
      task = task,
      engine = "xgboost",
      best_params = best_params,
      cv_metrics = cv_metrics,
      search_history = search_history,
      workflow = final_workflow
    ),
    file.path(RESULTS_DIR, paste0(model_key, "_results.rds"))
  )

  cat(sprintf("Saved fitted workflow to %s\n", file.path(MODEL_DIR, paste0(model_key, ".rds"))))

  invisible(final_workflow)
}

run_contract_lightgbm_training <- function(task) {
  model_key <- paste0(task, "2")
  dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

  model_data <- load_contract_model_data(task)
  recipe <- build_recipe(task, model_data)
  model_mode <- task_mode(task)
  primary_metric <- task_metric(task)

  spec <- parsnip::boost_tree(
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune(),
    min_n = tune::tune(),
    loss_reduction = tune::tune(),
    sample_size = tune::tune(),
    mtry = tune::tune()
  ) %>%
    parsnip::set_mode(model_mode) %>%
    parsnip::set_engine(
      "lightgbm",
      num_threads = 1,
      verbose = -1,
      counts = FALSE
    )

  workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec)

  fold_count <- max(2L, min(N_FOLDS, dplyr::n_distinct(model_data$playerId)))
  resamples <- rsample::group_vfold_cv(
    model_data,
    group = playerId,
    v = fold_count,
    balance = "observations"
  )
  metrics <- task_metrics(task)

  cluster_state <- create_cluster()
  on.exit(stop_cluster(cluster_state), add = TRUE)

  current_bounds <- list(
    trees = LGBM_TREE_RANGE,
    tree_depth = LGBM_TREE_DEPTH_RANGE,
    learn_rate = LGBM_LEARN_RATE_RANGE,
    min_n = LGBM_MIN_N_RANGE,
    loss_reduction = LGBM_LOSS_REDUCTION_RANGE,
    sample_size = LGBM_SAMPLE_SIZE_RANGE,
    mtry = LGBM_MTRY_RANGE
  )
  boundary_checks <- list(
    trees = list(lower = TRUE, upper = TRUE),
    tree_depth = list(lower = FALSE, upper = TRUE),
    learn_rate = list(lower = TRUE, upper = TRUE),
    min_n = list(lower = FALSE, upper = TRUE),
    loss_reduction = list(lower = TRUE, upper = TRUE),
    sample_size = list(lower = TRUE, upper = FALSE),
    mtry = list(lower = TRUE, upper = TRUE)
  )
  boundary_expanders <- list(
    trees = function(range, direction) expand_integer_range(range, direction, lower_step = 300L, upper_step = 600L, min_value = 200L, max_value = 7000L),
    tree_depth = function(range, direction) expand_integer_range(range, direction, lower_step = 2L, upper_step = 12L, min_value = 1L, max_value = LGBM_MAX_TREE_DEPTH_UPPER),
    learn_rate = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.5, upper_factor = 2, min_value = 5e-4, max_value = 0.3),
    min_n = function(range, direction) expand_integer_range(range, direction, lower_step = 5L, upper_step = 30L, min_value = 2L, max_value = 250L),
    loss_reduction = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.1, upper_factor = 4, min_value = 1e-8, max_value = 1e4),
    sample_size = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.8, upper_factor = 1.05, min_value = 0.3, max_value = 0.99),
    mtry = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.6, upper_factor = 1.05, min_value = 0.05, max_value = 1.0)
  )
  accepted_boundary_caps <- list(
    trees = list(lower = 200L, upper = 7000L),
    tree_depth = list(upper = LGBM_MAX_TREE_DEPTH_UPPER),
    learn_rate = list(lower = 5e-4, upper = 0.3),
    min_n = list(upper = 250L),
    loss_reduction = list(lower = 1e-8, upper = 1e4),
    sample_size = list(lower = 0.3),
    mtry = list(lower = 0.05, upper = 1.0)
  )
  simplicity_preferences <- list(
    tree_depth = "asc",
    trees = "asc",
    mtry = "asc",
    sample_size = "asc",
    min_n = "desc",
    loss_reduction = "desc"
  )

  boundary_expansion_count <- 0L
  search_history <- tibble::tibble()

  repeat {
    params <- hardhat::extract_parameter_set_dials(workflow) %>%
      update(
        trees = dials::trees(current_bounds$trees),
        tree_depth = dials::tree_depth(current_bounds$tree_depth),
        learn_rate = dials::learn_rate(log10(current_bounds$learn_rate)),
        min_n = dials::min_n(current_bounds$min_n),
        loss_reduction = dials::loss_reduction(log10(current_bounds$loss_reduction)),
        sample_size = dials::sample_prop(current_bounds$sample_size),
        mtry = dials::mtry_prop(current_bounds$mtry)
      )

    tuning_grid <- dials::grid_space_filling(params, size = GRID_SIZE)

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

    metric_results <- tune::collect_metrics(tuned) %>%
      dplyr::filter(.metric == primary_metric)
    selection <- select_one_se_candidate(
      metric_results,
      current_bounds,
      boundary_checks,
      simplicity_preferences
    )
    raw_best_metrics <- selection$raw_best
    selected_metrics <- selection$selected
    best_params <- selected_metrics %>%
      dplyr::select(dplyr::all_of(tuning_param_columns(selected_metrics)))
    raw_best_params <- raw_best_metrics %>%
      dplyr::select(dplyr::all_of(tuning_param_columns(raw_best_metrics)))

    raw_boundary_hits <- detect_boundary_hits(as.list(raw_best_params), current_bounds, boundary_checks)
    boundary_hits <- detect_boundary_hits(as.list(best_params), current_bounds, boundary_checks)
    boundary_decision <- accept_boundary_hits(boundary_hits, accepted_boundary_caps)
    accepted_hits <- boundary_decision$accepted
    expansion_hits <- boundary_decision$expand
    search_action <- if (nrow(expansion_hits) == 0L) {
      if (nrow(accepted_hits) > 0L) "accepted_boundary_cap" else "break_no_boundary"
    } else {
      "expand_bounds"
    }

    search_history <- dplyr::bind_rows(
      search_history,
      tibble::tibble(
        iteration = nrow(search_history) + 1L,
        trees_lower = current_bounds$trees[[1]],
        trees_upper = current_bounds$trees[[2]],
        tree_depth_lower = current_bounds$tree_depth[[1]],
        tree_depth_upper = current_bounds$tree_depth[[2]],
        learn_rate_lower = current_bounds$learn_rate[[1]],
        learn_rate_upper = current_bounds$learn_rate[[2]],
        min_n_lower = current_bounds$min_n[[1]],
        min_n_upper = current_bounds$min_n[[2]],
        loss_reduction_lower = current_bounds$loss_reduction[[1]],
        loss_reduction_upper = current_bounds$loss_reduction[[2]],
        sample_size_lower = current_bounds$sample_size[[1]],
        sample_size_upper = current_bounds$sample_size[[2]],
        mtry_lower = current_bounds$mtry[[1]],
        mtry_upper = current_bounds$mtry[[2]],
        raw_best_config = raw_best_metrics$.config[[1]],
        raw_best_metric = raw_best_metrics$mean[[1]],
        raw_best_std_err = raw_best_metrics$std_err[[1]],
        raw_best_boundary_hits = if (nrow(raw_boundary_hits) == 0L) "none" else format_boundary_hits(raw_boundary_hits),
        selected_config = selected_metrics$.config[[1]],
        selected_metric = selected_metrics$mean[[1]],
        selected_std_err = selected_metrics$std_err[[1]],
        selected_boundary_hits = if (nrow(boundary_hits) == 0L) "none" else format_boundary_hits(boundary_hits),
        selected_boundary_hit_count = nrow(boundary_hits),
        one_se_threshold = selection$one_se_threshold[[1]],
        one_se_candidate_count = nrow(selection$candidates),
        accepted_boundary_hits = if (nrow(accepted_hits) == 0L) "none" else format_boundary_hits(accepted_hits),
        expansion_boundary_hits = if (nrow(expansion_hits) == 0L) "none" else format_boundary_hits(expansion_hits),
        action = search_action
      )
    )

    if (nrow(expansion_hits) == 0L) {
      break
    }

    boundary_expansion_count <- boundary_expansion_count + 1L
    if (boundary_expansion_count > MAX_BOUNDARY_EXPANSIONS) {
      break
    }

    next_bounds <- expand_bounds(current_bounds, expansion_hits, boundary_expanders)
    if (identical(next_bounds, current_bounds)) {
      break
    }

    current_bounds <- next_bounds
  }

  final_workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec) %>%
    tune::finalize_workflow(best_params) %>%
    parsnip::fit(model_data)

  cv_metrics <- tune::collect_metrics(tuned)
  training_summary <- tibble::tibble(
    model = model_key,
    task = task,
    engine = "lightgbm",
    mode = model_mode,
    rows = nrow(model_data),
    players = dplyr::n_distinct(model_data$playerId),
    folds = fold_count,
    predictors = count_recipe_predictors(recipe, model_data),
    boundary_expansions = boundary_expansion_count
  )
  selection_summary <- best_params %>%
    dplyr::mutate(
      selected_config = selected_metrics$.config[[1]],
      selected_metric = selected_metrics$mean[[1]],
      selected_std_err = selected_metrics$std_err[[1]],
      one_se_threshold = selection$one_se_threshold[[1]],
      one_se_candidate_count = nrow(selection$candidates)
    )

  importance_plot <- plot_lgbm_importance(final_workflow, model_key)

  readr::write_csv(best_params, file.path(RESULTS_DIR, paste0(model_key, "_best_params.csv")))
  readr::write_csv(cv_metrics, file.path(RESULTS_DIR, paste0(model_key, "_cv_metrics.csv")))
  readr::write_csv(training_summary, file.path(RESULTS_DIR, paste0(model_key, "_training_summary.csv")))
  readr::write_csv(selection_summary, file.path(RESULTS_DIR, paste0(model_key, "_selection_summary.csv")))
  readr::write_csv(search_history, file.path(RESULTS_DIR, paste0(model_key, "_search_history.csv")))

  if (!is.null(importance_plot)) {
    ggplot2::ggsave(
      filename = file.path(RESULTS_DIR, paste0(model_key, "_importance.png")),
      plot = importance_plot,
      width = 8,
      height = 6,
      dpi = 180
    )
  }

  saveRDS(final_workflow, file.path(MODEL_DIR, paste0(model_key, ".rds")))
  saveRDS(
    list(
      task = task,
      engine = "lightgbm",
      best_params = best_params,
      cv_metrics = cv_metrics,
      search_history = search_history,
      workflow = final_workflow
    ),
    file.path(RESULTS_DIR, paste0(model_key, "_results.rds"))
  )

  cat(sprintf("Saved fitted workflow to %s\n", file.path(MODEL_DIR, paste0(model_key, ".rds"))))

  invisible(final_workflow)
}
