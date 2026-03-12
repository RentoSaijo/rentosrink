## ----- Shared xGBoost Training ----- ##

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(doParallel))

tidymodels::tidymodels_prefer()

set.seed(20060527)

TRAIN_SEASON_STARTS <- c(2023L, 2024L)
N_FOLDS <- 5L
GRID_SIZE <- 32L
MAX_BOUNDARY_EXPANSIONS <- 8L
TREE_RANGE <- c(650L, 2000L)
TREE_DEPTH_RANGE <- c(1L, 16L)
LEARN_RATE_RANGE <- c(1.5e-3, 0.06)
MIN_N_RANGE <- c(5L, 75L)
LOSS_REDUCTION_RANGE <- c(1e-5, 10)
SAMPLE_SIZE_RANGE <- c(0.62, 1.0)

derive_season_start <- function(game_id) {
  as.integer(substr(as.character(game_id), 1, 4))
}

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

load_model_data <- function(dataset) {
  data_path <- file.path("models", "xG", "data", paste0(dataset, "_train.csv"))
  model_data <- readr::read_csv(data_path, show_col_types = FALSE) %>%
    dplyr::mutate(seasonStart = derive_season_start(gameId)) %>%
    dplyr::filter(seasonStart %in% TRAIN_SEASON_STARTS)

  if (!all(TRAIN_SEASON_STARTS %in% unique(model_data$seasonStart))) {
    stop("Expected xG training data for 2023-24 and 2024-25 only.")
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
    recipes::update_role(gameId, eventId, seasonStart, new_role = "id") %>%
    recipes::step_string2factor(recipes::all_nominal_predictors()) %>%
    recipes::step_unknown(recipes::all_nominal_predictors()) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_zv(recipes::all_predictors())
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

run_xgboost_training <- function(dataset) {
  model_key <- paste0(dataset, "1")
  results_dir <- file.path("models", "xG", "results")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

  model_data <- load_model_data(dataset)

  recipe <- build_recipe(model_data)
  predictor_count <- count_recipe_predictors(recipe, model_data)

  spec <- parsnip::boost_tree(
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune(),
    min_n = tune::tune(),
    loss_reduction = tune::tune(),
    sample_size = tune::tune(),
    mtry = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine(
      "xgboost",
      objective = "binary:logistic",
      eval_metric = "logloss",
      tree_method = "hist",
      nthread = 1
    )

  workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec)

  mtry_upper <- max(1L, min(45L, predictor_count))
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

  current_bounds <- list(
    trees = TREE_RANGE,
    tree_depth = TREE_DEPTH_RANGE,
    learn_rate = LEARN_RATE_RANGE,
    min_n = MIN_N_RANGE,
    loss_reduction = LOSS_REDUCTION_RANGE,
    sample_size = SAMPLE_SIZE_RANGE,
    mtry = c(1L, mtry_upper)
  )
  boundary_checks <- list(
    trees = list(lower = TRUE, upper = TRUE),
    tree_depth = list(lower = FALSE, upper = TRUE),
    learn_rate = list(lower = TRUE, upper = TRUE),
    min_n = list(lower = FALSE, upper = TRUE),
    loss_reduction = list(lower = TRUE, upper = TRUE),
    sample_size = list(lower = TRUE, upper = FALSE),
    mtry = list(lower = FALSE, upper = FALSE)
  )
  boundary_expanders <- list(
    trees = function(range, direction) expand_integer_range(range, direction, lower_step = 250L, upper_step = 500L, min_value = 200L, max_value = 6000L),
    tree_depth = function(range, direction) expand_integer_range(range, direction, lower_step = 2L, upper_step = 6L, min_value = 1L, max_value = 32L),
    learn_rate = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.5, upper_factor = 2, min_value = 1e-4, max_value = 0.3),
    min_n = function(range, direction) expand_integer_range(range, direction, lower_step = 5L, upper_step = 25L, min_value = 2L, max_value = 200L),
    loss_reduction = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.1, upper_factor = 4, min_value = 1e-8, max_value = 1e3),
    sample_size = function(range, direction) expand_numeric_range(range, direction, lower_factor = 0.8, upper_factor = 1.05, min_value = 0.3, max_value = 1.0),
    mtry = function(range, direction) range
  )
  accepted_boundary_caps <- list(
    trees = list(lower = 200L, upper = 6000L),
    tree_depth = list(upper = 32L),
    learn_rate = list(lower = 1e-4, upper = 0.3),
    min_n = list(upper = 200L),
    loss_reduction = list(lower = 1e-8, upper = 1e3),
    sample_size = list(lower = 0.3)
  )
  boundary_expansion_count <- 0L
  search_history <- tibble::tibble()
  simplicity_preferences <- list(
    tree_depth = "asc",
    trees = "asc",
    mtry = "asc",
    sample_size = "asc",
    min_n = "desc",
    loss_reduction = "desc"
  )

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
      dplyr::filter(.metric == "mn_log_loss")
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
    best_metrics <- metric_results %>%
      dplyr::arrange(mean, std_err) %>%
      dplyr::slice_head(n = 10)
    best_log_loss <- selected_metrics$mean[[1]]
    raw_best_params <- raw_best_metrics %>%
      dplyr::select(dplyr::all_of(tuning_param_columns(raw_best_metrics)))

    if (selected_metrics$.config[[1]] != raw_best_metrics$.config[[1]]) {
      message(
        sprintf(
          paste(
            "Selecting one-SE XGBoost candidate %s (mn_log_loss=%.6f)",
            "instead of boundary/raw-best candidate %s (mn_log_loss=%.6f)."
          ),
          selected_metrics$.config[[1]],
          selected_metrics$mean[[1]],
          raw_best_metrics$.config[[1]],
          raw_best_metrics$mean[[1]]
        )
      )
    }

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
        raw_best_mn_log_loss = raw_best_metrics$mean[[1]],
        raw_best_std_err = raw_best_metrics$std_err[[1]],
        raw_best_boundary_hits = if (nrow(raw_boundary_hits) == 0L) "none" else format_boundary_hits(raw_boundary_hits),
        selected_config = selected_metrics$.config[[1]],
        selected_mn_log_loss = selected_metrics$mean[[1]],
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

    if (nrow(accepted_hits) > 0L) {
      message(
        sprintf(
          "Accepting XGBoost boundary hits at configured caps for %s.",
          format_boundary_hits(accepted_hits)
        )
      )
    }

    if (nrow(expansion_hits) == 0L) {
      break
    }

    boundary_expansion_count <- boundary_expansion_count + 1L

    if (boundary_expansion_count > MAX_BOUNDARY_EXPANSIONS) {
      stop(
        paste(
          "Best parameters kept hitting tuning boundaries after",
          MAX_BOUNDARY_EXPANSIONS,
          "expansions:",
          format_boundary_hits(expansion_hits)
        )
      )
    }

    next_bounds <- expand_bounds(current_bounds, expansion_hits, boundary_expanders)

    if (identical(next_bounds, current_bounds)) {
      stop(
        paste(
          "Unable to expand tuning boundaries for:",
          format_boundary_hits(expansion_hits)
        )
      )
    }

    message(
      sprintf(
        "Expanding XGBoost tuning bounds for %s and retuning.",
        format_boundary_hits(expansion_hits)
      )
    )

    current_bounds <- next_bounds
  }

  training_summary <- tibble::tibble(
    seasons = paste(sort(unique(model_data$seasonStart)), collapse = ","),
    games = dplyr::n_distinct(model_data$gameId),
    rows = nrow(model_data),
    goal_rate = mean(model_data$isGoal == "goal")
  )

  final_workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec) %>%
    tune::finalize_workflow(best_params) %>%
    parsnip::fit(model_data)

  importance <- xgboost::xgb.importance(
    model = workflows::extract_fit_parsnip(final_workflow)$fit
  ) %>%
    tibble::as_tibble() %>%
    dplyr::slice_max(order_by = Gain, n = 20) %>%
    dplyr::arrange(Gain) %>%
    dplyr::mutate(Feature = factor(Feature, levels = Feature))

  importance_plot <- ggplot2::ggplot(
    importance,
    ggplot2::aes(x = Gain, y = Feature)
  ) +
    ggplot2::geom_col(fill = "#1b6ca8") +
    ggplot2::labs(
      title = sprintf("%s XGBoost Feature Importance", toupper(model_key)),
      x = "Gain",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)

  cv_metrics <- tune::collect_metrics(tuned)
  selection_summary <- tibble::tibble(
    selection_rule = dplyr::if_else(
      selected_metrics$.config[[1]] == raw_best_metrics$.config[[1]],
      "raw_best",
      "one_se_simpler_candidate"
    ),
    selected_config = selected_metrics$.config[[1]],
    selected_mn_log_loss = selected_metrics$mean[[1]],
    selected_std_err = selected_metrics$std_err[[1]],
    raw_best_config = raw_best_metrics$.config[[1]],
    raw_best_mn_log_loss = raw_best_metrics$mean[[1]],
    raw_best_std_err = raw_best_metrics$std_err[[1]],
    one_se_threshold = selection$one_se_threshold[[1]],
    one_se_candidate_count = nrow(selection$candidates)
  )

  readr::write_csv(best_params, file.path(results_dir, paste0(model_key, "_best_params.csv")))
  readr::write_csv(cv_metrics, file.path(results_dir, paste0(model_key, "_cv_metrics.csv")))
  readr::write_csv(training_summary, file.path(results_dir, paste0(model_key, "_training_summary.csv")))
  readr::write_csv(selection_summary, file.path(results_dir, paste0(model_key, "_selection_summary.csv")))
  readr::write_csv(search_history, file.path(results_dir, paste0(model_key, "_search_history.csv")))
  ggplot2::ggsave(
    filename = file.path(results_dir, paste0(model_key, "_importance.png")),
    plot = importance_plot,
    width = 8,
    height = 6,
    dpi = 150
  )

  saveRDS(final_workflow, file.path("models", "xG", paste0(model_key, ".rds")))
  saveRDS(
    list(
      dataset = dataset,
      engine = "xgboost",
      training_summary = training_summary,
      best_params = best_params,
      best_cv = best_metrics,
      raw_best_cv = raw_best_metrics,
      selected_cv = selected_metrics,
      selection_summary = selection_summary,
      search_history = search_history,
      cv_metrics = cv_metrics,
      feature_importance = importance
    ),
    file.path(results_dir, paste0(model_key, "_results.rds"))
  )

  print(training_summary)
  print(selection_summary)
  print(best_metrics)
  print(importance_plot)
  cat(sprintf("Best grouped CV mn_log_loss: %.6f\n", best_log_loss))
  cat(sprintf("Saved fitted workflow to models/xG/%s.rds\n", model_key))
  cat(sprintf("Saved training results to models/xG/results/%s_results.rds\n", model_key))
}
