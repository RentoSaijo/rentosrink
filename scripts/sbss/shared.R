suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nhlscraper))
suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(bonsai))

tidymodels::tidymodels_prefer()

source(file.path("models", "xG", "prepare.R"))

set.seed(20060527)

XG_DATASETS <- c("sd", "ev", "pp", "sh", "en", "ps")
SBSS_PARTS <- 5L
CURRENT_V3_ENGINES <- c(
  sd = "xgboost",
  ev = "lightgbm",
  pp = "xgboost",
  sh = "lightgbm",
  en = "lightgbm",
  ps = "xgboost"
)
CURRENT_V3_MODEL_KEYS <- c(
  sd = "sd1",
  ev = "ev2",
  pp = "pp1",
  sh = "sh2",
  en = "en2",
  ps = "ps1"
)
LEGACY_V1_PAIR <- c(20102011L, 20112012L)
CURRENT_V3_PAIR <- c(20232024L, 20242025L)

SBSS_CACHE <- new.env(parent = emptyenv())

load_training_env <- function(path) {
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  env
}

SBSS_CURRENT_XGB_ENV <- load_training_env(file.path("models", "xG", "train1.R"))
SBSS_CURRENT_LGB_ENV <- load_training_env(file.path("models", "xG", "train2.R"))
SBSS_LEGACY_ENV <- load_training_env(file.path("models", "xG", "legacy", "train.R"))

cached_read_csv <- function(path) {
  key <- paste0("csv::", normalizePath(path, winslash = "/", mustWork = TRUE))

  if (!exists(key, envir = SBSS_CACHE, inherits = FALSE)) {
    assign(
      key,
      readr::read_csv(path, show_col_types = FALSE),
      envir = SBSS_CACHE
    )
  }

  get(key, envir = SBSS_CACHE, inherits = FALSE)
}

cached_read_rds <- function(path) {
  key <- paste0("rds::", normalizePath(path, winslash = "/", mustWork = TRUE))

  if (!exists(key, envir = SBSS_CACHE, inherits = FALSE)) {
    assign(key, readRDS(path), envir = SBSS_CACHE)
  }

  get(key, envir = SBSS_CACHE, inherits = FALSE)
}

season_start_from_season_id <- function(season_id) {
  as.integer(substr(as.character(season_id), 1, 4))
}

season_start_from_game_id <- function(game_id) {
  as.integer(substr(as.character(game_id), 1, 4))
}

normalize_situation_code <- function(x) {
  out <- suppressWarnings(as.integer(as.character(x)))
  out <- ifelse(is.na(out), NA_character_, sprintf("%04d", out))
  out
}

is_regular_season_shootout <- function(game_type_id, period_number) {
  !is.na(game_type_id) & !is.na(period_number) &
    game_type_id == 2L & period_number == 5L
}

is_penalty_shot_attempt <- function(situation_code, game_type_id, period_number) {
  sc <- normalize_situation_code(situation_code)
  !is.na(sc) & sc %in% c("0101", "1010") &
    !is_regular_season_shootout(game_type_id, period_number)
}

normalize_output_strength_state <- function(strength_state, situation_code, game_type_id, period_number) {
  out <- as.character(strength_state)
  sc <- normalize_situation_code(situation_code)
  out[is.na(sc)] <- "even-strength"
  out[is_penalty_shot_attempt(situation_code, game_type_id, period_number)] <- "even-strength"
  out
}

score_dataset_key <- function(situation) {
  dplyr::case_when(
    situation == "ps" ~ "ps",
    TRUE ~ situation
  )
}

factorize_logical_predictors <- function(data, exclude = "isGoal") {
  logical_predictors <- setdiff(
    names(data)[vapply(data, is.logical, logical(1))],
    exclude
  )

  data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(logical_predictors),
        ~ factor(dplyr::if_else(.x, "yes", "no"), levels = c("no", "yes"))
      )
    )
}

prepare_current_training_frame <- function(data) {
  data %>%
    dplyr::mutate(
      seasonStart = season_start_from_game_id(gameId),
      isGoal = dplyr::if_else(isGoal, "goal", "no_goal"),
      isGoal = factor(isGoal, levels = c("no_goal", "goal"))
    ) %>%
    factorize_logical_predictors()
}

prepare_current_scoring_frame <- function(data) {
  data %>%
    dplyr::mutate(seasonStart = season_start_from_game_id(gameId)) %>%
    factorize_logical_predictors()
}

prepare_legacy_training_frame <- function(data) {
  data %>%
    dplyr::mutate(
      season = season_id_from_game_id(gameId),
      isGoal = dplyr::if_else(isGoal, "goal", "no_goal"),
      isGoal = factor(isGoal, levels = c("no_goal", "goal"))
    ) %>%
    factorize_logical_predictors()
}

prepare_legacy_scoring_frame <- function(data) {
  data %>%
    dplyr::mutate(season = season_id_from_game_id(gameId)) %>%
    factorize_logical_predictors()
}

load_current_training_csv <- function(dataset) {
  cached_read_csv(file.path("models", "xG", "data", paste0(dataset, "_train.csv")))
}

load_legacy_training_csv <- function(dataset, version = 1L) {
  cached_read_csv(
    file.path("models", "xG", "legacy", "data", paste0(dataset, version, "_train.csv"))
  )
}

load_saved_current_v3_models <- function(datasets = names(CURRENT_V3_MODEL_KEYS)) {
  purrr::map(
    CURRENT_V3_MODEL_KEYS[datasets],
    ~ cached_read_rds(file.path("models", "xG", paste0(.x, ".rds")))
  )
}

load_saved_legacy_models <- function(version, datasets = XG_DATASETS) {
  purrr::map(
    datasets,
    ~ cached_read_rds(file.path("models", "xG", "legacy", paste0(.x, version, ".rds")))
  ) %>%
    rlang::set_names(datasets)
}

load_current_best_params <- function(dataset) {
  model_key <- CURRENT_V3_MODEL_KEYS[[dataset]]
  cached_read_csv(file.path("models", "xG", "results", paste0(model_key, "_best_params.csv"))) %>%
    dplyr::slice(1)
}

build_current_spec <- function(dataset, engine, best_params) {
  if (engine == "xgboost") {
    return(
      parsnip::boost_tree(
        mode = "classification",
        trees = as.integer(best_params$trees[[1]]),
        tree_depth = as.integer(best_params$tree_depth[[1]]),
        learn_rate = as.numeric(best_params$learn_rate[[1]]),
        min_n = as.integer(best_params$min_n[[1]]),
        loss_reduction = as.numeric(best_params$loss_reduction[[1]]),
        sample_size = as.numeric(best_params$sample_size[[1]]),
        mtry = as.integer(best_params$mtry[[1]])
      ) %>%
        parsnip::set_engine(
          "xgboost",
          objective = "binary:logistic",
          eval_metric = "logloss",
          tree_method = "hist",
          nthread = 1
        )
    )
  }

  parsnip::boost_tree(
    mode = "classification",
    trees = as.integer(best_params$trees[[1]]),
    tree_depth = as.integer(best_params$tree_depth[[1]]),
    learn_rate = as.numeric(best_params$learn_rate[[1]]),
    min_n = as.integer(best_params$min_n[[1]]),
    loss_reduction = as.numeric(best_params$loss_reduction[[1]]),
    sample_size = as.numeric(best_params$sample_size[[1]]),
    mtry = as.numeric(best_params$mtry[[1]])
  ) %>%
    parsnip::set_engine(
      "lightgbm",
      num_threads = 1,
      verbose = -1,
      counts = FALSE
    )
}

fit_current_refit_model <- function(dataset, training_data) {
  engine <- CURRENT_V3_ENGINES[[dataset]]
  best_params <- load_current_best_params(dataset)
  recipe <- if (engine == "xgboost") {
    SBSS_CURRENT_XGB_ENV$build_recipe(training_data)
  } else {
    SBSS_CURRENT_LGB_ENV$build_recipe(training_data)
  }

  spec <- build_current_spec(dataset, engine, best_params)

  workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec) %>%
    parsnip::fit(training_data)
}

fit_legacy_ridge_model <- function(training_data) {
  recipe <- SBSS_LEGACY_ENV$build_recipe(training_data)

  spec <- parsnip::logistic_reg(
    mode = "classification",
    penalty = tune::tune(),
    mixture = 0
  ) %>%
    parsnip::set_engine("glmnet")

  workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec)

  fold_count <- max(
    2L,
    min(SBSS_LEGACY_ENV$N_FOLDS, dplyr::n_distinct(training_data$gameId))
  )

  cluster_state <- SBSS_LEGACY_ENV$create_cluster()
  on.exit(SBSS_LEGACY_ENV$stop_cluster(cluster_state), add = TRUE)

  current_penalty_range <- SBSS_LEGACY_ENV$PENALTY_RANGE
  expansion_count <- 0L

  repeat {
    penalty_param <- dials::penalty(range = current_penalty_range)
    tuning_grid <- dials::grid_regular(penalty_param, levels = SBSS_LEGACY_ENV$GRID_LEVELS)

    tuned <- tune::tune_grid(
      workflow,
      resamples = rsample::group_vfold_cv(
        training_data,
        group = gameId,
        v = fold_count,
        balance = "observations"
      ),
      grid = tuning_grid,
      metrics = yardstick::metric_set(
        yardstick::mn_log_loss,
        SBSS_LEGACY_ENV$goal_roc_auc,
        SBSS_LEGACY_ENV$goal_pr_auc,
        yardstick::brier_class
      ),
      control = tune::control_grid(
        verbose = FALSE,
        allow_par = cluster_state$n_workers > 1L,
        parallel_over = "resamples",
        save_pred = FALSE
      )
    )

    best_params <- tune::select_best(tuned, metric = "mn_log_loss")
    boundary_hit <- SBSS_LEGACY_ENV$penalty_boundary_hit(
      best_params,
      current_penalty_range
    )

    if (!is.na(boundary_hit) &&
      boundary_hit == "lower" &&
      current_penalty_range[[1]] <= SBSS_LEGACY_ENV$LOWER_BOUND_ACCEPT_LOG10) {
      break
    }

    if (is.na(boundary_hit)) {
      break
    }

    expansion_count <- expansion_count + 1L

    if (expansion_count > SBSS_LEGACY_ENV$MAX_PENALTY_EXPANSIONS) {
      stop("Legacy ridge penalty kept hitting the search boundary.")
    }

    current_penalty_range <- SBSS_LEGACY_ENV$expand_penalty_range(
      current_penalty_range,
      boundary_hit
    )
  }

  workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec) %>%
    tune::finalize_workflow(best_params) %>%
    parsnip::fit(training_data)
}

score_partition_frames <- function(partitions, models, model_family) {
  purrr::imap_dfr(
    partitions,
    function(data, dataset) {
      if (nrow(data) == 0L) {
        return(tibble::tibble())
      }

      scoring_data <- if (model_family == "legacy") {
        prepare_legacy_scoring_frame(data)
      } else {
        prepare_current_scoring_frame(data)
      }

      preds <- predict(models[[dataset]], new_data = scoring_data, type = "prob")

      tibble::tibble(
        gameId = as.integer(data$gameId),
        eventId = as.integer(data$eventId),
        xG = as.numeric(preds$.pred_goal)
      )
    }
  )
}

make_game_parts <- function(data, n_parts = SBSS_PARTS) {
  games <- data %>%
    dplyr::distinct(gameId) %>%
    dplyr::arrange(gameId)

  if (nrow(games) == 0L) {
    return(games %>% dplyr::mutate(part = integer()))
  }

  games %>%
    dplyr::mutate(part = dplyr::ntile(dplyr::row_number(), n_parts))
}

score_legacy_saved <- function(eligible_attempts, version) {
  partitions <- build_xg_partitions(eligible_attempts)
  needed_datasets <- names(partitions)[vapply(partitions, nrow, integer(1)) > 0L]
  models <- load_saved_legacy_models(version, datasets = needed_datasets)
  score_partition_frames(partitions, models, model_family = "legacy")
}

score_current_saved_v3 <- function(eligible_attempts) {
  partitions <- build_xg_partitions(eligible_attempts)
  needed_datasets <- names(partitions)[vapply(partitions, nrow, integer(1)) > 0L]
  models <- load_saved_current_v3_models(datasets = needed_datasets)
  score_partition_frames(partitions, models, model_family = "current")
}

score_legacy_crossfit <- function(eligible_attempts, season_id) {
  parts_tbl <- make_game_parts(eligible_attempts)
  eligible_attempts <- eligible_attempts %>%
    dplyr::left_join(parts_tbl, by = "gameId")

  other_season <- setdiff(LEGACY_V1_PAIR, season_id)

  purrr::map_dfr(
    sort(unique(parts_tbl$part)),
    function(part_id) {
      cat(sprintf("legacy crossfit %s part %d/%d\n", season_id, part_id, SBSS_PARTS))
      flush.console()

      holdout <- eligible_attempts %>%
        dplyr::filter(part == part_id)

      if (nrow(holdout) == 0L) {
        return(tibble::tibble())
      }

      holdout_games <- sort(unique(holdout$gameId))
      holdout_partitions <- build_xg_partitions(holdout)
      needed_datasets <- names(holdout_partitions)[
        vapply(holdout_partitions, nrow, integer(1)) > 0L
      ]

      purrr::map_dfr(
        needed_datasets,
        function(dataset) {
          cat(sprintf("  fitting legacy dataset %s\n", dataset))
          flush.console()

          raw_training <- load_legacy_training_csv(dataset, version = 1L) %>%
            dplyr::mutate(season = season_id_from_game_id(gameId)) %>%
            dplyr::filter(
              season == other_season |
                !(season == season_id & gameId %in% holdout_games)
            ) %>%
            dplyr::select(-season)

          model <- fit_legacy_ridge_model(
            prepare_legacy_training_frame(raw_training)
          )
          partition_data <- holdout_partitions[[dataset]]
          preds <- predict(
            model,
            new_data = prepare_legacy_scoring_frame(partition_data),
            type = "prob"
          )

          tibble::tibble(
            gameId = as.integer(partition_data$gameId),
            eventId = as.integer(partition_data$eventId),
            xG = as.numeric(preds$.pred_goal)
          )
        }
      )
    }
  )
}

score_current_crossfit_v3 <- function(eligible_attempts, season_id) {
  parts_tbl <- make_game_parts(eligible_attempts)
  eligible_attempts <- eligible_attempts %>%
    dplyr::left_join(parts_tbl, by = "gameId")

  season_start <- season_start_from_season_id(season_id)
  other_start <- setdiff(season_start_from_season_id(CURRENT_V3_PAIR), season_start)

  purrr::map_dfr(
    sort(unique(parts_tbl$part)),
    function(part_id) {
      cat(sprintf("current v3 crossfit %s part %d/%d\n", season_id, part_id, SBSS_PARTS))
      flush.console()

      holdout <- eligible_attempts %>%
        dplyr::filter(part == part_id)

      if (nrow(holdout) == 0L) {
        return(tibble::tibble())
      }

      holdout_games <- sort(unique(holdout$gameId))
      holdout_partitions <- build_xg_partitions(holdout)
      needed_datasets <- names(holdout_partitions)[
        vapply(holdout_partitions, nrow, integer(1)) > 0L
      ]

      purrr::map_dfr(
        needed_datasets,
        function(dataset) {
          cat(sprintf("  refitting current dataset %s\n", dataset))
          flush.console()

          raw_training <- load_current_training_csv(dataset) %>%
            dplyr::mutate(seasonStart = season_start_from_game_id(gameId)) %>%
            dplyr::filter(
              seasonStart == other_start |
                !(seasonStart == season_start & gameId %in% holdout_games)
            ) %>%
            dplyr::select(-seasonStart)

          model <- fit_current_refit_model(
            dataset,
            prepare_current_training_frame(raw_training)
          )
          partition_data <- holdout_partitions[[dataset]]
          preds <- predict(
            model,
            new_data = prepare_current_scoring_frame(partition_data),
            type = "prob"
          )

          tibble::tibble(
            gameId = as.integer(partition_data$gameId),
            eventId = as.integer(partition_data$eventId),
            xG = as.numeric(preds$.pred_goal)
          )
        }
      )
    }
  )
}

score_season_xg <- function(eligible_attempts, season_id) {
  if (nrow(eligible_attempts) == 0L) {
    return(tibble::tibble(gameId = integer(), eventId = integer(), xG = double()))
  }

  if (season_id %in% LEGACY_V1_PAIR) {
    return(score_legacy_crossfit(eligible_attempts, season_id))
  }

  if (season_id >= 20122013L && season_id <= 20172018L) {
    return(score_legacy_saved(eligible_attempts, version = 1L))
  }

  if (season_id >= 20182019L && season_id <= 20222023L) {
    return(score_legacy_saved(eligible_attempts, version = 2L))
  }

  if (season_id %in% CURRENT_V3_PAIR) {
    return(score_current_crossfit_v3(eligible_attempts, season_id))
  }

  if (season_id == 20252026L) {
    return(score_current_saved_v3(eligible_attempts))
  }

  stop(sprintf("Unsupported sbss season: %s", season_id))
}

prepare_sbss_shot_attempts <- function(season_id) {
  pbps <- load_xg_season(season_id)
  pbps <- normalize_xg_pbp_schema(pbps)

  goalie_ids <- pbps %>%
    dplyr::distinct(goaliePlayerIdAgainst) %>%
    dplyr::filter(!is.na(goaliePlayerIdAgainst)) %>%
    dplyr::pull(goaliePlayerIdAgainst) %>%
    as.integer()

  prev_events <- pbps %>%
    dplyr::transmute(
      gameId,
      eventId,
      typeDescKeyPrevRaw = eventTypeDescKey,
      reasonPrev = reason,
      shotTypePrev = shotType,
      eventOwnerTeamIdPrev = eventOwnerTeamId
    )

  attempts <- pbps %>%
    dplyr::filter(
      gameTypeId %in% 2:3,
      eventTypeDescKey %in% c("goal", "shot-on-goal", "missed-shot", "blocked-shot")
    ) %>%
    dplyr::left_join(
      prev_events,
      by = c("gameId", "eventIdPrev" = "eventId")
    ) %>%
    add_shift_list_columns() %>%
    dplyr::mutate(
      situationCode = normalize_situation_code(situationCode),
      periodType = as.character(periodType),
      isEmptyNetFor = dplyr::coalesce(isEmptyNetFor, FALSE),
      isEmptyNetAgainst = dplyr::coalesce(isEmptyNetAgainst, FALSE),
      shootingPlayerId = dplyr::coalesce(shootingPlayerId, scoringPlayerId),
      strengthStateOutput = normalize_output_strength_state(
        strengthState,
        situationCode,
        gameTypeId,
        periodNumber
      ),
      isShootout = is_regular_season_shootout(gameTypeId, periodNumber),
      typeDescKeyPrev = make_type_desc_key_prev(
        type_desc_key_prev = typeDescKeyPrevRaw,
        reason_prev = reasonPrev,
        shot_type_prev = shotTypePrev,
        event_owner_team_id_prev = eventOwnerTeamIdPrev,
        event_owner_team_id = eventOwnerTeamId
      )
    ) %>%
    append_xg_situation_columns() %>%
    dplyr::filter(is.na(shootingPlayerId) | !(shootingPlayerId %in% goalie_ids))

  required_shift_cols <- c(
    "playerIdsFor",
    "playerIdsAgainst",
    "secondsElapsedInShiftFor",
    "secondsElapsedInShiftAgainst",
    "secondsElapsedInPeriodSinceLastShiftFor",
    "secondsElapsedInPeriodSinceLastShiftAgainst",
    "dYCoordNorm"
  )

  missing_shift_cols <- setdiff(required_shift_cols, names(attempts))
  if (length(missing_shift_cols) > 0L) {
    stop(
      paste(
        "Missing required sbss feature columns:",
        paste(missing_shift_cols, collapse = ", ")
      )
    )
  }

  shift_elapsed_for_skater <- purrr::map2(
    attempts$playerIdsFor,
    attempts$secondsElapsedInShiftFor,
    aligned_skater_values,
    goalie_ids = goalie_ids
  )

  shift_elapsed_against_skater <- purrr::map2(
    attempts$playerIdsAgainst,
    attempts$secondsElapsedInShiftAgainst,
    aligned_skater_values,
    goalie_ids = goalie_ids
  )

  shift_rest_for_skater <- purrr::map2(
    attempts$playerIdsFor,
    attempts$secondsElapsedInPeriodSinceLastShiftFor,
    aligned_skater_values,
    goalie_ids = goalie_ids
  )

  shift_rest_against_skater <- purrr::map2(
    attempts$playerIdsAgainst,
    attempts$secondsElapsedInPeriodSinceLastShiftAgainst,
    aligned_skater_values,
    goalie_ids = goalie_ids
  )

  attempts <- attempts %>%
    dplyr::mutate(
      isGoal = eventTypeDescKey == "goal",
      isPlayoff = gameTypeId == 3,
      isOvertime = periodType == "OT",
      isBehindNet = is_behind_net(xCoordNorm),
      crossedRoyalRoad = is_royal_road(yCoordNorm, dYCoordNorm),
      minSecondsElapsedInShiftFor = purrr::map_dbl(shift_elapsed_for_skater, safe_min_numeric),
      maxSecondsElapsedInShiftFor = purrr::map_dbl(shift_elapsed_for_skater, safe_max_numeric),
      avgSecondsElapsedInShiftFor = purrr::map_dbl(shift_elapsed_for_skater, safe_mean_numeric),
      minSecondsElapsedInShiftAgainst = purrr::map_dbl(shift_elapsed_against_skater, safe_min_numeric),
      maxSecondsElapsedInShiftAgainst = purrr::map_dbl(shift_elapsed_against_skater, safe_max_numeric),
      avgSecondsElapsedInShiftAgainst = purrr::map_dbl(shift_elapsed_against_skater, safe_mean_numeric),
      minSecondsElapsedSinceLastShiftFor = purrr::map_dbl(shift_rest_for_skater, safe_min_numeric),
      maxSecondsElapsedSinceLastShiftFor = purrr::map_dbl(shift_rest_for_skater, safe_max_numeric),
      avgSecondsElapsedSinceLastShiftFor = purrr::map_dbl(shift_rest_for_skater, safe_mean_numeric),
      minSecondsElapsedSinceLastShiftAgainst = purrr::map_dbl(shift_rest_against_skater, safe_min_numeric),
      maxSecondsElapsedSinceLastShiftAgainst = purrr::map_dbl(shift_rest_against_skater, safe_max_numeric),
      avgSecondsElapsedSinceLastShiftAgainst = purrr::map_dbl(shift_rest_against_skater, safe_mean_numeric),
      shooterSecondsElapsedInShift = purrr::pmap_dbl(
        list(playerIdsFor, secondsElapsedInShiftFor, shootingPlayerId),
        extract_aligned_player_value
      ),
      shooterSecondsElapsedSinceLastShift = purrr::pmap_dbl(
        list(
          playerIdsFor,
          secondsElapsedInPeriodSinceLastShiftFor,
          shootingPlayerId
        ),
        extract_aligned_player_value
      ),
      isCorsi = eventTypeDescKey %in% c("blocked-shot", "missed-shot", "shot-on-goal", "goal"),
      isFenwick = eventTypeDescKey %in% c("missed-shot", "shot-on-goal", "goal"),
      isShot = eventTypeDescKey %in% c("shot-on-goal", "goal")
    )

  if (any(attempts$n_situations != 1, na.rm = TRUE) || any(is.na(attempts$n_situations))) {
    print(attempts %>% dplyr::count(n_situations, sort = TRUE))
    stop("sbss situation definitions are not mutually exclusive and collectively exhaustive.")
  }

  attempts
}

build_scored_shot_attempts_from_attempts <- function(attempts, season_id) {
  attempts <- attempts %>%
    dplyr::filter(!isShootout)
  eligible_attempts <- attempts %>%
    dplyr::filter(isFenwick)

  xg_predictions <- score_season_xg(eligible_attempts, season_id) %>%
    dplyr::distinct(gameId, eventId, .keep_all = TRUE) %>%
    dplyr::rename(predictedXG = xG)

  scored <- attempts %>%
    dplyr::left_join(xg_predictions, by = c("gameId", "eventId"))

  missing_fenwick <- scored %>%
    dplyr::filter(isFenwick, is.na(predictedXG))

  if (nrow(missing_fenwick) > 0L) {
    stop("Missing xG predictions for one or more fenwick sbss rows.")
  }

  scored %>%
    dplyr::mutate(
      xG = dplyr::if_else(isFenwick, predictedXG, 0),
      xG = as.numeric(xG)
    ) %>%
    dplyr::select(-predictedXG)
}

build_scored_shot_attempts <- function(season_id) {
  attempts <- prepare_sbss_shot_attempts(season_id)
  build_scored_shot_attempts_from_attempts(attempts, season_id)
}

build_goalie_team_lookup <- function(scored_attempts) {
  if (nrow(scored_attempts) == 0L) {
    return(tibble::tibble(
      gameId = integer(),
      goaliePlayerId = integer(),
      goalieTeamId = integer()
    ))
  }

  games_lookup <- nhlscraper::games() %>%
    dplyr::filter(gameId %in% unique(as.integer(scored_attempts$gameId))) %>%
    dplyr::transmute(
      gameId = as.integer(gameId),
      homeTeamId = as.integer(homeTeamId),
      visitingTeamId = as.integer(visitingTeamId)
    )

  dplyr::bind_rows(
    scored_attempts %>%
      dplyr::transmute(
        gameId = as.integer(gameId),
        goaliePlayerId = as.integer(homeGoaliePlayerId)
      ) %>%
      dplyr::filter(!is.na(goaliePlayerId)) %>%
      dplyr::distinct() %>%
      dplyr::left_join(games_lookup, by = "gameId") %>%
      dplyr::transmute(gameId, goaliePlayerId, goalieTeamId = homeTeamId),
    scored_attempts %>%
      dplyr::transmute(
        gameId = as.integer(gameId),
        goaliePlayerId = as.integer(awayGoaliePlayerId)
      ) %>%
      dplyr::filter(!is.na(goaliePlayerId)) %>%
      dplyr::distinct() %>%
      dplyr::left_join(games_lookup, by = "gameId") %>%
      dplyr::transmute(gameId, goaliePlayerId, goalieTeamId = visitingTeamId)
  ) %>%
    dplyr::distinct(gameId, goaliePlayerId, .keep_all = TRUE)
}

sbss_expected_event_keys <- function(attempts, player_col) {
  attempts %>%
    dplyr::filter(!isShootout) %>%
    dplyr::filter(!is.na(.data[[player_col]])) %>%
    dplyr::transmute(
      gameId = as.integer(gameId),
      eventId = as.integer(eventId)
    ) %>%
    dplyr::distinct()
}

sbss_existing_event_keys <- function(existing) {
  existing %>%
    dplyr::transmute(
      gameId = as.integer(gameId),
      eventId = as.integer(eventId)
    ) %>%
    dplyr::distinct()
}

append_sbss_entity_rows <- function(existing, additions, id_col) {
  if (nrow(additions) == 0L) {
    return(existing %>% dplyr::arrange(.data[[id_col]], gameId, eventId))
  }

  all_cols <- union(names(existing), names(additions))

  for (col in setdiff(all_cols, names(existing))) {
    existing[[col]] <- NA
  }

  for (col in setdiff(all_cols, names(additions))) {
    additions[[col]] <- NA
  }

  dplyr::bind_rows(
    existing %>% dplyr::select(dplyr::all_of(all_cols)),
    additions %>% dplyr::select(dplyr::all_of(all_cols))
  ) %>%
    dplyr::distinct(gameId, eventId, .keep_all = TRUE) %>%
    dplyr::arrange(.data[[id_col]], gameId, eventId)
}

rebuild_skater_sbss_from_existing <- function(season_id) {
  existing_path <- file.path("data", "sbss", paste0("skaters_", season_id, ".csv"))
  if (!file.exists(existing_path)) {
    scored_attempts <- build_scored_shot_attempts(season_id)
    return(build_skater_sbss(scored_attempts))
  }

  existing <- readr::read_csv(existing_path, show_col_types = FALSE)
  attempts <- prepare_sbss_shot_attempts(season_id)
  missing_events <- sbss_expected_event_keys(attempts, "shootingPlayerId") %>%
    dplyr::anti_join(sbss_existing_event_keys(existing), by = c("gameId", "eventId"))

  if (nrow(missing_events) > 0L) {
    cat(
      sprintf(
        "Existing skater sbss for %s is missing %d events across %d games; appending only missing events.\n",
        season_id,
        nrow(missing_events),
        dplyr::n_distinct(missing_events$gameId)
      )
    )
    missing_attempts <- attempts %>%
      dplyr::semi_join(missing_events, by = c("gameId", "eventId"))
    additions <- missing_attempts %>%
      build_scored_shot_attempts_from_attempts(season_id = season_id) %>%
      build_skater_sbss()

    return(append_sbss_entity_rows(existing, additions, id_col = "shooterPlayerId"))
  }

  existing %>%
    dplyr::arrange(shooterPlayerId, gameId, eventId)
}

rebuild_goalie_sbss_from_existing <- function(season_id) {
  existing_path <- file.path("data", "sbss", paste0("goalies_", season_id, ".csv"))
  if (!file.exists(existing_path)) {
    scored_attempts <- build_scored_shot_attempts(season_id)
    return(build_goalie_sbss(scored_attempts))
  }

  existing <- readr::read_csv(existing_path, show_col_types = FALSE)
  attempts <- prepare_sbss_shot_attempts(season_id)
  missing_events <- sbss_expected_event_keys(attempts, "goaliePlayerIdAgainst") %>%
    dplyr::anti_join(sbss_existing_event_keys(existing), by = c("gameId", "eventId"))

  if (nrow(missing_events) > 0L) {
    cat(
      sprintf(
        "Existing goalie sbss for %s is missing %d events across %d games; appending only missing events.\n",
        season_id,
        nrow(missing_events),
        dplyr::n_distinct(missing_events$gameId)
      )
    )
    missing_attempts <- attempts %>%
      dplyr::semi_join(missing_events, by = c("gameId", "eventId"))
    additions <- missing_attempts %>%
      build_scored_shot_attempts_from_attempts(season_id = season_id) %>%
      build_goalie_sbss()

    return(append_sbss_entity_rows(existing, additions, id_col = "goaliePlayerId"))
  }

  existing %>%
    dplyr::arrange(goaliePlayerId, gameId, eventId)
}

build_skater_sbss <- function(scored_attempts) {
  goalie_team_lookup <- build_goalie_team_lookup(scored_attempts)

  scored_attempts %>%
    dplyr::filter(!is.na(shootingPlayerId)) %>%
    dplyr::transmute(
      shooterPlayerId = as.integer(shootingPlayerId),
      gameId = as.integer(gameId),
      eventId = as.integer(eventId),
      strengthState = as.character(strengthStateOutput),
      xCoordNorm = as.numeric(xCoordNorm),
      yCoordNorm = as.numeric(yCoordNorm),
      isRush = as.logical(isRush),
      isRebound = as.logical(isRebound),
      isCorsi = as.logical(isCorsi),
      isFenwick = as.logical(isFenwick),
      isShot = as.logical(isShot),
      isGoal = as.logical(isGoal),
      xG = as.numeric(xG),
      goaliePlayerId = as.integer(goaliePlayerIdAgainst),
      shooterTeamId = as.integer(eventOwnerTeamId)
    ) %>%
    dplyr::left_join(goalie_team_lookup, by = c("gameId", "goaliePlayerId")) %>%
    dplyr::select(
      shooterPlayerId,
      gameId,
      eventId,
      strengthState,
      xCoordNorm,
      yCoordNorm,
      isRush,
      isRebound,
      isCorsi,
      isFenwick,
      isShot,
      isGoal,
      xG,
      goaliePlayerId,
      goalieTeamId,
      shooterTeamId
    ) %>%
    dplyr::arrange(shooterPlayerId, gameId, eventId)
}

build_goalie_sbss <- function(scored_attempts) {
  goalie_team_lookup <- build_goalie_team_lookup(scored_attempts)

  scored_attempts %>%
    dplyr::filter(!is.na(goaliePlayerIdAgainst)) %>%
    dplyr::transmute(
      goaliePlayerId = as.integer(goaliePlayerIdAgainst),
      gameId = as.integer(gameId),
      eventId = as.integer(eventId),
      strengthState = as.character(strengthStateOutput),
      xCoordNorm = as.numeric(xCoordNorm),
      yCoordNorm = as.numeric(yCoordNorm),
      isRush = as.logical(isRush),
      isRebound = as.logical(isRebound),
      isCorsi = as.logical(isCorsi),
      isFenwick = as.logical(isFenwick),
      isShot = as.logical(isShot),
      isGoal = as.logical(isGoal),
      xG = as.numeric(xG),
      shooterPlayerId = as.integer(shootingPlayerId),
      shooterTeamId = as.integer(eventOwnerTeamId)
    ) %>%
    dplyr::left_join(goalie_team_lookup, by = c("gameId", "goaliePlayerId")) %>%
    dplyr::select(
      goaliePlayerId,
      gameId,
      eventId,
      strengthState,
      xCoordNorm,
      yCoordNorm,
      isRush,
      isRebound,
      isCorsi,
      isFenwick,
      isShot,
      isGoal,
      xG,
      shooterPlayerId,
      shooterTeamId,
      goalieTeamId
    ) %>%
    dplyr::arrange(goaliePlayerId, gameId, eventId)
}

write_aggregate_entity_file <- function(data, aggregate_path) {
  dir.create(dirname(aggregate_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, aggregate_path)

  invisible(NULL)
}
