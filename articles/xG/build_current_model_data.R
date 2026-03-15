suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(readr))
suppressMessages(library(stringr))
suppressMessages(library(tibble))

RESULTS_DIR <- file.path("models", "xG", "results")
OUT_DIR <- file.path("articles", "xG", "data")
UNSEEN_FUTURE_SEASON <- 20252026L

CURRENT_V3_MODELS <- tibble::tribble(
  ~dataset, ~model, ~engine,
  "sd", "sd1", "xgboost",
  "ev", "ev2", "lightgbm",
  "pp", "pp1", "xgboost",
  "sh", "sh2", "lightgbm",
  "en", "en2", "lightgbm",
  "so", "so1", "xgboost"
)

read_results_rds <- function(model_key) {
  readRDS(file.path(RESULTS_DIR, paste0(model_key, "_results.rds")))
}

dataset_label <- function(x) {
  dplyr::case_when(
    x == "sd" ~ "Standard 5v5",
    x == "ev" ~ "Non-Standard Even Strength",
    x == "pp" ~ "Power Play",
    x == "sh" ~ "Shorthanded",
    x == "en" ~ "Empty Net",
    x == "so" ~ "Shootout / Penalty Shot",
    TRUE ~ x
  )
}

engine_label <- function(x) {
  dplyr::case_when(
    x == "xgboost" ~ "XGBoost",
    x == "lightgbm" ~ "LightGBM",
    TRUE ~ x
  )
}

current_training_summary <- purrr::pmap_dfr(
  CURRENT_V3_MODELS,
  function(dataset, model, engine) {
    obj <- read_results_rds(model)
    summary_tbl <- tibble::as_tibble(obj[["training_summary"]])

    summary_tbl %>%
      dplyr::mutate(
        dataset = dataset,
        datasetLabel = dataset_label(dataset),
        model = model,
        engine = engine,
        engineLabel = engine_label(engine)
      ) %>%
      dplyr::select(
        dataset,
        datasetLabel,
        model,
        engine,
        engineLabel,
        seasons,
        games,
        rows,
        goal_rate
      )
  }
) %>%
  dplyr::mutate(
    datasetLabel = factor(
      datasetLabel,
      levels = c(
        "Standard 5v5",
        "Power Play",
        "Shorthanded",
        "Non-Standard Even Strength",
        "Empty Net",
        "Shootout / Penalty Shot"
      )
    )
  ) %>%
  dplyr::arrange(datasetLabel)

future_overall <- readr::read_csv(
  file.path(RESULTS_DIR, "compare_overall.csv"),
  show_col_types = FALSE
) %>%
  dplyr::filter(season == UNSEEN_FUTURE_SEASON) %>%
  dplyr::mutate(
    label = dplyr::case_when(
      engine == "xgboost" ~ "All XGBoost",
      engine == "lightgbm" ~ "All LightGBM",
      TRUE ~ engine
    ),
    engineLabel = engine_label(engine)
  ) %>%
  dplyr::select(
    season,
    label,
    engine,
    engineLabel,
    rows,
    goals,
    total_xg,
    goal_rate,
    xg_rate,
    log_loss,
    brier,
    roc_auc,
    pr_auc,
    calibration_ratio,
    calibration_error
  )

future_v3_overall <- readr::read_csv(
  file.path(RESULTS_DIR, "compare_v3_overall.csv"),
  show_col_types = FALSE
) %>%
  dplyr::filter(season == UNSEEN_FUTURE_SEASON) %>%
  dplyr::mutate(
    label = "Hybrid V3",
    engine = "hybrid",
    engineLabel = "Hybrid"
  ) %>%
  dplyr::select(
    season,
    label,
    engine,
    engineLabel,
    rows,
    goals,
    total_xg,
    goal_rate,
    xg_rate,
    log_loss,
    brier,
    roc_auc,
    pr_auc,
    calibration_ratio,
    calibration_error
  )

current_unseen_future_overall <- dplyr::bind_rows(future_overall, future_v3_overall) %>%
  dplyr::mutate(
    label = factor(label, levels = c("All XGBoost", "All LightGBM", "Hybrid V3"))
  ) %>%
  dplyr::arrange(label)

future_by_partition <- readr::read_csv(
  file.path(RESULTS_DIR, "compare_by_dataset.csv"),
  show_col_types = FALSE
) %>%
  dplyr::filter(season == UNSEEN_FUTURE_SEASON) %>%
  dplyr::mutate(
    datasetLabel = dataset_label(dataset),
    label = engine_label(engine),
    model = engine
  ) %>%
  dplyr::select(
    season,
    dataset,
    datasetLabel,
    model,
    engine,
    label,
    rows,
    goals,
    total_xg,
    goal_rate,
    xg_rate,
    log_loss,
    brier,
    roc_auc,
    pr_auc,
    calibration_ratio,
    calibration_error
  )

future_v3_by_partition <- readr::read_csv(
  file.path(RESULTS_DIR, "compare_v3_by_dataset.csv"),
  show_col_types = FALSE
) %>%
  dplyr::filter(season == UNSEEN_FUTURE_SEASON) %>%
  dplyr::mutate(
    datasetLabel = dataset_label(dataset),
    label = "Hybrid V3"
  ) %>%
  dplyr::select(
    season,
    dataset,
    datasetLabel,
    model,
    engine,
    label,
    rows,
    goals,
    total_xg,
    goal_rate,
    xg_rate,
    log_loss,
    brier,
    roc_auc,
    pr_auc,
    calibration_ratio,
    calibration_error
  )

current_unseen_future_by_partition <- dplyr::bind_rows(
  future_by_partition,
  future_v3_by_partition
) %>%
  dplyr::mutate(
    datasetLabel = factor(
      datasetLabel,
      levels = c(
        "Standard 5v5",
        "Non-Standard Even Strength",
        "Power Play",
        "Shorthanded",
        "Empty Net",
        "Shootout / Penalty Shot"
      )
    ),
    label = factor(label, levels = c("XGBoost", "LightGBM", "Hybrid V3"))
  ) %>%
  dplyr::arrange(datasetLabel, label)

current_feature_importance <- purrr::pmap_dfr(
  CURRENT_V3_MODELS,
  function(dataset, model, engine) {
    obj <- read_results_rds(model)
    imp <- tibble::as_tibble(obj[["feature_importance"]])

    imp %>%
      dplyr::mutate(
        dataset = dataset,
        datasetLabel = dataset_label(dataset),
        model = model,
        engine = engine,
        engineLabel = engine_label(engine)
      ) %>%
      dplyr::arrange(dplyr::desc(Gain)) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::rename(
        feature = Feature,
        gain = Gain,
        cover = Cover,
        frequency = Frequency
      ) %>%
      dplyr::select(
        dataset,
        datasetLabel,
        model,
        engine,
        engineLabel,
        rank,
        feature,
        gain,
        cover,
        frequency
      )
  }
) %>%
  dplyr::mutate(
    datasetLabel = factor(
      datasetLabel,
      levels = c(
        "Standard 5v5",
        "Non-Standard Even Strength",
        "Power Play",
        "Shorthanded",
        "Empty Net",
        "Shootout / Penalty Shot"
      )
    )
  ) %>%
  dplyr::arrange(datasetLabel, rank)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(current_training_summary, file.path(OUT_DIR, "current_training_summary.csv"))
readr::write_csv(current_unseen_future_overall, file.path(OUT_DIR, "current_unseen_future_overall.csv"))
readr::write_csv(current_unseen_future_by_partition, file.path(OUT_DIR, "current_unseen_future_by_partition.csv"))
readr::write_csv(current_feature_importance, file.path(OUT_DIR, "current_feature_importance.csv"))
