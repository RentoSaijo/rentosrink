# Build architecture summary datasets for the contract projection article.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(bonsai))
suppressMessages(library(xgboost))
suppressMessages(library(lightgbm))

id_cols <- c(
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
response_cols <- c('term', 'aavP')
core_predictors <- c(
  'ageAtSigning',
  'contractNumber',
  'prevTerm',
  'prevAAVP',
  'positionCode',
  'isResign'
)
basic_stats <- c('assists', 'blocks', 'giveaways', 'hits', 'takeaways')
advanced_stats <- c(
  'x', 'y', 'iCorsiF', 'iFenwickF', 'iSOGF', 'iGF', 'ixGF',
  'oCorsiF', 'oFenwickF', 'oSOGF', 'oGF', 'oxGF',
  'oCorsiA', 'oFenwickA', 'oSOGA', 'oGA', 'oxGA'
)
basic_offensive <- c('assists')
basic_defensive <- c('blocks', 'giveaways', 'hits', 'takeaways')
advanced_offensive <- c(
  'x', 'y', 'iCorsiF', 'iFenwickF', 'iSOGF', 'iGF', 'ixGF',
  'oCorsiF', 'oFenwickF', 'oSOGF', 'oGF', 'oxGF'
)
advanced_defensive <- c('oCorsiA', 'oFenwickA', 'oSOGA', 'oGA', 'oxGA')

delta_stem <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

label_season <- function(season_id) {
  x <- as.character(season_id)
  paste0(substr(x, 1, 4), '-', substr(x, 7, 8))
}

classify_feature_family <- function(name) {
  if (name %in% core_predictors) {
    return('Player and Contract Context')
  }
  if (grepl(
    paste0('^(', paste(c(basic_offensive, advanced_offensive), collapse = '|'), ')_2_(ev|pp|sh)_p60_avg$'),
    name
  )) {
    return('Offensive Rates')
  }
  if (grepl(
    paste0('^d(', paste(sapply(c(basic_offensive, advanced_offensive), delta_stem), collapse = '|'), ')_2_(ev|pp|sh)_p60_avg$'),
    name
  )) {
    return('Offensive Trends')
  }
  if (grepl(
    paste0('^(', paste(c(basic_defensive, advanced_defensive), collapse = '|'), ')_2_(ev|pp|sh)_p60_avg$'),
    name
  )) {
    return('Defensive Rates')
  }
  if (grepl(
    paste0('^d(', paste(sapply(c(basic_defensive, advanced_defensive), delta_stem), collapse = '|'), ')_2_(ev|pp|sh)_p60_avg$'),
    name
  )) {
    return('Defensive Trends')
  }
  if (grepl('^mP_2_(ev|pp|sh)_avg$', name)) {
    return('Usage Levels')
  }
  if (grepl('^dMP_2_(ev|pp|sh)_avg$', name)) {
    return('Usage Trends')
  }
  'Other'
}

out_dir <- 'articles/contracts/data'
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

train <- readr::read_csv('models/contracts/data/skater_contracts_train.csv', show_col_types = FALSE)
test <- readr::read_csv('models/contracts/data/skater_contracts_test.csv', show_col_types = FALSE)
validate_all <- readr::read_csv('models/contracts/data/skater_contracts_validate.csv', show_col_types = FALSE)

predictor_cols <- setdiff(names(train), c(id_cols, response_cols))
feature_families <- vapply(predictor_cols, classify_feature_family, character(1))

family_counts <- as.data.frame(table(feature_families), stringsAsFactors = FALSE)
names(family_counts) <- c('featureFamily', 'count')
family_counts <- family_counts[order(-family_counts$count, family_counts$featureFamily), ]
row.names(family_counts) <- NULL
write.csv(family_counts, file.path(out_dir, 'feature_family_counts.csv'), row.names = FALSE)

term_dist <- as.data.frame(table(train$isResign, train$term), stringsAsFactors = FALSE)
names(term_dist) <- c('isResign', 'term', 'contracts')
term_dist$isResign <- ifelse(term_dist$isResign, 'Re-sign', 'Not Re-sign')
term_dist$term <- as.integer(as.character(term_dist$term))
term_dist$contracts <- as.integer(term_dist$contracts)
totals <- aggregate(contracts ~ isResign, term_dist, sum)
names(totals)[2] <- 'scenarioTotal'
term_dist <- merge(term_dist, totals, by = 'isResign', all.x = TRUE, sort = FALSE)
term_dist$sharePct <- ifelse(term_dist$scenarioTotal > 0, 100 * term_dist$contracts / term_dist$scenarioTotal, 0)
term_dist <- term_dist[order(term_dist$isResign, term_dist$term), ]
write.csv(term_dist, file.path(out_dir, 'term_distribution_by_resign.csv'), row.names = FALSE)

scenario_grid <- expand.grid(
  scenario = c('Re-sign', 'Not Re-sign'),
  term = 1:8,
  stringsAsFactors = FALSE
)
scenario_grid$isAllowed <- ifelse(
  scenario_grid$scenario == 'Re-sign',
  TRUE,
  scenario_grid$term <= 7
)
write.csv(scenario_grid, file.path(out_dir, 'scenario_term_grid.csv'), row.names = FALSE)

scenario_volume <- aggregate(term ~ isResign, test, function(x) c(rows = length(x), terms = length(unique(x))))
scenario_volume <- data.frame(
  scenario = ifelse(scenario_volume$isResign, 'Re-sign', 'Not Re-sign'),
  rows = scenario_volume$term[, 'rows'],
  uniqueTerms = scenario_volume$term[, 'terms'],
  stringsAsFactors = FALSE
)
scenario_volume$players <- aggregate(playerId ~ isResign, test, function(x) length(unique(x)))$playerId
scenario_volume$rowsPerPlayer <- scenario_volume$rows / scenario_volume$players
scenario_volume <- scenario_volume[order(scenario_volume$scenario), ]
write.csv(scenario_volume, file.path(out_dir, 'scenario_volume.csv'), row.names = FALSE)

snapshot <- data.frame(
  metric = c(
    'Training Contracts',
    'Training Skaters',
    'Training Season Range',
    'Training Predictors',
    'Validation Contracts',
    'Validation Season',
    'Testing Scenario Rows',
    'Testing Skaters',
    'Testing Rows per Skater'
  ),
  value = c(
    nrow(train),
    length(unique(train[['playerId']])),
    paste0(label_season(min(train[['startSeasonId']])), ' to ', label_season(max(train[['startSeasonId']]))),
    length(predictor_cols),
    nrow(validate_all),
    label_season(unique(validate_all[['startSeasonId']])[1]),
    nrow(test),
    length(unique(test[['playerId']])),
    sprintf('%.0f', nrow(test) / length(unique(test[['playerId']])))
  ),
  stringsAsFactors = FALSE
)
write.csv(snapshot, file.path(out_dir, 'data_snapshot.csv'), row.names = FALSE)

# Build model comparison tables from the held-out validation split.
validate <- validate_all %>%
  dplyr::filter(!is.na(term), !is.na(aavP), !is.na(isResign)) %>%
  dplyr::mutate(term = suppressWarnings(as.integer(as.numeric(term))))
seen <- train %>%
  dplyr::filter(!is.na(term), !is.na(aavP), !is.na(isResign)) %>%
  dplyr::mutate(term = suppressWarnings(as.integer(as.numeric(term))))

version_to_model <- c(
  '1' = 'Random Forest',
  '2' = 'XGBoost',
  '3' = 'LightGBM'
)

parse_prob_term_values <- function(prob_tbl) {
  suppressWarnings(as.integer(stringr::str_extract(colnames(prob_tbl), '[0-9]+$')))
}

predict_term_argmax <- function(prob_tbl) {
  prob_mat <- as.matrix(prob_tbl)
  prob_term_values <- parse_prob_term_values(prob_tbl)
  max_idx <- max.col(prob_mat, ties.method = 'first')
  prob_term_values[max_idx]
}

pull_pterm <- function(prob_tbl, term_values) {
  prob_mat <- as.matrix(prob_tbl)
  prob_term_values <- parse_prob_term_values(prob_tbl)
  col_idx <- match(term_values, prob_term_values)
  out <- rep(NA_real_, nrow(prob_mat))
  valid <- !is.na(col_idx)
  out[valid] <- prob_mat[cbind(which(valid), col_idx[valid])]
  out
}

safe_prob <- function(x, eps = 1e-15) {
  pmax(x, eps)
}

evaluate_term <- function(model, data_tbl) {
  prob_tbl <- predict(model, new_data = data_tbl, type = 'prob')
  pred_term <- predict_term_argmax(prob_tbl)
  p_true <- pull_pterm(prob_tbl, data_tbl[['term']])
  tibble::tibble(
    accuracy = mean(pred_term == data_tbl[['term']], na.rm = TRUE),
    logLoss = -mean(log(safe_prob(p_true)), na.rm = TRUE)
  )
}

evaluate_aavp <- function(model, data_tbl) {
  pred <- predict(model, new_data = data_tbl) %>%
    dplyr::pull(.pred)
  err <- pred - data_tbl[['aavP']]
  tibble::tibble(
    mse = mean(err ^ 2, na.rm = TRUE),
    rmse = sqrt(mean(err ^ 2, na.rm = TRUE)),
    mae = mean(abs(err), na.rm = TRUE)
  )
}

term_results_by_split <- purrr::map_dfr(1:3, function(version) {
  model_path <- paste0('models/contracts/skater/term', version, '.rds')
  model <- readRDS(model_path)
  seen_eval <- evaluate_term(model, seen)
  unseen_eval <- evaluate_term(model, validate)
  dplyr::bind_rows(
    tibble::tibble(
      split = 'Seen',
      splitRole = 'Seen',
      version = version,
      candidate = unname(version_to_model[as.character(version)]),
      modelFile = basename(model_path)
    ) %>% dplyr::bind_cols(seen_eval),
    tibble::tibble(
      split = 'Unseen Future',
      splitRole = 'Unseen Future',
      version = version,
      candidate = unname(version_to_model[as.character(version)]),
      modelFile = basename(model_path)
    ) %>% dplyr::bind_cols(unseen_eval)
  )
}) %>%
  dplyr::group_by(split) %>%
  dplyr::arrange(logLoss, dplyr::desc(accuracy), version, .by_group = TRUE) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::ungroup()

write.csv(term_results_by_split, file.path(out_dir, 'model_compare_term_by_split.csv'), row.names = FALSE)
write.csv(
  term_results_by_split %>% dplyr::filter(split == 'Unseen Future') %>% dplyr::arrange(rank),
  file.path(out_dir, 'model_compare_term.csv'),
  row.names = FALSE
)

aavp_results_by_split <- purrr::map_dfr(1:3, function(version) {
  model_path <- paste0('models/contracts/skater/aavP', version, '.rds')
  model <- readRDS(model_path)
  seen_eval <- evaluate_aavp(model, seen)
  unseen_eval <- evaluate_aavp(model, validate)
  dplyr::bind_rows(
    tibble::tibble(
      split = 'Seen',
      splitRole = 'Seen',
      version = version,
      candidate = unname(version_to_model[as.character(version)]),
      modelFile = basename(model_path)
    ) %>% dplyr::bind_cols(seen_eval),
    tibble::tibble(
      split = 'Unseen Future',
      splitRole = 'Unseen Future',
      version = version,
      candidate = unname(version_to_model[as.character(version)]),
      modelFile = basename(model_path)
    ) %>% dplyr::bind_cols(unseen_eval)
  )
}) %>%
  dplyr::group_by(split) %>%
  dplyr::arrange(mse, rmse, mae, version, .by_group = TRUE) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::ungroup()

write.csv(aavp_results_by_split, file.path(out_dir, 'model_compare_aavp_by_split.csv'), row.names = FALSE)
write.csv(
  aavp_results_by_split %>% dplyr::filter(split == 'Unseen Future') %>% dplyr::arrange(rank),
  file.path(out_dir, 'model_compare_aavp.csv'),
  row.names = FALSE
)

# Export feature importance for selected unseen-future winners.
extract_importance <- function(model, task, candidate, model_file, engine_name) {
  engine <- workflows::extract_fit_engine(model)
  imp <- if (engine_name == 'lightgbm') {
    tryCatch(lightgbm::lgb.importance(model = engine), error = function(e) NULL)
  } else if (engine_name == 'xgboost') {
    tryCatch(xgboost::xgb.importance(model = engine), error = function(e) NULL)
  } else {
    NULL
  }

  if (is.null(imp) || nrow(imp) == 0L) {
    return(tibble::tibble(
      task = character(),
      candidate = character(),
      modelFile = character(),
      feature = character(),
      gain = numeric(),
      gainShare = numeric(),
      rank = integer()
    ))
  }

  imp_tbl <- tibble::as_tibble(imp)
  feature_col <- if ('Feature' %in% names(imp_tbl)) 'Feature' else names(imp_tbl)[1]
  gain_col <- if ('Gain' %in% names(imp_tbl)) 'Gain' else names(imp_tbl)[2]

  out <- imp_tbl %>%
    dplyr::transmute(
      feature = as.character(.data[[feature_col]]),
      gain = as.numeric(.data[[gain_col]])
    ) %>%
    dplyr::filter(!is.na(gain), !is.na(feature)) %>%
    dplyr::arrange(dplyr::desc(gain))

  total_gain <- sum(out$gain, na.rm = TRUE)
  out <- out %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      gainShare = if (total_gain > 0) gain / total_gain else 0,
      task = task,
      candidate = candidate,
      modelFile = model_file
    ) %>%
    dplyr::select(task, candidate, modelFile, feature, gain, gainShare, rank)

  out
}

best_term <- term_results_by_split %>%
  dplyr::filter(split == 'Unseen Future') %>%
  dplyr::arrange(rank) %>%
  dplyr::slice_head(n = 1)
best_aavp <- aavp_results_by_split %>%
  dplyr::filter(split == 'Unseen Future') %>%
  dplyr::arrange(rank) %>%
  dplyr::slice_head(n = 1)

best_term_model <- readRDS(file.path('models/contracts/skater', best_term$modelFile[[1]]))
best_aavp_model <- readRDS(file.path('models/contracts/skater', best_aavp$modelFile[[1]]))

term_engine <- dplyr::if_else(best_term$candidate[[1]] == 'LightGBM', 'lightgbm', 'xgboost')
aavp_engine <- dplyr::if_else(best_aavp$candidate[[1]] == 'LightGBM', 'lightgbm', 'xgboost')

importance_tbl <- dplyr::bind_rows(
  extract_importance(
    model = best_term_model,
    task = 'Term',
    candidate = best_term$candidate[[1]],
    model_file = best_term$modelFile[[1]],
    engine_name = term_engine
  ),
  extract_importance(
    model = best_aavp_model,
    task = 'AAV%',
    candidate = best_aavp$candidate[[1]],
    model_file = best_aavp$modelFile[[1]],
    engine_name = aavp_engine
  )
)

write.csv(importance_tbl, file.path(out_dir, 'model_importance_selected.csv'), row.names = FALSE)
