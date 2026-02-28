# ----- Setup ----- #

suppressMessages(library(dplyr))
suppressMessages(library(readr))
suppressMessages(library(lightgbm))

# ----- Build ----- #

partitions <- c('standard', 'special', 'empty', 'shootout')
versions <- 1:4

out <- list()

for (p in partitions) {
  for (v in versions) {
    path <- file.path('models', 'xG', p, paste0('model', v, '.rds'))
    if (!file.exists(path)) next

    obj <- readRDS(path)
    if (!is.list(obj) || !('model' %in% names(obj))) next

    imp <- lightgbm::lgb.importance(obj$model, percentage = TRUE)
    if (is.null(imp) || nrow(imp) == 0) next

    split_col <- if ('Split' %in% names(imp)) 'Split' else if ('Frequency' %in% names(imp)) 'Frequency' else NA_character_

    out[[length(out) + 1]] <- imp %>%
      as_tibble() %>%
      transmute(
        partition = p,
        version = v,
        feature = Feature,
        split = if (!is.na(split_col)) .data[[split_col]] else NA_real_,
        gain = Gain,
        cover = Cover
      ) %>%
      arrange(desc(gain)) %>%
      mutate(rank = row_number())
  }
}

if (length(out) == 0) {
  stop('No feature-importance rows could be generated from models/xG/*.rds.')
}

imp_all <- bind_rows(out)

dir.create('articles/xG/data', recursive = TRUE, showWarnings = FALSE)
write_csv(imp_all, 'articles/xG/data/feature_importance_all.csv')

imp_v4 <- imp_all %>%
  filter(version == 4) %>%
  group_by(partition) %>%
  slice_min(order_by = rank, n = 20, with_ties = FALSE) %>%
  ungroup()
write_csv(imp_v4, 'articles/xG/data/feature_importance_v4_top20.csv')
