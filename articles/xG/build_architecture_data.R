# ----- Setup ----- #

suppressMessages(library(readr))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(purrr))
suppressMessages(library(nhlscraper))

# ----- Helpers ----- #

SEASONS <- c(20212022, 20232024, 20252026)

bucket_from_code <- function(code, is_empty_net_against = NULL) {
  code <- as.character(code)
  d2 <- substr(code, 2, 2)
  d4 <- substr(code, 4, 4)

  if (!is.null(is_empty_net_against)) {
    return(dplyr::case_when(
      is_empty_net_against ~ 'Empty-Net Context',
      code %in% c('0101', '1010') ~ 'Shootout / Penalty Shot',
      code == '1551' ~ 'Standard 5v5',
      TRUE ~ 'Special Teams'
    ))
  }

  dplyr::case_when(
    code %in% c('0101', '1010') ~ 'Shootout / Penalty Shot',
    code == '1551' ~ 'Standard 5v5',
    d2 == '0' | d4 == '0' ~ 'Empty-Net Context',
    TRUE ~ 'Special Teams'
  )
}

read_local_shots <- function(path) {
  season <- stringr::str_extract(basename(path), '[0-9]{8}')
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::transmute(
      season = season,
      situationCode = as.character(situationCode),
      typeDescKey = as.character(typeDescKey),
      isGoal = typeDescKey == 'goal',
      bucket = bucket_from_code(situationCode)
    )
}

read_pbp_shots <- function(season) {
  nhlscraper::gc_pbps(season) %>%
    dplyr::filter(
      gameTypeId %in% 2:3,
      typeDescKey %in% c('goal', 'shot-on-goal', 'missed-shot')
    ) %>%
    dplyr::transmute(
      season = as.character(season),
      situationCode = as.character(situationCode),
      typeDescKey = as.character(typeDescKey),
      isGoal = typeDescKey == 'goal',
      bucket = bucket_from_code(
        code = situationCode,
        is_empty_net_against = dplyr::coalesce(isEmptyNetAgainst, FALSE)
      )
    )
}

# ----- Build ----- #

shots <- NULL
source_label <- NULL

pbp_try <- tryCatch(
  purrr::map_dfr(SEASONS, read_pbp_shots),
  error = function(e) tibble::tibble()
)

if (nrow(pbp_try) > 0) {
  shots <- pbp_try
  source_label <- 'nhlscraper_gc_pbps'
} else {
  shots_files <- list.files(
    'data',
    pattern = '^shots_[0-9]{8}_v4[.]csv$',
    full.names = TRUE
  )

  if (length(shots_files) == 0L) {
    stop('No PBP network data available and no local v4 shots files found under data/.')
  }

  shots <- purrr::map_dfr(shots_files, read_local_shots)
  source_label <- 'local_shots_v4_proxy'
}

bucket_levels <- c(
  'Standard 5v5',
  'Special Teams',
  'Empty-Net Context',
  'Shootout / Penalty Shot'
)

partition_by_season <- shots %>%
  dplyr::group_by(season, bucket) %>%
  dplyr::summarise(
    shots = dplyr::n(),
    goals = sum(isGoal, na.rm = TRUE),
    goalRate = goals / shots,
    .groups = 'drop'
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(shotShare = shots / sum(shots, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(bucket = factor(bucket, levels = bucket_levels)) %>%
  dplyr::arrange(season, bucket)

partition_overall <- shots %>%
  dplyr::group_by(bucket) %>%
  dplyr::summarise(
    shots = dplyr::n(),
    goals = sum(isGoal, na.rm = TRUE),
    goalRate = goals / shots,
    .groups = 'drop'
  ) %>%
  dplyr::mutate(shotShare = shots / sum(shots, na.rm = TRUE)) %>%
  dplyr::mutate(bucket = factor(bucket, levels = bucket_levels)) %>%
  dplyr::arrange(bucket)

non_standard_codes <- shots %>%
  dplyr::filter(bucket != 'Standard 5v5') %>%
  dplyr::group_by(situationCode, bucket) %>%
  dplyr::summarise(
    shots = dplyr::n(),
    goals = sum(isGoal, na.rm = TRUE),
    goalRate = goals / shots,
    .groups = 'drop'
  ) %>%
  dplyr::mutate(shotShareWithinBucket = shots / sum(shots), .by = bucket) %>%
  dplyr::arrange(bucket, dplyr::desc(shots))

code_overlap <- shots %>%
  dplyr::group_by(situationCode, bucket) %>%
  dplyr::summarise(shots = dplyr::n(), .groups = 'drop') %>%
  dplyr::group_by(situationCode) %>%
  dplyr::mutate(nBuckets = dplyr::n_distinct(bucket)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(nBuckets > 1) %>%
  dplyr::arrange(situationCode, bucket)

data_source <- tibble::tibble(
  source = source_label,
  generatedAtUtc = as.character(Sys.time()),
  seasons = paste(SEASONS, collapse = ', ')
)

dir.create('articles/xG/data', recursive = TRUE, showWarnings = FALSE)

readr::write_csv(partition_by_season, 'articles/xG/data/partition_by_season.csv')
readr::write_csv(partition_overall, 'articles/xG/data/partition_overall.csv')
readr::write_csv(non_standard_codes, 'articles/xG/data/non_standard_codes.csv')
readr::write_csv(code_overlap, 'articles/xG/data/code_overlap.csv')
readr::write_csv(data_source, 'articles/xG/data/data_source.csv')
