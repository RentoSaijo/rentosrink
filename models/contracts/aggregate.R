# Load library.
suppressMessages(library(tidyverse))

# Aggregate skater shot analyses by season.
skater_files <- list.files(
  path = 'data',
  pattern = '^skater_shot_analysis_[0-9]{8}\\.csv$',
  full.names = TRUE
) %>%
  tibble::tibble(file = .) %>%
  dplyr::mutate(
    seasonId = stringr::str_extract(basename(file), '[0-9]{8}') %>% as.integer()
  ) %>%
  dplyr::filter(seasonId >= 20142015) %>%
  dplyr::arrange(seasonId)

skater_shot_analysis <- purrr::map2_dfr(
  .x = skater_files$file,
  .y = skater_files$seasonId,
  .f = ~ readr::read_csv(.x, show_col_types = FALSE) %>%
    dplyr::mutate(seasonId = .y)
)

# Write to CSV.
readr::write_csv(skater_shot_analysis, 'models/contracts/data/skater_shot_analysis.csv')

# Aggregate goalie shot analyses by season.
goalie_files <- list.files(
  path = 'data',
  pattern = '^goalie_shot_analysis_[0-9]{8}\\.csv$',
  full.names = TRUE
) %>%
  tibble::tibble(file = .) %>%
  dplyr::mutate(
    seasonId = stringr::str_extract(basename(file), '[0-9]{8}') %>% as.integer()
  ) %>%
  dplyr::filter(seasonId >= 20142015) %>%
  dplyr::arrange(seasonId)

goalie_shot_analysis <- purrr::map2_dfr(
  .x = goalie_files$file,
  .y = goalie_files$seasonId,
  .f = ~ readr::read_csv(.x, show_col_types = FALSE) %>%
    dplyr::mutate(seasonId = .y)
)

# Write to CSV.
readr::write_csv(goalie_shot_analysis, 'models/contracts/data/goalie_shot_analysis.csv')
