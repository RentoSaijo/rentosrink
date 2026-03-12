# ----- Setup ----- #

suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

source(file.path("models", "xG", "prepare.R"))

START_SEASON <- 20232024
END_SEASON <- 20242025
OUTPUT_DIR <- "models/xG/data"

make_seasons <- function(start_season, end_season) {
  start_year <- as.integer(substr(as.character(start_season), 1, 4))
  end_year <- as.integer(substr(as.character(end_season), 1, 4))

  vapply(
    start_year:end_year,
    FUN.VALUE = integer(1),
    function(year) as.integer(paste0(year, year + 1))
  )
}

SEASONS <- make_seasons(START_SEASON, END_SEASON)

# ----- Clean ----- #

shots <- prepare_xg_shots(SEASONS)
partitions <- build_xg_partitions(shots)
partition_cols <- get_xg_partition_columns()

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

purrr::iwalk(
  partitions,
  function(data, key) {
    write_partition_csv(
      data = data,
      cols = partition_cols[[key]],
      path = file.path(OUTPUT_DIR, paste0(key, "_train.csv")),
      label = key
    )
  }
)
