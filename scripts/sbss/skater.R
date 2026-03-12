suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))

cat("Building scored shot attempts for skaters...\n")
skaters <- rebuild_skater_sbss_from_existing(SEASON)

aggregate_path <- file.path("data", "sbss", paste0("skaters_", SEASON, ".csv"))
split_dir <- file.path("data", "sbss", "skater")

write_split_entity_files(
  data = skaters,
  id_col = "shooterPlayerId",
  season_id = SEASON,
  aggregate_path = aggregate_path,
  split_dir = split_dir
)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(skaters),
  " Skaters:", dplyr::n_distinct(skaters$shooterPlayerId),
  "\n"
)
