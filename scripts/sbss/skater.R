suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))

cat("Building scored shot attempts for skaters...\n")
skaters <- rebuild_skater_sbss_from_existing(SEASON)

aggregate_path <- file.path("data", "sbss", paste0("skaters_", SEASON, ".csv"))

write_aggregate_entity_file(data = skaters, aggregate_path = aggregate_path)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(skaters),
  " Skaters:", dplyr::n_distinct(skaters$shooterPlayerId),
  "\n"
)
