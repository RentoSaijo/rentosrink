suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))

cat("Building scored shot attempts for goalies...\n")
goalies <- rebuild_goalie_sbss_from_existing(SEASON)

aggregate_path <- file.path("data", "sbss", paste0("goalies_", SEASON, ".csv"))
split_dir <- file.path("data", "sbss", "goalie")

write_split_entity_files(
  data = goalies,
  id_col = "goaliePlayerId",
  season_id = SEASON,
  aggregate_path = aggregate_path,
  split_dir = split_dir
)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(goalies),
  " Goalies:", dplyr::n_distinct(goalies$goaliePlayerId),
  "\n"
)
