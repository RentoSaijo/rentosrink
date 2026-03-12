suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))
source(file.path("scripts", "gbgs", "shared_advanced.R"))

cat("Loading goalie GBG base...\n")
base <- readr::read_csv(
  file.path("data", "gbgs", "basic", paste0("goalies_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    playerId = as.integer(playerId),
    gameId = as.integer(gameId)
  )

goalie_ids <- sort(unique(base$playerId))

cat("Loading goalie SBS...\n")
attempts <- readr::read_csv(
  file.path("data", "sbss", paste0("goalies_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    goaliePlayerId = as.integer(goaliePlayerId),
    gameId = as.integer(gameId),
    strength = normalize_gbg_strength(strengthState),
    xG = as.numeric(xG)
  ) %>%
  dplyr::filter(strength %in% c("ev", "pp", "sh"))

metric_long <- summarise_entity_metric(
  attempts,
  id_col = "goaliePlayerId",
  metric = "xGA",
  value_col = "xG",
  strength_col = "strength",
  valid_ids = goalie_ids,
  out_id_col = "playerId"
)

goalies <- build_gbg_output(
  base = base,
  metric_long = metric_long,
  id_cols = c("playerId", "gameId"),
  metrics = c("xGA")
)

aggregate_path <- file.path("data", "gbgs", "advanced", paste0("goalies_", SEASON, ".csv"))
split_dir <- file.path("data", "gbgs", "advanced", "goalie")

write_split_entity_files(
  data = goalies,
  id_col = "playerId",
  season_id = SEASON,
  aggregate_path = aggregate_path,
  split_dir = split_dir
)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(goalies),
  " Goalies:", dplyr::n_distinct(goalies$playerId),
  "\n"
)
