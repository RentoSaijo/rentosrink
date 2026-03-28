suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))
source(file.path("scripts", "gbgs", "shared_advanced.R"))

aggregate_path <- file.path("data", "gbgs", "advanced", paste0("goalies_", SEASON, ".csv"))
existing <- load_existing_gbg_file(aggregate_path)
existing_game_ids <- extract_existing_game_ids(existing)

season_game_ids <- readr::read_csv(
  file.path("data", "games.csv"),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    gameId = as.integer(gameId),
    seasonId = as.integer(seasonId)
  ) %>%
  dplyr::filter(seasonId == SEASON, !is.na(gameId)) %>%
  dplyr::pull(gameId) %>%
  unique() %>%
  sort()

missing_game_ids <- setdiff(season_game_ids, existing_game_ids)

if (length(missing_game_ids) == 0L) {
  cat("No missing goalie advanced GBG games to append.\n")
  quit(save = "no", status = 0)
}

cat("Loading goalie GBG base...\n")
base <- readr::read_csv(
  file.path("data", "gbgs", "basic", paste0("goalies_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    playerId = as.integer(playerId),
    gameId = as.integer(gameId),
    teamId = as.integer(teamId)
  ) %>%
  dplyr::filter(gameId %in% missing_game_ids)

goalie_ids <- sort(unique(base$playerId))

cat("Loading goalie SBS...\n")
attempts <- readr::read_csv(
  file.path("data", "sbss", paste0("goalies_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    goaliePlayerId = as.integer(goaliePlayerId),
    gameId = as.integer(gameId),
    strengthAgainst = flip_strength_code(normalize_gbg_strength(strengthState)),
    xG = as.numeric(xG)
  ) %>%
  dplyr::filter(gameId %in% missing_game_ids) %>%
  dplyr::filter(strengthAgainst %in% c("ev", "pp", "sh"))

ensure_base_game_coverage(base, attempts, "Goalie GBGS advanced")

metric_long <- summarise_entity_metric(
  attempts,
  id_col = "goaliePlayerId",
  metric = "xGA",
  value_col = "xG",
  strength_col = "strengthAgainst",
  valid_ids = goalie_ids,
  out_id_col = "playerId"
)

goalies <- build_gbg_output(
  base = base,
  metric_long = metric_long,
  id_cols = c("playerId", "gameId", "teamId"),
  metrics = c("xGA")
)

dir.create(dirname(aggregate_path), recursive = TRUE, showWarnings = FALSE)
goalies <- append_gbg_rows(existing, goalies, id_cols = c("playerId", "gameId", "teamId"))
readr::write_csv(goalies, aggregate_path)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(goalies),
  " Goalies:", dplyr::n_distinct(goalies$playerId),
  "\n"
)
