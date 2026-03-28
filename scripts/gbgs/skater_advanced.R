suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))
source(file.path("scripts", "gbgs", "shared_advanced.R"))

aggregate_path <- file.path("data", "gbgs", "advanced", paste0("skaters_", SEASON, ".csv"))
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
  cat("No missing skater advanced GBG games to append.\n")
  quit(save = "no", status = 0)
}

cat("Loading skater GBG base...\n")
base <- readr::read_csv(
  file.path("data", "gbgs", "basic", paste0("skaters_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    playerId = as.integer(playerId),
    gameId = as.integer(gameId),
    teamId = as.integer(teamId)
  ) %>%
  dplyr::filter(gameId %in% missing_game_ids)

skater_ids <- sort(unique(base$playerId))

cat("Loading skater SBS...\n")
sbs <- readr::read_csv(
  file.path("data", "sbss", paste0("skaters_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    shooterPlayerId = as.integer(shooterPlayerId),
    gameId = as.integer(gameId),
    eventId = as.integer(eventId),
    strengthFor = normalize_gbg_strength(strengthState),
    xG = as.numeric(xG),
    goaliePlayerId = as.integer(goaliePlayerId)
  ) %>%
  dplyr::filter(gameId %in% missing_game_ids) %>%
  dplyr::mutate(strengthAgainst = flip_strength_code(strengthFor)) %>%
  dplyr::filter(strengthFor %in% c("ev", "pp", "sh"))

cat("Loading skater on-ice context...\n")
attempt_context <- prepare_sbss_shot_attempts(SEASON) %>%
  dplyr::transmute(
    gameId = as.integer(gameId),
    eventId = as.integer(eventId),
    playerIdsFor = playerIdsFor,
    playerIdsAgainst = playerIdsAgainst
) %>%
  dplyr::filter(gameId %in% missing_game_ids)

attempts <- sbs %>%
  dplyr::left_join(attempt_context, by = c("gameId", "eventId"))

ensure_base_game_coverage(base, attempts, "Skater GBGS advanced")

metric_long <- dplyr::bind_rows(
  summarise_entity_metric(
    attempts,
    id_col = "shooterPlayerId",
    metric = "ixGF",
    value_col = "xG",
    strength_col = "strengthFor",
    valid_ids = skater_ids,
    out_id_col = "playerId"
  ),
  summarise_list_metric(
    attempts,
    ids_col = "playerIdsFor",
    metric = "oxGF",
    value_col = "xG",
    strength_col = "strengthFor",
    valid_ids = skater_ids
  ),
  summarise_list_metric(
    attempts,
    ids_col = "playerIdsAgainst",
    metric = "oxGA",
    value_col = "xG",
    strength_col = "strengthAgainst",
    valid_ids = skater_ids
  )
)

metrics <- c("ixGF", "oxGF", "oxGA")
skaters <- build_gbg_output(
  base = base,
  metric_long = metric_long,
  id_cols = c("playerId", "gameId", "teamId"),
  metrics = metrics
)

dir.create(dirname(aggregate_path), recursive = TRUE, showWarnings = FALSE)
skaters <- append_gbg_rows(existing, skaters, id_cols = c("playerId", "gameId", "teamId"))
readr::write_csv(skaters, aggregate_path)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(skaters),
  " Skaters:", dplyr::n_distinct(skaters$playerId),
  "\n"
)
