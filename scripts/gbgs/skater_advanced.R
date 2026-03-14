suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))
source(file.path("scripts", "gbgs", "shared_advanced.R"))

cat("Loading skater GBG base...\n")
base <- readr::read_csv(
  file.path("data", "gbgs", "basic", paste0("skaters_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    playerId = as.integer(playerId),
    gameId = as.integer(gameId),
    teamId = as.integer(teamId)
  )

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
    strength = normalize_gbg_strength(strengthState),
    xG = as.numeric(xG),
    goaliePlayerId = as.integer(goaliePlayerId)
  ) %>%
  dplyr::filter(strength %in% c("ev", "pp", "sh"))

cat("Loading skater on-ice context...\n")
attempt_context <- prepare_sbss_shot_attempts(SEASON) %>%
  dplyr::transmute(
    gameId = as.integer(gameId),
    eventId = as.integer(eventId),
    playerIdsFor = playerIdsFor,
    playerIdsAgainst = playerIdsAgainst
  )

attempts <- sbs %>%
  dplyr::left_join(attempt_context, by = c("gameId", "eventId"))

metric_long <- dplyr::bind_rows(
  summarise_entity_metric(
    attempts,
    id_col = "shooterPlayerId",
    metric = "ixGF",
    value_col = "xG",
    strength_col = "strength",
    valid_ids = skater_ids,
    out_id_col = "playerId"
  ),
  summarise_list_metric(
    attempts,
    ids_col = "playerIdsFor",
    metric = "oxGF",
    value_col = "xG",
    strength_col = "strength",
    valid_ids = skater_ids
  ),
  summarise_list_metric(
    attempts,
    ids_col = "playerIdsAgainst",
    metric = "oxGA",
    value_col = "xG",
    strength_col = "strength",
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

aggregate_path <- file.path("data", "gbgs", "advanced", paste0("skaters_", SEASON, ".csv"))
dir.create(dirname(aggregate_path), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(skaters, aggregate_path)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(skaters),
  " Skaters:", dplyr::n_distinct(skaters$playerId),
  "\n"
)
