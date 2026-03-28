suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))
source(file.path("scripts", "gbgs", "shared_advanced.R"))

aggregate_path <- file.path("data", "gbgs", "advanced", paste0("teams_", SEASON, ".csv"))
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
  cat("No missing team advanced GBG games to append.\n")
  quit(save = "no", status = 0)
}

cat("Loading team GBG base...\n")
base <- readr::read_csv(
  file.path("data", "gbgs", "basic", paste0("teams_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    teamId = as.integer(teamId),
    gameId = as.integer(gameId)
  ) %>%
  dplyr::filter(gameId %in% missing_game_ids)

opponents <- base %>%
  dplyr::distinct(gameId, teamId) %>%
  dplyr::inner_join(
    base %>% dplyr::distinct(gameId, teamId),
    by = "gameId",
    suffix = c("For", "Against"),
    relationship = "many-to-many"
  ) %>%
  dplyr::filter(teamIdFor != teamIdAgainst)

cat("Loading skater SBS for team xG...\n")
attempts <- readr::read_csv(
  file.path("data", "sbss", paste0("skaters_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    gameId = as.integer(gameId),
    teamIdFor = as.integer(shooterTeamId),
    strengthFor = normalize_gbg_strength(strengthState),
    xG = as.numeric(xG)
  ) %>%
  dplyr::filter(gameId %in% missing_game_ids) %>%
  dplyr::mutate(strengthAgainst = flip_strength_code(strengthFor)) %>%
  dplyr::filter(strengthFor %in% c("ev", "pp", "sh"))

attempts_against <- attempts %>%
  dplyr::left_join(opponents, by = c("gameId", "teamIdFor")) %>%
  dplyr::rename(teamId = teamIdAgainst)

ensure_base_game_coverage(base, attempts, "Team GBGS advanced")

metric_long <- dplyr::bind_rows(
  summarise_entity_metric(
    attempts %>% dplyr::rename(teamId = teamIdFor),
    id_col = "teamId",
    metric = "xGF",
    value_col = "xG",
    strength_col = "strengthFor"
  ),
  summarise_entity_metric(
    attempts_against,
    id_col = "teamId",
    metric = "xGA",
    value_col = "xG",
    strength_col = "strengthAgainst"
  )
)

teams <- build_gbg_output(
  base = base,
  metric_long = metric_long,
  id_cols = c("teamId", "gameId"),
  metrics = c("xGF", "xGA")
)

dir.create(dirname(aggregate_path), recursive = TRUE, showWarnings = FALSE)
teams <- append_gbg_rows(existing, teams, id_cols = c("teamId", "gameId"))
readr::write_csv(teams, aggregate_path)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(teams),
  " Teams:", dplyr::n_distinct(teams$teamId),
  "\n"
)
