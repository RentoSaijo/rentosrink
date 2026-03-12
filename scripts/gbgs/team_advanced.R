suppressPackageStartupMessages(library(tidyverse))

season_env <- Sys.getenv("SEASON", unset = "20252026")
SEASON <- as.integer(season_env)

source(file.path("scripts", "sbss", "shared.R"))
source(file.path("scripts", "gbgs", "shared_advanced.R"))

cat("Loading team GBG base...\n")
base <- readr::read_csv(
  file.path("data", "gbgs", "basic", paste0("teams_", SEASON, ".csv")),
  show_col_types = FALSE
) %>%
  dplyr::transmute(
    teamId = as.integer(teamId),
    gameId = as.integer(gameId)
  )

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
    strength = normalize_gbg_strength(strengthState),
    xG = as.numeric(xG)
  ) %>%
  dplyr::filter(strength %in% c("ev", "pp", "sh"))

attempts_against <- attempts %>%
  dplyr::left_join(opponents, by = c("gameId", "teamIdFor")) %>%
  dplyr::rename(teamId = teamIdAgainst)

metric_long <- dplyr::bind_rows(
  summarise_entity_metric(
    attempts %>% dplyr::rename(teamId = teamIdFor),
    id_col = "teamId",
    metric = "xGF",
    value_col = "xG",
    strength_col = "strength"
  ),
  summarise_entity_metric(
    attempts_against,
    id_col = "teamId",
    metric = "xGA",
    value_col = "xG",
    strength_col = "strength"
  )
)

teams <- build_gbg_output(
  base = base,
  metric_long = metric_long,
  id_cols = c("teamId", "gameId"),
  metrics = c("xGF", "xGA")
)

aggregate_path <- file.path("data", "gbgs", "advanced", paste0("teams_", SEASON, ".csv"))
split_dir <- file.path("data", "gbgs", "advanced", "team")

write_split_entity_files(
  data = teams,
  id_col = "teamId",
  season_id = SEASON,
  aggregate_path = aggregate_path,
  split_dir = split_dir
)

cat("Wrote season file:", aggregate_path, "\n")
cat(
  "Rows:", nrow(teams),
  " Teams:", dplyr::n_distinct(teams$teamId),
  "\n"
)
