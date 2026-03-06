# ----- Setup ----- #

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nhlscraper))

season_env <- Sys.getenv("SEASON", unset = "20242025")
SEASON <- as.integer(season_env)

# ----- Helpers ----- #

normalize_strength_state <- function(x) {
  out <- stringr::str_to_lower(as.character(x))
  out <- stringr::str_trim(out)

  dplyr::case_when(
    out == "even-strength" ~ "ev",
    out == "power-play" ~ "pp",
    out == "penalty-kill" ~ "sh",
    stringr::str_detect(out, "^ev") | stringr::str_detect(out, "even") ~ "ev",
    stringr::str_detect(out, "^pp") | stringr::str_detect(out, "power") ~ "pp",
    stringr::str_detect(out, "^sh") |
      stringr::str_detect(out, "^pk") |
      stringr::str_detect(out, "short") |
      stringr::str_detect(out, "penalty\\s*-?\\s*kill") ~ "sh",
    TRUE ~ NA_character_
  )
}

flip_strength_code <- function(x) {
  dplyr::case_when(
    x == "pp" ~ "sh",
    x == "sh" ~ "pp",
    x == "ev" ~ "ev",
    TRUE ~ NA_character_
  )
}

normalize_id_list <- function(x) {
  purrr::map(x, function(ids) {
    if (is.null(ids) || length(ids) == 0L) return(integer())
    if (length(ids) == 1L && is.na(ids)) return(integer())
    as.integer(ids)
  })
}

first_non_na_date <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0L) as.Date(NA) else vals[[1]]
}

ensure_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    for (col in missing_cols) {
      df[[col]] <- 0
    }
  }
  df
}

empty_metric_long <- function() {
  tibble::tibble(
    playerId = integer(),
    gameId = integer(),
    gameTypeId = integer(),
    strength = character(),
    metric = character(),
    value = double()
  )
}

make_expected_metric_cols <- function(
    metrics,
    strengths = c("ev", "pp", "sh")
) {
  out <- character()
  for (metric in metrics) {
    for (strength in strengths) {
      out <- c(out, paste0(metric, "_", strength))
    }
  }
  out
}

safe_goalie_game_summary <- function(season, game_type) {
  out <- tryCatch(
    nhlscraper::goalie_game_report(
      season = season,
      game_type = game_type,
      category = "summary"
    ),
    error = function(e) tibble::tibble()
  )

  if (nrow(out) == 0) {
    return(tibble::tibble(
      playerId = integer(),
      gameId = integer(),
      gameTypeId = integer(),
      gameDate = as.Date(character())
    ))
  }

  out %>%
    dplyr::transmute(
      playerId = as.integer(playerId),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(game_type),
      gameDate = as.Date(gameDate)
    )
}

summarise_goalie_metric_state <- function(
    df,
    metric,
    state_col = "stateModifier",
    value_col = "value"
) {
  if (nrow(df) == 0) return(empty_metric_long())

  df %>%
    dplyr::transmute(
      playerId = as.integer(goalieIdResolved),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(gameTypeId),
      strength = as.character(strengthAgainst),
      state = as.character(.data[[state_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(playerId), !is.na(strength), !is.na(state), !is.na(value)) %>%
    dplyr::group_by(playerId, gameId, gameTypeId, strength, state) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::transmute(
      playerId,
      gameId,
      gameTypeId,
      strength,
      metric = paste0(metric, "_", state),
      value
    )
}

infer_blocked_goalie <- function(ids_against, goalie_ids) {
  ids <- as.integer(ids_against)
  hit <- base::intersect(ids, goalie_ids)
  if (length(hit) == 1L) hit[[1]] else NA_integer_
}

# ----- Load Data ----- #

cat("Loading goalie appearances...\n")
goalie_games <- dplyr::bind_rows(
  safe_goalie_game_summary(SEASON, 2L),
  safe_goalie_game_summary(SEASON, 3L)
) %>%
  dplyr::distinct(playerId, gameId, gameTypeId, gameDate)

goalie_ids <- sort(unique(goalie_games$playerId))

cat("Loading pbp and shift data...\n")
pbps <- nhlscraper::gc_pbps(SEASON)
shifts <- nhlscraper::shift_charts(SEASON)
pbp <- nhlscraper::add_on_ice_players(pbps, shifts) %>%
  dplyr::filter(
    gameTypeId %in% c(2L, 3L),
    !(gameTypeId == 2L & period == 5L)
  ) %>%
  dplyr::mutate(
    strengthFor = normalize_strength_state(strengthState),
    strengthAgainst = flip_strength_code(strengthFor),
    playerIdsAgainst = normalize_id_list(playerIdsAgainst),
    isRush = dplyr::coalesce(as.logical(isRush), FALSE),
    isRebound = dplyr::coalesce(as.logical(isRebound), FALSE),
    createdReboundFlag = dplyr::coalesce(as.logical(createdRebound), FALSE),
    stateModifier = dplyr::case_when(
      isRush & isRebound ~ "both",
      isRush ~ "rush",
      isRebound ~ "rebound",
      TRUE ~ "neither"
    )
  )

pbp <- pbp %>%
  dplyr::mutate(
    inferredBlockedGoalieId = purrr::map_int(
      playerIdsAgainst,
      infer_blocked_goalie,
      goalie_ids = goalie_ids
    ),
    goalieIdResolved = dplyr::case_when(
      !is.na(goalieInNetId) ~ as.integer(goalieInNetId),
      typeDescKey == "blocked-shot" ~ inferredBlockedGoalieId,
      TRUE ~ NA_integer_
    )
  )

games <- nhlscraper::games() %>%
  dplyr::filter(seasonId == SEASON, gameTypeId %in% c(2L, 3L)) %>%
  dplyr::transmute(
    gameId = as.integer(gameId),
    gameTypeId = as.integer(gameTypeId),
    gameDate = as.Date(gameDate)
  )

game_dates <- dplyr::bind_rows(
  games,
  goalie_games %>% dplyr::select(gameId, gameTypeId, gameDate)
) %>%
  dplyr::group_by(gameId, gameTypeId) %>%
  dplyr::summarise(gameDate = first_non_na_date(gameDate), .groups = "drop")

# ----- Metrics ----- #

state_metric_bases <- c("cA", "fA", "sA", "gA", "apA", "asA", "rgA")
state_levels <- c("neither", "rush", "rebound", "both")
state_metric_names <- unlist(
  lapply(state_metric_bases, function(metric) paste0(metric, "_", state_levels)),
  use.names = FALSE
)
all_metric_names <- state_metric_names

shots_all <- pbp %>%
  dplyr::filter(typeDescKey %in% c("goal", "shot-on-goal", "missed-shot", "blocked-shot")) %>%
  dplyr::mutate(value = 1)

shots_fenwick <- shots_all %>%
  dplyr::filter(typeDescKey %in% c("goal", "shot-on-goal", "missed-shot"))

shots_sog <- shots_all %>%
  dplyr::filter(typeDescKey %in% c("goal", "shot-on-goal"))

goals <- pbp %>%
  dplyr::filter(typeDescKey == "goal")

rebound_given <- shots_all %>%
  dplyr::mutate(value = as.numeric(createdReboundFlag))

goals_ap1 <- goals %>%
  dplyr::mutate(value = as.numeric(!is.na(assist1PlayerId))) %>%
  dplyr::filter(value > 0)

goals_ap2 <- goals %>%
  dplyr::mutate(value = as.numeric(!is.na(assist2PlayerId))) %>%
  dplyr::filter(value > 0)

stats_long <- dplyr::bind_rows(
  summarise_goalie_metric_state(shots_all, "cA"),
  summarise_goalie_metric_state(shots_fenwick, "fA"),
  summarise_goalie_metric_state(shots_sog, "sA"),
  summarise_goalie_metric_state(goals %>% dplyr::mutate(value = 1), "gA"),
  summarise_goalie_metric_state(goals_ap1, "apA"),
  summarise_goalie_metric_state(goals_ap2, "asA"),
  summarise_goalie_metric_state(rebound_given, "rgA"),
) %>%
  dplyr::filter(strength %in% c("ev", "pp", "sh"))

stats_wide <- stats_long %>%
  tidyr::pivot_wider(
    names_from = c(metric, strength),
    values_from = value,
    names_glue = "{metric}_{strength}",
    values_fill = 0
  )

expected_cols <- make_expected_metric_cols(all_metric_names)

goalies <- goalie_games %>%
  dplyr::left_join(stats_wide, by = c("playerId", "gameId", "gameTypeId")) %>%
  dplyr::left_join(game_dates, by = c("gameId", "gameTypeId"), suffix = c("", "_games")) %>%
  dplyr::mutate(gameDate = dplyr::coalesce(gameDate, gameDate_games)) %>%
  dplyr::select(-gameDate_games) %>%
  ensure_cols(expected_cols) %>%
  dplyr::select(playerId, gameId, gameDate, tidyselect::all_of(expected_cols)) %>%
  dplyr::arrange(playerId, gameDate, gameId)

# ----- Write Files ----- #

out_dir <- file.path("data", "gbgs", "basic")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

season_path <- file.path(out_dir, paste0("goalies_", SEASON, ".csv"))

if (nrow(goalies) > 0) {
  player_ids <- sort(unique(goalies$playerId))
  wrong_dir <- file.path("data", "gbgs")
  wrong_paths <- c(
    file.path(wrong_dir, paste0("goalies_", SEASON, ".csv")),
    file.path(wrong_dir, paste0(player_ids, "_", SEASON, ".csv"))
  )
  wrong_paths <- wrong_paths[file.exists(wrong_paths)]
  if (length(wrong_paths) > 0L) {
    invisible(file.remove(wrong_paths))
  }

  readr::write_csv(goalies, season_path)
  for (player_id in player_ids) {
    player_path <- file.path(out_dir, paste0(player_id, "_", SEASON, ".csv"))
    readr::write_csv(
      goalies %>%
        dplyr::filter(playerId == player_id) %>%
        dplyr::arrange(gameDate, gameId) %>%
        dplyr::select(-playerId),
      player_path
    )
  }
} else {
  readr::write_csv(goalies, season_path)
}

cat("Wrote season file:", season_path, "\n")
cat("Rows:", nrow(goalies), " Goalies:", length(unique(goalies$playerId)), "\n")
