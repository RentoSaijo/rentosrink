# ----- Setup ----- #

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nhlscraper))

season_env <- Sys.getenv("SEASON", unset = "20252026")
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

normalize_situation_code <- function(x) {
  out <- suppressWarnings(as.integer(as.character(x)))
  out <- ifelse(is.na(out), NA_character_, sprintf("%04d", out))
  out
}

derive_strength_state <- function(strength_state, situation_code, game_type_id, period_number) {
  out <- normalize_strength_state(strength_state)
  sc <- normalize_situation_code(situation_code)
  is_shootout <- !is.na(game_type_id) & !is.na(period_number) & game_type_id == 2L & period_number == 5L
  is_penalty_shot <- !is.na(sc) & sc %in% c("0101", "1010") & !is_shootout
  out[is.na(out)] <- "ev"
  out[is_penalty_shot] <- "ev"
  out
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
      teamTriCode = character(),
      teamId = integer(),
      gameId = integer(),
      gameTypeId = integer(),
      gameDate = as.Date(character())
    ))
  }

  out %>%
    dplyr::transmute(
      playerId = as.integer(playerId),
      teamTriCode = if ("teamTriCode" %in% names(out)) as.character(teamTriCode) else NA_character_,
      teamId = if ("teamId" %in% names(out)) as.integer(teamId) else NA_integer_,
      gameId = as.integer(gameId),
      gameTypeId = as.integer(game_type),
      gameDate = as.Date(gameDate)
    )
}

summarise_goalie_metric <- function(df, metric, value_col = "value") {
  if (nrow(df) == 0) return(empty_metric_long())

  df %>%
    dplyr::transmute(
      playerId = as.integer(goalieIdResolved),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(gameTypeId),
      strength = as.character(strengthAgainst),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(playerId), !is.na(strength), !is.na(value)) %>%
    dplyr::group_by(playerId, gameId, gameTypeId, strength) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(metric = metric, .before = value)
}

summarise_goalie_actor_metric <- function(
    df,
    metric,
    player_col,
    strength_col,
    value_col = "value"
) {
  if (nrow(df) == 0) return(empty_metric_long())

  df %>%
    dplyr::transmute(
      playerId = as.integer(.data[[player_col]]),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(gameTypeId),
      strength = as.character(.data[[strength_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(playerId), !is.na(strength), !is.na(value)) %>%
    dplyr::group_by(playerId, gameId, gameTypeId, strength) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(metric = metric, .before = value)
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
) 

games <- nhlscraper::games() %>%
  dplyr::filter(seasonId == SEASON, gameTypeId %in% c(2L, 3L)) %>%
  dplyr::transmute(
    gameId = as.integer(gameId),
    gameTypeId = as.integer(gameTypeId),
    gameDate = as.Date(gameDate),
    homeTeamId = as.integer(homeTeamId),
    visitingTeamId = as.integer(visitingTeamId)
  )

season_team_lookup <- nhlscraper::teams() %>%
  dplyr::transmute(
    teamId = as.integer(teamId),
    teamTriCode = as.character(teamTriCode)
  ) %>%
  dplyr::filter(
    !is.na(teamId),
    !is.na(teamTriCode),
    teamId %in% unique(c(games$homeTeamId, games$visitingTeamId))
  ) %>%
  dplyr::distinct(teamTriCode, .keep_all = TRUE)

goalie_games <- goalie_games %>%
  dplyr::left_join(season_team_lookup, by = "teamTriCode", suffix = c("", "_lookup")) %>%
  dplyr::mutate(teamId = dplyr::coalesce(teamId, teamId_lookup)) %>%
  dplyr::select(-teamId_lookup) %>%
  dplyr::distinct(playerId, teamTriCode, teamId, gameId, gameTypeId, gameDate)

goalie_ids <- sort(unique(goalie_games$playerId))

cat("Loading pbp and shift data...\n")
pbps <- nhlscraper::gc_pbps(SEASON)
shifts <- nhlscraper::shift_charts(SEASON)
pbp <- nhlscraper::add_shift_times(pbps, shifts)

goalie_in_net_id <- if ("goalieInNetId" %in% names(pbp)) {
  suppressWarnings(as.integer(pbp$goalieInNetId))
} else {
  rep(NA_integer_, nrow(pbp))
}
pbp$goalieInNetIdCompat <- goalie_in_net_id

period_number <- if ("periodNumber" %in% names(pbp)) {
  suppressWarnings(as.integer(pbp$periodNumber))
} else if ("period" %in% names(pbp)) {
  suppressWarnings(as.integer(pbp$period))
} else {
  rep(NA_integer_, nrow(pbp))
}

pbp <- pbp %>%
  dplyr::mutate(period = period_number) %>%
  dplyr::filter(
    gameTypeId %in% c(2L, 3L),
    !(gameTypeId == 2L & period == 5L)
  ) %>%
  dplyr::mutate(
    typeDescKey = as.character(eventTypeDescKey),
    strengthFor = derive_strength_state(strengthState, situationCode, gameTypeId, period),
    strengthAgainst = flip_strength_code(strengthFor),
    isRush = dplyr::coalesce(as.logical(isRush), FALSE),
    isRebound = dplyr::coalesce(as.logical(isRebound), FALSE),
    createdReboundFlag = dplyr::coalesce(as.logical(createdRebound), FALSE),
    goalieIdResolved = as.integer(dplyr::coalesce(
      goaliePlayerIdAgainst,
      goalieInNetIdCompat
    ))
  )

game_dates <- dplyr::bind_rows(
  games %>% dplyr::select(gameId, gameTypeId, gameDate),
  goalie_games %>% dplyr::select(gameId, gameTypeId, gameDate)
) %>%
  dplyr::group_by(gameId, gameTypeId) %>%
  dplyr::summarise(gameDate = first_non_na_date(gameDate), .groups = "drop")

goalie_team_map <- dplyr::bind_rows(
  pbp %>%
    dplyr::transmute(gameId = as.integer(gameId), playerId = as.integer(homeGoaliePlayerId)) %>%
    dplyr::filter(!is.na(playerId)) %>%
    dplyr::distinct() %>%
    dplyr::left_join(games %>% dplyr::select(gameId, homeTeamId), by = "gameId") %>%
    dplyr::transmute(playerId, gameId, teamId = homeTeamId),
  pbp %>%
    dplyr::transmute(gameId = as.integer(gameId), playerId = as.integer(awayGoaliePlayerId)) %>%
    dplyr::filter(!is.na(playerId)) %>%
    dplyr::distinct() %>%
    dplyr::left_join(games %>% dplyr::select(gameId, visitingTeamId), by = "gameId") %>%
    dplyr::transmute(playerId, gameId, teamId = visitingTeamId)
) %>%
  dplyr::distinct(playerId, gameId, .keep_all = TRUE)

# ----- Metrics ----- #

metric_names <- c(
  "cA", "fA", "sA", "gA", "apA", "asA",
  "mD", "mC",
  "rsA", "rbA", "rgA"
)
all_metric_names <- metric_names

penalties <- pbp %>%
  dplyr::filter(typeDescKey == "penalty") %>%
  dplyr::mutate(value = pmax(dplyr::coalesce(as.numeric(penaltyDuration), 0), 0))

shots_all <- pbp %>%
  dplyr::filter(
    typeDescKey %in% c("goal", "shot-on-goal", "missed-shot", "blocked-shot"),
    !(typeDescKey == "missed-shot" & reason == "short")
  ) %>%
  dplyr::mutate(
    value = 1,
    isRushVal = as.numeric(isRush),
    isReboundVal = as.numeric(isRebound),
    createdReboundVal = as.numeric(createdReboundFlag)
  )

shots_fenwick <- shots_all %>%
  dplyr::filter(typeDescKey %in% c("goal", "shot-on-goal", "missed-shot"))

shots_sog <- shots_all %>%
  dplyr::filter(typeDescKey %in% c("goal", "shot-on-goal"))

goals <- pbp %>%
  dplyr::filter(typeDescKey == "goal")

goals_ap1 <- goals %>%
  dplyr::mutate(value = as.numeric(!is.na(assist1PlayerId))) %>%
  dplyr::filter(value > 0)

goals_ap2 <- goals %>%
  dplyr::mutate(value = as.numeric(!is.na(assist2PlayerId))) %>%
  dplyr::filter(value > 0)

stats_long <- dplyr::bind_rows(
  summarise_goalie_metric(shots_all, "cA"),
  summarise_goalie_metric(shots_fenwick, "fA"),
  summarise_goalie_metric(shots_sog, "sA"),
  summarise_goalie_metric(goals %>% dplyr::mutate(value = 1), "gA"),
  summarise_goalie_metric(goals_ap1, "apA"),
  summarise_goalie_metric(goals_ap2, "asA"),
  summarise_goalie_actor_metric(penalties, "mD", "drawnByPlayerId", "strengthAgainst"),
  summarise_goalie_actor_metric(penalties, "mC", "committedByPlayerId", "strengthFor"),
  summarise_goalie_metric(shots_all, "rsA", value_col = "isRushVal"),
  summarise_goalie_metric(shots_all, "rbA", value_col = "isReboundVal"),
  summarise_goalie_metric(shots_all, "rgA", value_col = "createdReboundVal")
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
  dplyr::left_join(goalie_team_map, by = c("playerId", "gameId"), suffix = c("", "_map")) %>%
  dplyr::mutate(teamId = dplyr::coalesce(teamId, teamId_map)) %>%
  dplyr::left_join(stats_wide, by = c("playerId", "gameId", "gameTypeId")) %>%
  dplyr::left_join(game_dates, by = c("gameId", "gameTypeId"), suffix = c("", "_games")) %>%
  dplyr::mutate(gameDate = dplyr::coalesce(gameDate, gameDate_games)) %>%
  dplyr::select(-teamTriCode, -teamId_map, -gameDate_games) %>%
  ensure_cols(expected_cols) %>%
  dplyr::select(playerId, gameId, teamId, gameDate, tidyselect::all_of(expected_cols)) %>%
  dplyr::arrange(playerId, gameDate, gameId)

# ----- Write Files ----- #

out_dir <- file.path("data", "gbgs", "basic")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

season_path <- file.path(out_dir, paste0("goalies_", SEASON, ".csv"))
readr::write_csv(goalies, season_path)

cat("Wrote season file:", season_path, "\n")
cat("Rows:", nrow(goalies), " Goalies:", length(unique(goalies$playerId)), "\n")
