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

strength_from_counts <- function(for_count, against_count) {
  dplyr::case_when(
    is.na(for_count) | is.na(against_count) ~ NA_character_,
    for_count == against_count ~ "ev",
    for_count > against_count ~ "pp",
    for_count < against_count ~ "sh",
    TRUE ~ NA_character_
  )
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
    teamId = integer(),
    gameId = integer(),
    gameTypeId = integer(),
    strength = character(),
    metric = character(),
    value = double()
  )
}

coerce_report_seconds <- function(x) {
  if (is.numeric(x)) {
    return(dplyr::coalesce(as.numeric(x), 0))
  }

  chr <- as.character(x)
  out <- suppressWarnings(as.numeric(chr))
  needs_hms <- is.na(out) & !is.na(chr) & stringr::str_detect(chr, "^\\d{1,2}:\\d{2}(?::\\d{2})?$")

  if (any(needs_hms)) {
    parts <- stringr::str_split(chr[needs_hms], ":", simplify = TRUE)
    out[needs_hms] <- dplyr::case_when(
      ncol(parts) == 2L ~ as.numeric(parts[, 1]) * 60 + as.numeric(parts[, 2]),
      ncol(parts) == 3L ~ as.numeric(parts[, 1]) * 3600 + as.numeric(parts[, 2]) * 60 + as.numeric(parts[, 3]),
      TRUE ~ NA_real_
    )
  }

  dplyr::coalesce(out, 0)
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

summarise_team_metric <- function(df, metric, team_col, strength_col, value_col = "value") {
  if (nrow(df) == 0) return(empty_metric_long())

  df %>%
    dplyr::transmute(
      teamId = as.integer(.data[[team_col]]),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(gameTypeId),
      strength = as.character(.data[[strength_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(teamId), !is.na(strength), !is.na(value)) %>%
    dplyr::group_by(teamId, gameId, gameTypeId, strength) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(metric = metric, .before = value)
}

summarise_team_metric_state <- function(
    df,
    metric,
    team_col,
    strength_col,
    state_col = "stateModifier",
    value_col = "value"
) {
  if (nrow(df) == 0) return(empty_metric_long())

  df %>%
    dplyr::transmute(
      teamId = as.integer(.data[[team_col]]),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(gameTypeId),
      strength = as.character(.data[[strength_col]]),
      state = as.character(.data[[state_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(teamId), !is.na(strength), !is.na(state), !is.na(value)) %>%
    dplyr::group_by(teamId, gameId, gameTypeId, strength, state) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::transmute(
      teamId,
      gameId,
      gameTypeId,
      strength,
      metric = paste0(metric, "_", state),
      value
    )
}

# ----- Load Data ----- #

cat("Loading games...\n")
games <- nhlscraper::games() %>%
  dplyr::filter(seasonId == SEASON, gameTypeId %in% c(2L, 3L)) %>%
  dplyr::transmute(
    gameId = as.integer(gameId),
    gameTypeId = as.integer(gameTypeId),
    gameDate = as.Date(gameDate),
    homeTeamId = as.integer(homeTeamId),
    visitingTeamId = as.integer(visitingTeamId)
  )

cat("Loading pbp data...\n")
pbp <- nhlscraper::gc_pbps(SEASON) %>%
  dplyr::filter(
    gameTypeId %in% c(2L, 3L),
    !(gameTypeId == 2L & period == 5L)
  ) %>%
  dplyr::left_join(games, by = c("gameId", "gameTypeId")) %>%
  dplyr::mutate(
    homeStrength = strength_from_counts(homeSkaterCount, awaySkaterCount),
    awayStrength = strength_from_counts(awaySkaterCount, homeSkaterCount),
    teamForId = dplyr::case_when(
      eventOwnerTeamId == homeTeamId ~ homeTeamId,
      eventOwnerTeamId == visitingTeamId ~ visitingTeamId,
      TRUE ~ NA_integer_
    ),
    teamAgainstId = dplyr::case_when(
      eventOwnerTeamId == homeTeamId ~ visitingTeamId,
      eventOwnerTeamId == visitingTeamId ~ homeTeamId,
      TRUE ~ NA_integer_
    ),
    strengthFor = dplyr::case_when(
      eventOwnerTeamId == homeTeamId ~ homeStrength,
      eventOwnerTeamId == visitingTeamId ~ awayStrength,
      TRUE ~ NA_character_
    ),
    strengthAgainst = dplyr::case_when(
      eventOwnerTeamId == homeTeamId ~ awayStrength,
      eventOwnerTeamId == visitingTeamId ~ homeStrength,
      TRUE ~ NA_character_
    ),
    duration = dplyr::coalesce(as.numeric(duration), 0),
    isRush = dplyr::coalesce(as.logical(isRush), FALSE),
    isRebound = dplyr::coalesce(as.logical(isRebound), FALSE),
    createdReboundFlag = dplyr::coalesce(as.logical(createdRebound), FALSE),
    stateModifier = dplyr::case_when(
      isRush & isRebound ~ "both",
      isRush ~ "rush",
      isRebound ~ "rebound",
      TRUE ~ "neither"
    )
  ) %>%
  dplyr::arrange(gameId, sortOrder)

played_games <- pbp %>%
  dplyr::distinct(gameId, gameTypeId)

games <- games %>%
  dplyr::semi_join(played_games, by = c("gameId", "gameTypeId"))

teams_base <- dplyr::bind_rows(
  games %>% dplyr::transmute(teamId = homeTeamId, gameId, gameTypeId, gameDate),
  games %>% dplyr::transmute(teamId = visitingTeamId, gameId, gameTypeId, gameDate)
) %>%
  dplyr::arrange(teamId, gameDate, gameId)

game_dates <- dplyr::bind_rows(
  games %>% dplyr::select(gameId, gameTypeId, gameDate),
  teams_base %>% dplyr::distinct(gameId, gameTypeId, gameDate)
) %>%
  dplyr::group_by(gameId, gameTypeId) %>%
  dplyr::summarise(gameDate = first_non_na_date(gameDate), .groups = "drop")

# ----- Team Minutes ----- #

cat("Loading team time reports...\n")
team_pp <- dplyr::bind_rows(
  nhlscraper::team_game_report(season = SEASON, game_type = 2, category = "powerplaytime") %>%
    dplyr::mutate(gameTypeId = 2L),
  nhlscraper::team_game_report(season = SEASON, game_type = 3, category = "powerplaytime") %>%
    dplyr::mutate(gameTypeId = 3L)
) %>%
  dplyr::transmute(
    teamId = as.integer(teamId),
    gameId = as.integer(gameId),
    gameTypeId = as.integer(gameTypeId),
    ppMinutes = coerce_report_seconds(timeOnIcePp) / 60
  )

team_pk <- dplyr::bind_rows(
  nhlscraper::team_game_report(season = SEASON, game_type = 2, category = "penaltykilltime") %>%
    dplyr::mutate(gameTypeId = 2L),
  nhlscraper::team_game_report(season = SEASON, game_type = 3, category = "penaltykilltime") %>%
    dplyr::mutate(gameTypeId = 3L)
 ) %>%
  dplyr::transmute(
    teamId = as.integer(teamId),
    gameId = as.integer(gameId),
    gameTypeId = as.integer(gameTypeId),
    shMinutes = coerce_report_seconds(timeOnIceShorthanded) / 60
  )

game_durations <- pbp %>%
  dplyr::group_by(gameId, gameTypeId) %>%
  dplyr::summarise(
    gameDurationMinutes = max(dplyr::coalesce(as.numeric(secondsElapsedInGame), 0), na.rm = TRUE) / 60,
    .groups = "drop"
  ) %>%
  dplyr::mutate(gameDurationMinutes = dplyr::coalesce(gameDurationMinutes, 0))

team_time_base <- teams_base %>%
  dplyr::select(teamId, gameId, gameTypeId) %>%
  dplyr::left_join(game_durations, by = c("gameId", "gameTypeId")) %>%
  dplyr::left_join(team_pp, by = c("teamId", "gameId", "gameTypeId")) %>%
  dplyr::left_join(team_pk, by = c("teamId", "gameId", "gameTypeId")) %>%
  dplyr::mutate(
    gameDurationMinutes = dplyr::coalesce(gameDurationMinutes, 0),
    ppMinutes = dplyr::coalesce(ppMinutes, 0),
    shMinutes = dplyr::coalesce(shMinutes, 0),
    evMinutes = pmax(gameDurationMinutes - ppMinutes - shMinutes, 0)
  )

team_time <- dplyr::bind_rows(
  team_time_base %>%
    dplyr::transmute(
      teamId,
      gameId,
      gameTypeId,
      strength = "ev",
      metric = "mP",
      value = evMinutes
    ),
  team_time_base %>%
    dplyr::transmute(
      teamId,
      gameId,
      gameTypeId,
      strength = "pp",
      metric = "mP",
      value = ppMinutes
    ),
  team_time_base %>%
    dplyr::transmute(
      teamId,
      gameId,
      gameTypeId,
      strength = "sh",
      metric = "mP",
      value = shMinutes
    )
)

# ----- Metrics ----- #

metric_names <- c("mP", "FW", "FL", "HG", "HT", "TW", "GW", "MD", "MC")
shot_metric_names <- c(
  "CF", "CA",
  "FF", "FA",
  "SF", "SA",
  "GF", "GA",
  "APF", "APA",
  "ASF", "ASA",
  "RSF", "RSA",
  "RBF", "RBA",
  "RCF", "RCA"
)
all_metric_names <- c(metric_names, shot_metric_names)

faceoffs <- pbp %>% dplyr::filter(typeDescKey == "faceoff") %>% dplyr::mutate(value = 1)
hits <- pbp %>% dplyr::filter(typeDescKey == "hit") %>% dplyr::mutate(value = 1)
takeaways <- pbp %>% dplyr::filter(typeDescKey == "takeaway") %>% dplyr::mutate(value = 1)
giveaways <- pbp %>% dplyr::filter(typeDescKey == "giveaway") %>% dplyr::mutate(value = 1)
penalties <- pbp %>% dplyr::filter(typeDescKey == "penalty") %>% dplyr::mutate(value = pmax(duration, 0))

shots <- pbp %>%
  dplyr::filter(
    typeDescKey %in% c("goal", "shot-on-goal", "missed-shot", "blocked-shot"),
    !(typeDescKey == "missed-shot" & reason == "short")
  ) %>%
  dplyr::mutate(
    value = 1,
    isFenwick = as.numeric(typeDescKey != "blocked-shot"),
    isSOG = as.numeric(typeDescKey %in% c("goal", "shot-on-goal")),
    isGoal = as.numeric(typeDescKey == "goal"),
    isRushVal = as.numeric(isRush),
    isReboundVal = as.numeric(isRebound),
    createdReboundVal = as.numeric(createdReboundFlag)
  )

goals <- pbp %>% dplyr::filter(typeDescKey == "goal")
goals_ap1 <- goals %>%
  dplyr::mutate(value = as.numeric(!is.na(assist1PlayerId))) %>%
  dplyr::filter(value > 0)
goals_ap2 <- goals %>%
  dplyr::mutate(value = as.numeric(!is.na(assist2PlayerId))) %>%
  dplyr::filter(value > 0)

stats_long <- dplyr::bind_rows(
  team_time,
  summarise_team_metric(faceoffs, "FW", "teamForId", "strengthFor"),
  summarise_team_metric(faceoffs, "FL", "teamAgainstId", "strengthAgainst"),
  summarise_team_metric(hits, "HG", "teamForId", "strengthFor"),
  summarise_team_metric(hits, "HT", "teamAgainstId", "strengthAgainst"),
  summarise_team_metric(takeaways, "TW", "teamForId", "strengthFor"),
  summarise_team_metric(giveaways, "GW", "teamForId", "strengthFor"),
  summarise_team_metric(penalties, "MD", "teamAgainstId", "strengthAgainst"),
  summarise_team_metric(penalties, "MC", "teamForId", "strengthFor"),
  summarise_team_metric(shots, "CF", "teamForId", "strengthFor"),
  summarise_team_metric(shots, "CA", "teamAgainstId", "strengthAgainst"),
  summarise_team_metric(shots, "FF", "teamForId", "strengthFor", value_col = "isFenwick"),
  summarise_team_metric(shots, "FA", "teamAgainstId", "strengthAgainst", value_col = "isFenwick"),
  summarise_team_metric(shots, "SF", "teamForId", "strengthFor", value_col = "isSOG"),
  summarise_team_metric(shots, "SA", "teamAgainstId", "strengthAgainst", value_col = "isSOG"),
  summarise_team_metric(shots, "GF", "teamForId", "strengthFor", value_col = "isGoal"),
  summarise_team_metric(shots, "GA", "teamAgainstId", "strengthAgainst", value_col = "isGoal"),
  summarise_team_metric(goals_ap1, "APF", "teamForId", "strengthFor"),
  summarise_team_metric(goals_ap1, "APA", "teamAgainstId", "strengthAgainst"),
  summarise_team_metric(goals_ap2, "ASF", "teamForId", "strengthFor"),
  summarise_team_metric(goals_ap2, "ASA", "teamAgainstId", "strengthAgainst"),
  summarise_team_metric(shots, "RSF", "teamForId", "strengthFor", value_col = "isRushVal"),
  summarise_team_metric(shots, "RSA", "teamAgainstId", "strengthAgainst", value_col = "isRushVal"),
  summarise_team_metric(shots, "RBF", "teamForId", "strengthFor", value_col = "isReboundVal"),
  summarise_team_metric(shots, "RBA", "teamAgainstId", "strengthAgainst", value_col = "isReboundVal"),
  summarise_team_metric(shots, "RCF", "teamForId", "strengthFor", value_col = "createdReboundVal"),
  summarise_team_metric(shots, "RCA", "teamAgainstId", "strengthAgainst", value_col = "createdReboundVal")
) %>%
  dplyr::filter(strength %in% c("ev", "pp", "sh"))

team_game_strength <- stats_long %>%
  tidyr::pivot_wider(
    names_from = metric,
    values_from = value,
    values_fill = 0
  ) %>%
  dplyr::left_join(game_dates, by = c("gameId", "gameTypeId"))

team_game_strength <- ensure_cols(team_game_strength, all_metric_names)

teams_out <- team_game_strength %>%
  tidyr::pivot_wider(
    id_cols = c(teamId, gameId, gameTypeId, gameDate),
    names_from = strength,
    values_from = tidyselect::all_of(all_metric_names),
    names_glue = "{.value}_{strength}",
    values_fill = 0
  )

expected_cols <- make_expected_metric_cols(all_metric_names)
teams_out <- ensure_cols(teams_out, expected_cols) %>%
  dplyr::select(teamId, gameId, gameDate, tidyselect::all_of(expected_cols)) %>%
  dplyr::arrange(teamId, gameDate, gameId)

# ----- Write Files ----- #

out_dir <- file.path("data", "gbgs", "basic")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
split_dir <- file.path(out_dir, "team")
dir.create(split_dir, recursive = TRUE, showWarnings = FALSE)

season_path <- file.path(out_dir, paste0("teams_", SEASON, ".csv"))
readr::write_csv(teams_out, season_path)

if (nrow(teams_out) > 0) {
  team_ids <- sort(unique(teams_out$teamId))
  wrong_paths <- file.path(out_dir, paste0(team_ids, "_", SEASON, ".csv"))
  wrong_paths <- wrong_paths[file.exists(wrong_paths)]
  if (length(wrong_paths) > 0L) {
    invisible(file.remove(wrong_paths))
  }
  for (team_id in team_ids) {
    team_path <- file.path(split_dir, paste0(team_id, "_", SEASON, ".csv"))
    readr::write_csv(
      teams_out %>%
        dplyr::filter(teamId == team_id) %>%
        dplyr::arrange(gameDate, gameId) %>%
        dplyr::select(-teamId),
      team_path
    )
  }
}

cat("Wrote season file:", season_path, "\n")
cat("Rows:", nrow(teams_out), " Teams:", length(unique(teams_out$teamId)), "\n")
