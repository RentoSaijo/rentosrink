# ----- Setup ----- #

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nhlscraper))

SEASON <- 20242025

# ----- Helpers ----- #

`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (length(x) == 1L && is.na(x)) return(y)
  x
}

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
    if (is.null(ids) || length(ids) == 0L) {
      return(integer())
    }
    if (length(ids) == 1L && is.na(ids)) {
      return(integer())
    }
    as.integer(ids)
  })
}

empty_metric_tbl <- function(metric) {
  tibble::tibble(
    playerId = integer(),
    gameId = integer(),
    gameTypeId = integer(),
    strength = character(),
    !!metric := double()
  )
}

summarise_individual <- function(df, metric, player_col, strength_col, value_col = "value") {
  if (nrow(df) == 0) return(empty_metric_tbl(metric))

  out <- df %>%
    dplyr::transmute(
      playerId = as.integer(.data[[player_col]]),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(gameTypeId),
      strength = as.character(.data[[strength_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(playerId), !is.na(strength), !is.na(value))

  if (nrow(out) == 0) return(empty_metric_tbl(metric))

  out %>%
    dplyr::group_by(playerId, gameId, gameTypeId, strength) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(!!metric := value)
}

summarise_onice <- function(
    df,
    metric,
    ids_col,
    strength_col,
    value_col = "value",
    drop_player_ids = integer()
) {
  if (nrow(df) == 0) return(empty_metric_tbl(metric))

  out <- df %>%
    dplyr::transmute(
      playerIds = normalize_id_list(.data[[ids_col]]),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(gameTypeId),
      strength = as.character(.data[[strength_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(strength), !is.na(value))

  if (nrow(out) == 0) return(empty_metric_tbl(metric))

  out <- out %>%
    tidyr::unnest_longer(playerIds, values_to = "playerId") %>%
    dplyr::mutate(playerId = as.integer(playerId)) %>%
    dplyr::filter(!is.na(playerId))

  if (length(drop_player_ids) > 0L) {
    out <- out %>% dplyr::filter(!playerId %in% drop_player_ids)
  }

  if (nrow(out) == 0) return(empty_metric_tbl(metric))

  out %>%
    dplyr::group_by(playerId, gameId, gameTypeId, strength) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(!!metric := value)
}

metric_to_long <- function(df, metric) {
  if (nrow(df) == 0) {
    return(tibble::tibble(
      playerId = integer(),
      gameId = integer(),
      gameTypeId = integer(),
      strength = character(),
      metric = character(),
      value = double()
    ))
  }

  df %>%
    dplyr::transmute(
      playerId,
      gameId,
      gameTypeId,
      strength,
      metric = metric,
      value = as.numeric(.data[[metric]])
    )
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

safe_skater_game_toi <- function(season, game_type) {
  out <- tryCatch(
    nhlscraper::skater_game_report(
      season = season,
      game_type = game_type,
      category = "timeonice"
    ),
    error = function(e) tibble::tibble()
  )

  if (nrow(out) == 0) {
    return(tibble::tibble(
      playerId = integer(),
      gameId = integer(),
      gameTypeId = integer(),
      gameDate = as.Date(character()),
      strength = character(),
      mP = double()
    ))
  }

  ev <- if ("evTimeOnIce" %in% names(out)) as.numeric(out$evTimeOnIce) else rep(0, nrow(out))
  pp <- if ("ppTimeOnIce" %in% names(out)) as.numeric(out$ppTimeOnIce) else rep(0, nrow(out))
  sh <- if ("shTimeOnIce" %in% names(out)) as.numeric(out$shTimeOnIce) else rep(0, nrow(out))

  out %>%
    dplyr::transmute(
      playerId = as.integer(playerId),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(game_type),
      gameDate = as.Date(gameDate),
      ev = dplyr::coalesce(ev, 0) / 60,
      pp = dplyr::coalesce(pp, 0) / 60,
      sh = dplyr::coalesce(sh, 0) / 60
    ) %>%
    tidyr::pivot_longer(
      cols = c(ev, pp, sh),
      names_to = "strength",
      values_to = "mP"
    )
}

first_non_na_date <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0L) as.Date(NA) else vals[[1]]
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

# ----- Load Data ----- #

cat("Loading pbp and shift data...\n")
pbps <- nhlscraper::gc_pbps(SEASON)
shifts <- nhlscraper::shift_charts(SEASON)
pbp <- nhlscraper::add_on_ice_players(pbps, shifts) %>%
  dplyr::filter(
    gameTypeId %in% c(2L, 3L),
    !(gameTypeId == 2L & period == 5L)
  )

pbp <- pbp %>%
  dplyr::mutate(
    strengthFor = normalize_strength_state(strengthState),
    strengthAgainst = flip_strength_code(strengthFor),
    playerIdsFor = normalize_id_list(playerIdsFor),
    playerIdsAgainst = normalize_id_list(playerIdsAgainst),
    shooterId = as.integer(dplyr::coalesce(shootingPlayerId, scoringPlayerId)),
    duration = dplyr::coalesce(as.numeric(duration), 0),
    isRush = dplyr::coalesce(as.logical(isRush), FALSE),
    isRebound = dplyr::coalesce(as.logical(isRebound), FALSE),
    createdReboundFlag = dplyr::coalesce(as.logical(createdRebound), FALSE)
  )

goalie_ids <- pbp %>%
  dplyr::distinct(goalieInNetId) %>%
  dplyr::filter(!is.na(goalieInNetId)) %>%
  dplyr::pull(goalieInNetId) %>%
  as.integer()

cat("Loading TOI and game-date data...\n")
toi_long <- dplyr::bind_rows(
  safe_skater_game_toi(SEASON, 2L),
  safe_skater_game_toi(SEASON, 3L)
)
skater_ids <- sort(unique(toi_long$playerId))

games <- nhlscraper::games() %>%
  dplyr::filter(seasonId == SEASON, gameTypeId %in% c(2L, 3L)) %>%
  dplyr::transmute(
    gameId = as.integer(gameId),
    gameTypeId = as.integer(gameTypeId),
    gameDate = as.Date(gameDate)
  )

toi_dates <- toi_long %>%
  dplyr::distinct(gameId, gameTypeId, gameDate)

game_dates <- dplyr::bind_rows(games, toi_dates) %>%
  dplyr::group_by(gameId, gameTypeId) %>%
  dplyr::summarise(gameDate = first_non_na_date(gameDate), .groups = "drop")

# ----- Metrics ----- #

metric_longs <- list(
  toi_long %>%
    dplyr::transmute(
      playerId = as.integer(playerId),
      gameId = as.integer(gameId),
      gameTypeId = as.integer(gameTypeId),
      strength = as.character(strength),
      metric = "mP",
      value = as.numeric(mP)
    )
)

faceoffs <- pbp %>% dplyr::filter(typeDescKey == "faceoff") %>% dplyr::mutate(value = 1)

metric_longs <- c(metric_longs, list(
  metric_to_long(summarise_individual(faceoffs, "iFW", "winningPlayerId", "strengthFor"), "iFW"),
  metric_to_long(summarise_onice(faceoffs, "oFW", "playerIdsFor", "strengthFor", drop_player_ids = goalie_ids), "oFW"),
  metric_to_long(summarise_individual(faceoffs, "iFL", "losingPlayerId", "strengthAgainst"), "iFL"),
  metric_to_long(summarise_onice(faceoffs, "oFL", "playerIdsAgainst", "strengthAgainst", drop_player_ids = goalie_ids), "oFL")
))

hits <- pbp %>% dplyr::filter(typeDescKey == "hit") %>% dplyr::mutate(value = 1)

metric_longs <- c(metric_longs, list(
  metric_to_long(summarise_individual(hits, "iHG", "hittingPlayerId", "strengthFor"), "iHG"),
  metric_to_long(summarise_onice(hits, "oHG", "playerIdsFor", "strengthFor", drop_player_ids = goalie_ids), "oHG"),
  metric_to_long(summarise_individual(hits, "iHT", "hitteePlayerId", "strengthAgainst"), "iHT"),
  metric_to_long(summarise_onice(hits, "oHT", "playerIdsAgainst", "strengthAgainst", drop_player_ids = goalie_ids), "oHT")
))

takeaways <- pbp %>% dplyr::filter(typeDescKey == "takeaway") %>% dplyr::mutate(value = 1)
giveaways <- pbp %>% dplyr::filter(typeDescKey == "giveaway") %>% dplyr::mutate(value = 1)

metric_longs <- c(metric_longs, list(
  metric_to_long(summarise_individual(takeaways, "iTW", "playerId", "strengthFor"), "iTW"),
  metric_to_long(summarise_onice(takeaways, "oTW", "playerIdsFor", "strengthFor", drop_player_ids = goalie_ids), "oTW"),
  metric_to_long(summarise_individual(giveaways, "iGW", "playerId", "strengthFor"), "iGW"),
  metric_to_long(summarise_onice(giveaways, "oGW", "playerIdsFor", "strengthFor", drop_player_ids = goalie_ids), "oGW")
))

penalties <- pbp %>%
  dplyr::filter(typeDescKey == "penalty") %>%
  dplyr::mutate(value = pmax(duration, 0))

metric_longs <- c(metric_longs, list(
  metric_to_long(summarise_individual(penalties, "iMD", "drawnByPlayerId", "strengthAgainst"), "iMD"),
  metric_to_long(summarise_onice(penalties, "oMD", "playerIdsAgainst", "strengthAgainst", drop_player_ids = goalie_ids), "oMD"),
  metric_to_long(summarise_individual(penalties, "iMC", "committedByPlayerId", "strengthFor"), "iMC"),
  metric_to_long(summarise_onice(penalties, "oMC", "playerIdsFor", "strengthFor", drop_player_ids = goalie_ids), "oMC")
))

shots <- pbp %>%
  dplyr::filter(typeDescKey %in% c("goal", "shot-on-goal", "missed-shot", "blocked-shot")) %>%
  dplyr::mutate(
    value = 1,
    isFenwick = as.numeric(typeDescKey != "blocked-shot"),
    isSOG = as.numeric(typeDescKey %in% c("goal", "shot-on-goal")),
    isGoal = as.numeric(typeDescKey == "goal"),
    isRushVal = as.numeric(isRush),
    isReboundVal = as.numeric(isRebound),
    createdReboundVal = as.numeric(createdReboundFlag)
  )

metric_longs <- c(metric_longs, list(
  metric_to_long(summarise_individual(shots, "iCF", "shooterId", "strengthFor"), "iCF"),
  metric_to_long(summarise_onice(shots, "oCF", "playerIdsFor", "strengthFor", drop_player_ids = goalie_ids), "oCF"),
  metric_to_long(summarise_onice(shots, "oCA", "playerIdsAgainst", "strengthAgainst", drop_player_ids = goalie_ids), "oCA"),
  metric_to_long(summarise_individual(shots, "iFF", "shooterId", "strengthFor", value_col = "isFenwick"), "iFF"),
  metric_to_long(summarise_onice(shots, "oFF", "playerIdsFor", "strengthFor", value_col = "isFenwick", drop_player_ids = goalie_ids), "oFF"),
  metric_to_long(summarise_onice(shots, "oFA", "playerIdsAgainst", "strengthAgainst", value_col = "isFenwick", drop_player_ids = goalie_ids), "oFA"),
  metric_to_long(summarise_individual(shots, "iSF", "shooterId", "strengthFor", value_col = "isSOG"), "iSF"),
  metric_to_long(summarise_onice(shots, "oSF", "playerIdsFor", "strengthFor", value_col = "isSOG", drop_player_ids = goalie_ids), "oSF"),
  metric_to_long(summarise_onice(shots, "oSA", "playerIdsAgainst", "strengthAgainst", value_col = "isSOG", drop_player_ids = goalie_ids), "oSA"),
  metric_to_long(summarise_individual(shots, "iGF", "scoringPlayerId", "strengthFor", value_col = "isGoal"), "iGF"),
  metric_to_long(summarise_onice(shots, "oGF", "playerIdsFor", "strengthFor", value_col = "isGoal", drop_player_ids = goalie_ids), "oGF"),
  metric_to_long(summarise_onice(shots, "oGA", "playerIdsAgainst", "strengthAgainst", value_col = "isGoal", drop_player_ids = goalie_ids), "oGA"),
  metric_to_long(summarise_individual(shots, "iRSF", "shooterId", "strengthFor", value_col = "isRushVal"), "iRSF"),
  metric_to_long(summarise_onice(shots, "oRSF", "playerIdsFor", "strengthFor", value_col = "isRushVal", drop_player_ids = goalie_ids), "oRSF"),
  metric_to_long(summarise_onice(shots, "oRSA", "playerIdsAgainst", "strengthAgainst", value_col = "isRushVal", drop_player_ids = goalie_ids), "oRSA"),
  metric_to_long(summarise_individual(shots, "iRBF", "shooterId", "strengthFor", value_col = "isReboundVal"), "iRBF"),
  metric_to_long(summarise_onice(shots, "oRBF", "playerIdsFor", "strengthFor", value_col = "isReboundVal", drop_player_ids = goalie_ids), "oRBF"),
  metric_to_long(summarise_onice(shots, "oRBA", "playerIdsAgainst", "strengthAgainst", value_col = "isReboundVal", drop_player_ids = goalie_ids), "oRBA"),
  metric_to_long(summarise_individual(shots, "iRCF", "shooterId", "strengthFor", value_col = "createdReboundVal"), "iRCF"),
  metric_to_long(summarise_onice(shots, "oRCF", "playerIdsFor", "strengthFor", value_col = "createdReboundVal", drop_player_ids = goalie_ids), "oRCF"),
  metric_to_long(summarise_onice(shots, "oRCA", "playerIdsAgainst", "strengthAgainst", value_col = "createdReboundVal", drop_player_ids = goalie_ids), "oRCA")
))

goals <- pbp %>% dplyr::filter(typeDescKey == "goal")
goals_ap1 <- goals %>% dplyr::mutate(value = as.numeric(!is.na(assist1PlayerId))) %>% dplyr::filter(value > 0)
goals_ap2 <- goals %>% dplyr::mutate(value = as.numeric(!is.na(assist2PlayerId))) %>% dplyr::filter(value > 0)

metric_longs <- c(metric_longs, list(
  metric_to_long(summarise_individual(goals_ap1, "iAPF", "assist1PlayerId", "strengthFor"), "iAPF"),
  metric_to_long(summarise_onice(goals_ap1, "oAPF", "playerIdsFor", "strengthFor", drop_player_ids = goalie_ids), "oAPF"),
  metric_to_long(summarise_onice(goals_ap1, "oAPA", "playerIdsAgainst", "strengthAgainst", drop_player_ids = goalie_ids), "oAPA"),
  metric_to_long(summarise_individual(goals_ap2, "iASF", "assist2PlayerId", "strengthFor"), "iASF"),
  metric_to_long(summarise_onice(goals_ap2, "oASF", "playerIdsFor", "strengthFor", drop_player_ids = goalie_ids), "oASF"),
  metric_to_long(summarise_onice(goals_ap2, "oASA", "playerIdsAgainst", "strengthAgainst", drop_player_ids = goalie_ids), "oASA")
))

stats_long <- dplyr::bind_rows(metric_longs) %>%
  dplyr::filter(
    !is.na(playerId),
    !is.na(gameId),
    !is.na(gameTypeId),
    strength %in% c("ev", "pp", "sh")
  ) %>%
  dplyr::group_by(playerId, gameId, gameTypeId, strength, metric) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

metric_names <- c(
  "mP",
  "iFW", "oFW", "iFL", "oFL",
  "iHG", "oHG", "iHT", "oHT",
  "iTW", "oTW", "iGW", "oGW",
  "iMD", "oMD", "iMC", "oMC",
  "iCF", "oCF", "oCA",
  "iFF", "oFF", "oFA",
  "iSF", "oSF", "oSA",
  "iGF", "oGF", "oGA",
  "iAPF", "oAPF", "oAPA",
  "iASF", "oASF", "oASA",
  "iRSF", "oRSF", "oRSA",
  "iRBF", "oRBF", "oRBA",
  "iRCF", "oRCF", "oRCA"
)

player_game_strength <- stats_long %>%
  tidyr::pivot_wider(
    names_from = metric,
    values_from = value,
    values_fill = 0
  ) %>%
  dplyr::left_join(game_dates, by = c("gameId", "gameTypeId"))

player_game_strength <- ensure_cols(player_game_strength, metric_names)

skaters <- player_game_strength %>%
  tidyr::pivot_wider(
    id_cols = c(playerId, gameId, gameTypeId, gameDate),
    names_from = strength,
    values_from = tidyselect::all_of(metric_names),
    names_glue = "{.value}_{strength}",
    values_fill = 0
  )

expected_cols <- make_expected_metric_cols(metric_names)
skaters <- ensure_cols(skaters, expected_cols) %>%
  dplyr::select(playerId, gameId, gameTypeId, gameDate, tidyselect::all_of(expected_cols)) %>%
  dplyr::arrange(playerId, gameDate, gameId)

if (length(skater_ids) > 0L) {
  skaters <- skaters %>% dplyr::filter(playerId %in% skater_ids)
}

# ----- Write Files ----- #

out_dir <- file.path("data", "gbgs", "basic")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

season_path <- file.path(out_dir, paste0("skaters_", SEASON, ".csv"))

existing_player_files <- Sys.glob(file.path(out_dir, paste0("*_", SEASON, ".csv")))
existing_player_files <- setdiff(existing_player_files, season_path)
if (length(existing_player_files) > 0L) {
  invisible(file.remove(existing_player_files))
}

readr::write_csv(skaters, season_path)

if (nrow(skaters) > 0) {
  player_ids <- sort(unique(skaters$playerId))
  for (player_id in player_ids) {
    player_path <- file.path(out_dir, paste0(player_id, "_", SEASON, ".csv"))
    readr::write_csv(
      skaters %>%
        dplyr::filter(playerId == player_id) %>%
        dplyr::arrange(gameDate, gameId) %>%
        dplyr::select(-playerId),
      player_path
    )
  }
}

cat("Wrote season file:", season_path, "\n")
cat("Rows:", nrow(skaters), " Players:", length(unique(skaters$playerId)), "\n")
