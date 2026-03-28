normalize_shot_type <- function(x) {
  x <- stringr::str_to_lower(as.character(x))

  dplyr::case_when(
    x %in% c("backhand", "deflected", "slap", "snap", "tip-in", "wrist") ~ x,
    TRUE ~ "other"
  )
}

normalize_missed_reason <- function(x) {
  x <- stringr::str_to_lower(as.character(x))

  dplyr::case_when(
    x %in% c("goalpost", "hit-left-post", "hit-right-post", "hit-crossbar") ~ "post",
    x %in% c("over-net", "above-crossbar") ~ "high",
    x %in% c(
      "wide-of-net",
      "high-and-wide-left",
      "high-and-wide-right",
      "wide-left",
      "wide-right"
    ) ~ "wide",
    TRUE ~ "other"
  )
}

classify_xg_situations <- function(
    situation_code,
    is_empty_net_for,
    is_empty_net_against,
    skater_count_for,
    skater_count_against
) {
  situation_code <- as.character(situation_code)
  is_empty_net_for <- dplyr::coalesce(as.logical(is_empty_net_for), FALSE)
  is_empty_net_against <- dplyr::coalesce(as.logical(is_empty_net_against), FALSE)
  skater_count_for <- suppressWarnings(as.integer(skater_count_for))
  skater_count_against <- suppressWarnings(as.integer(skater_count_against))

  is_ps <- !is.na(situation_code) & situation_code %in% c("1010", "0101")
  is_en <- !is_ps & is_empty_net_against
  is_sd_standard <- (
    !is_ps &
      !is_en &
      !is.na(skater_count_for) &
      !is.na(skater_count_against) &
      skater_count_for == 5L &
      skater_count_against == 5L &
      !is_empty_net_for &
      !is_empty_net_against
  )
  is_ev <- (
    !is_ps &
      !is_en &
      !is.na(skater_count_for) &
      !is.na(skater_count_against) &
      skater_count_for == skater_count_against &
      !is_sd_standard
  )
  is_pp <- (
    !is_ps &
      !is_en &
      !is.na(skater_count_for) &
      !is.na(skater_count_against) &
      skater_count_for > skater_count_against
  )
  is_sh <- (
    !is_ps &
      !is_en &
      !is.na(skater_count_for) &
      !is.na(skater_count_against) &
      skater_count_for < skater_count_against
  )
  is_uncategorizable_partition <- !(
    is_ps |
      is_en |
      is_sd_standard |
      is_ev |
      is_pp |
      is_sh
  )
  is_sd <- is_sd_standard | is_uncategorizable_partition

  tibble::tibble(
    is_ps = is_ps,
    is_en = is_en,
    is_sd = is_sd,
    is_ev = is_ev,
    is_pp = is_pp,
    is_sh = is_sh,
    n_situations = (
      as.integer(is_ps) +
        as.integer(is_en) +
        as.integer(is_sd) +
        as.integer(is_ev) +
        as.integer(is_pp) +
        as.integer(is_sh)
    ),
    situation = dplyr::case_when(
      is_ps ~ "ps",
      is_en ~ "en",
      is_sd ~ "sd",
      is_ev ~ "ev",
      is_pp ~ "pp",
      is_sh ~ "sh",
      TRUE ~ NA_character_
    )
  )
}

append_xg_situation_columns <- function(data) {
  dplyr::bind_cols(
    data,
    classify_xg_situations(
      situation_code = data$situationCode,
      is_empty_net_for = data$isEmptyNetFor,
      is_empty_net_against = data$isEmptyNetAgainst,
      skater_count_for = data$skaterCountFor,
      skater_count_against = data$skaterCountAgainst
    )
  )
}

ensure_goalie_player_id_against <- function(data) {
  if ("goalieInNetId" %in% names(data)) {
    goalie_in_net <- suppressWarnings(as.integer(data$goalieInNetId))
    if ("goaliePlayerIdAgainst" %in% names(data)) {
      goalie_against <- suppressWarnings(as.integer(data$goaliePlayerIdAgainst))
      data$goaliePlayerIdAgainst <- dplyr::coalesce(goalie_in_net, goalie_against)
    } else {
      data$goaliePlayerIdAgainst <- goalie_in_net
    }
  }

  data
}

is_behind_net <- function(x_coord_norm, goal_line_x = 89) {
  !is.na(x_coord_norm) & x_coord_norm >= goal_line_x
}

is_royal_road <- function(y_coord_norm, d_y_n) {
  y_coord_norm_prev <- y_coord_norm - d_y_n

  !is.na(y_coord_norm) &
    !is.na(y_coord_norm_prev) &
    (y_coord_norm * y_coord_norm_prev) < 0
}

as_integer_vector <- function(x) {
  if (is.null(x)) integer() else as.integer(x)
}

as_numeric_vector <- function(x) {
  if (is.null(x)) numeric() else as.numeric(x)
}

aligned_skater_values <- function(player_ids, values, goalie_ids = integer()) {
  player_ids <- as_integer_vector(player_ids)
  values <- as_numeric_vector(values)

  if (length(player_ids) == 0L && length(values) == 0L) {
    return(numeric())
  }

  if (length(player_ids) != length(values)) {
    stop("Aligned player/value list length mismatch.")
  }

  keep <- !is.na(player_ids) & !(player_ids %in% as.integer(goalie_ids))
  values[keep]
}

safe_min_numeric <- function(x) {
  if (length(x) == 0L || all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

safe_max_numeric <- function(x) {
  if (length(x) == 0L || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

safe_mean_numeric <- function(x) {
  if (length(x) == 0L || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

extract_aligned_player_value <- function(player_ids, values, player_id) {
  if (is.na(player_id)) {
    return(NA_real_)
  }

  player_ids <- as_integer_vector(player_ids)
  values <- as_numeric_vector(values)

  if (length(player_ids) == 0L || length(values) == 0L) {
    return(NA_real_)
  }

  if (length(player_ids) != length(values)) {
    stop("Aligned player/value list length mismatch.")
  }

  idx <- match(as.integer(player_id), player_ids)
  if (is.na(idx)) NA_real_ else values[[idx]]
}

make_type_desc_key_prev <- function(
    type_desc_key_prev,
    reason_prev,
    shot_type_prev,
    event_owner_team_id_prev,
    event_owner_team_id
) {
  is_for <- !is.na(event_owner_team_id_prev) & event_owner_team_id_prev == event_owner_team_id

  dplyr::case_when(
    is.na(type_desc_key_prev) ~ NA_character_,
    type_desc_key_prev == "faceoff" & is_for ~ "won-faceoff",
    type_desc_key_prev == "faceoff" ~ "lost-faceoff",
    type_desc_key_prev == "shot-on-goal" & is_for ~ paste0(shot_type_prev, "-shot-on-goal-for"),
    type_desc_key_prev == "shot-on-goal" ~ paste0(shot_type_prev, "-shot-on-goal-against"),
    type_desc_key_prev == "hit" & is_for ~ "given-hit",
    type_desc_key_prev == "hit" ~ "taken-hit",
    type_desc_key_prev == "blocked-shot" & is_for ~ "blocked-shot-for",
    type_desc_key_prev == "blocked-shot" ~ "blocked-shot-against",
    type_desc_key_prev == "giveaway" & is_for ~ "giveaway-for",
    type_desc_key_prev == "giveaway" ~ "giveaway-against",
    type_desc_key_prev == "takeaway" & is_for ~ "takeaway-for",
    type_desc_key_prev == "takeaway" ~ "takeaway-against",
    type_desc_key_prev == "missed-shot" & is_for ~ paste0(
      normalize_missed_reason(reason_prev),
      "-missed-shot-for"
    ),
    type_desc_key_prev == "missed-shot" ~ paste0(
      normalize_missed_reason(reason_prev),
      "-missed-shot-against"
    ),
    TRUE ~ type_desc_key_prev
  )
}

add_missing_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))

  for (col in missing_cols) {
    data[[col]] <- NA
  }

  data
}

bind_rows_with_missing <- function(data_list) {
  all_cols <- Reduce(union, lapply(data_list, names))

  purrr::map_dfr(
    data_list,
    \(x) {
      x %>%
        add_missing_columns(all_cols) %>%
        dplyr::select(dplyr::all_of(all_cols))
    }
  )
}

extract_slot_indices <- function(data, suffix) {
  cols <- names(data)
  matches <- stringr::str_match(cols, paste0("^skater(\\d+)", suffix, "$"))

  matches[, 2] %>%
    stats::na.omit() %>%
    as.integer() %>%
    unique() %>%
    sort()
}

build_skater_slot_lists <- function(data, suffix, cast_fn) {
  slot_indices <- extract_slot_indices(data, suffix)

  if (length(slot_indices) == 0L) {
    return(rep(list(cast_fn(numeric())), nrow(data)))
  }

  slot_cols <- paste0("skater", slot_indices, suffix)
  slot_matrix <- do.call(
    cbind,
    lapply(
      slot_cols,
      function(col) {
        if (col %in% names(data)) {
          data[[col]]
        } else {
          rep(NA, nrow(data))
        }
      }
    )
  )

  lapply(
    seq_len(nrow(slot_matrix)),
    function(i) cast_fn(unname(slot_matrix[i, ]))
  )
}

add_shift_list_columns <- function(data) {
  data$playerIdsFor <- build_skater_slot_lists(data, "PlayerIdFor", as.integer)
  data$playerIdsAgainst <- build_skater_slot_lists(data, "PlayerIdAgainst", as.integer)
  data$secondsElapsedInShiftFor <- build_skater_slot_lists(
    data,
    "SecondsElapsedInShiftFor",
    as.numeric
  )
  data$secondsElapsedInShiftAgainst <- build_skater_slot_lists(
    data,
    "SecondsElapsedInShiftAgainst",
    as.numeric
  )
  data$secondsElapsedInPeriodSinceLastShiftFor <- build_skater_slot_lists(
    data,
    "SecondsElapsedInPeriodSinceLastShiftFor",
    as.numeric
  )
  data$secondsElapsedInPeriodSinceLastShiftAgainst <- build_skater_slot_lists(
    data,
    "SecondsElapsedInPeriodSinceLastShiftAgainst",
    as.numeric
  )

  data
}

normalize_xg_pbp_schema <- function(data) {
  data <- ensure_goalie_player_id_against(data)

  required_cols <- c(
    "eventTypeDescKey",
    "goaliePlayerIdAgainst",
    "periodNumber",
    "shotsFor",
    "shotsAgainst",
    "shotDifferential",
    "dXCoordNorm",
    "dYCoordNorm",
    "dDistance",
    "dAngle",
    "dSecondsElapsedInSequence",
    "dXCoordNormPerSecond",
    "dYCoordNormPerSecond",
    "dDistancePerSecond",
    "dAnglePerSecond"
  )

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      paste(
        "Missing required nhlscraper xG columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  data %>%
    dplyr::mutate(
      shotType = normalize_shot_type(shotType),
      reason = stringr::str_to_lower(as.character(reason))
    )
}

load_xg_season <- function(season) {
  cat(glue::glue("Loading {season}...\n"))

  nhlscraper::gc_pbps(season) %>%
    ensure_goalie_player_id_against() %>%
    nhlscraper::add_shift_times(nhlscraper::shift_charts(season)) %>%
    nhlscraper::add_deltas() %>%
    nhlscraper::add_shooter_biometrics() %>%
    ensure_goalie_player_id_against() %>%
    nhlscraper::add_goalie_biometrics() %>%
    ensure_goalie_player_id_against()
}

prepare_xg_shots <- function(seasons) {
  pbps_list <- purrr::map(seasons, load_xg_season)
  pbps <- bind_rows_with_missing(pbps_list)
  rm(pbps_list)

  pbps <- normalize_xg_pbp_schema(pbps)

  goalie_ids <- pbps %>%
    dplyr::distinct(goaliePlayerIdAgainst) %>%
    dplyr::filter(!is.na(goaliePlayerIdAgainst)) %>%
    dplyr::pull(goaliePlayerIdAgainst) %>%
    as.integer()

  prev_events <- pbps %>%
    dplyr::transmute(
      gameId,
      eventId,
      typeDescKeyPrevRaw = eventTypeDescKey,
      reasonPrev = reason,
      shotTypePrev = shotType,
      eventOwnerTeamIdPrev = eventOwnerTeamId
    )

  shots <- pbps %>%
    dplyr::filter(
      gameTypeId %in% 2:3,
      eventTypeDescKey %in% c("goal", "shot-on-goal", "missed-shot")
    ) %>%
    dplyr::left_join(
      prev_events,
      by = c("gameId", "eventIdPrev" = "eventId")
    ) %>%
    add_shift_list_columns() %>%
    dplyr::mutate(
      situationCode = as.character(situationCode),
      periodType = as.character(periodType),
      isEmptyNetFor = dplyr::coalesce(isEmptyNetFor, FALSE),
      isEmptyNetAgainst = dplyr::coalesce(isEmptyNetAgainst, FALSE),
      shootingPlayerId = dplyr::coalesce(shootingPlayerId, scoringPlayerId),
      typeDescKeyPrev = make_type_desc_key_prev(
        type_desc_key_prev = typeDescKeyPrevRaw,
        reason_prev = reasonPrev,
        shot_type_prev = shotTypePrev,
        event_owner_team_id_prev = eventOwnerTeamIdPrev,
        event_owner_team_id = eventOwnerTeamId
      )
    ) %>%
    append_xg_situation_columns() %>%
    dplyr::filter(is.na(shootingPlayerId) | !(shootingPlayerId %in% goalie_ids))

  required_shift_cols <- c(
    "playerIdsFor",
    "playerIdsAgainst",
    "secondsElapsedInShiftFor",
    "secondsElapsedInShiftAgainst",
    "secondsElapsedInPeriodSinceLastShiftFor",
    "secondsElapsedInPeriodSinceLastShiftAgainst",
    "dYCoordNorm"
  )

  missing_shift_cols <- setdiff(required_shift_cols, names(shots))
  if (length(missing_shift_cols) > 0L) {
    stop(
      paste(
        "Missing required xG feature columns:",
        paste(missing_shift_cols, collapse = ", ")
      )
    )
  }

  shift_elapsed_for_skater <- purrr::map2(
    shots$playerIdsFor,
    shots$secondsElapsedInShiftFor,
    aligned_skater_values,
    goalie_ids = goalie_ids
  )

  shift_elapsed_against_skater <- purrr::map2(
    shots$playerIdsAgainst,
    shots$secondsElapsedInShiftAgainst,
    aligned_skater_values,
    goalie_ids = goalie_ids
  )

  shift_rest_for_skater <- purrr::map2(
    shots$playerIdsFor,
    shots$secondsElapsedInPeriodSinceLastShiftFor,
    aligned_skater_values,
    goalie_ids = goalie_ids
  )

  shift_rest_against_skater <- purrr::map2(
    shots$playerIdsAgainst,
    shots$secondsElapsedInPeriodSinceLastShiftAgainst,
    aligned_skater_values,
    goalie_ids = goalie_ids
  )

  shots <- shots %>%
    dplyr::mutate(
      isGoal = eventTypeDescKey == "goal",
      isPlayoff = gameTypeId == 3,
      isOvertime = periodType == "OT",
      isBehindNet = is_behind_net(xCoordNorm),
      crossedRoyalRoad = is_royal_road(yCoordNorm, dYCoordNorm),
      minSecondsElapsedInShiftFor = purrr::map_dbl(shift_elapsed_for_skater, safe_min_numeric),
      maxSecondsElapsedInShiftFor = purrr::map_dbl(shift_elapsed_for_skater, safe_max_numeric),
      avgSecondsElapsedInShiftFor = purrr::map_dbl(shift_elapsed_for_skater, safe_mean_numeric),
      minSecondsElapsedInShiftAgainst = purrr::map_dbl(shift_elapsed_against_skater, safe_min_numeric),
      maxSecondsElapsedInShiftAgainst = purrr::map_dbl(shift_elapsed_against_skater, safe_max_numeric),
      avgSecondsElapsedInShiftAgainst = purrr::map_dbl(shift_elapsed_against_skater, safe_mean_numeric),
      minSecondsElapsedSinceLastShiftFor = purrr::map_dbl(shift_rest_for_skater, safe_min_numeric),
      maxSecondsElapsedSinceLastShiftFor = purrr::map_dbl(shift_rest_for_skater, safe_max_numeric),
      avgSecondsElapsedSinceLastShiftFor = purrr::map_dbl(shift_rest_for_skater, safe_mean_numeric),
      minSecondsElapsedSinceLastShiftAgainst = purrr::map_dbl(shift_rest_against_skater, safe_min_numeric),
      maxSecondsElapsedSinceLastShiftAgainst = purrr::map_dbl(shift_rest_against_skater, safe_max_numeric),
      avgSecondsElapsedSinceLastShiftAgainst = purrr::map_dbl(shift_rest_against_skater, safe_mean_numeric),
      shooterSecondsElapsedInShift = purrr::pmap_dbl(
        list(playerIdsFor, secondsElapsedInShiftFor, shootingPlayerId),
        extract_aligned_player_value
      ),
      shooterSecondsElapsedSinceLastShift = purrr::pmap_dbl(
        list(
          playerIdsFor,
          secondsElapsedInPeriodSinceLastShiftFor,
          shootingPlayerId
        ),
        extract_aligned_player_value
      )
    )

  if (any(shots$n_situations != 1, na.rm = TRUE) || any(is.na(shots$n_situations))) {
    print(shots %>% dplyr::count(n_situations, sort = TRUE))
    stop("Situation definitions are not mutually exclusive and collectively exhaustive.")
  }

  shots
}

get_xg_partition_columns <- function() {
  id_cols <- c("gameId", "eventId")

  sd_predictor_cols <- c(
    "isPlayoff",
    "isHome",
    "isOvertime",
    "periodNumber",
    "secondsElapsedInPeriod",
    "secondsElapsedInGame",
    "secondsElapsedInSequence",
    "zoneCode",
    "xCoordNorm",
    "yCoordNorm",
    "dXCoordNorm",
    "dYCoordNorm",
    "distance",
    "angle",
    "dDistance",
    "dAngle",
    "dSecondsElapsedInSequence",
    "dXCoordNormPerSecond",
    "dYCoordNormPerSecond",
    "dDistancePerSecond",
    "dAnglePerSecond",
    "isBehindNet",
    "crossedRoyalRoad",
    "typeDescKeyPrev",
    "shotType",
    "isRebound",
    "isRush",
    "goalsFor",
    "goalsAgainst",
    "goalDifferential",
    "shotsFor",
    "shotsAgainst",
    "shotDifferential",
    "fenwickFor",
    "fenwickAgainst",
    "fenwickDifferential",
    "corsiFor",
    "corsiAgainst",
    "corsiDifferential",
    "shooterHeight",
    "shooterWeight",
    "shooterHandCode",
    "shooterPositionCode",
    "shooterAge",
    "shooterSecondsElapsedInShift",
    "shooterSecondsElapsedSinceLastShift",
    "goalieHeight",
    "goalieWeight",
    "goalieHandCode",
    "goalieAge",
    "minSecondsElapsedInShiftFor",
    "maxSecondsElapsedInShiftFor",
    "avgSecondsElapsedInShiftFor",
    "minSecondsElapsedInShiftAgainst",
    "maxSecondsElapsedInShiftAgainst",
    "avgSecondsElapsedInShiftAgainst",
    "minSecondsElapsedSinceLastShiftFor",
    "maxSecondsElapsedSinceLastShiftFor",
    "avgSecondsElapsedSinceLastShiftFor",
    "minSecondsElapsedSinceLastShiftAgainst",
    "maxSecondsElapsedSinceLastShiftAgainst",
    "avgSecondsElapsedSinceLastShiftAgainst"
  )

  ev_extra_predictor_cols <- c(
    "isEmptyNetFor",
    "skaterCountFor",
    "skaterCountAgainst",
    "manDifferential",
    "strengthState"
  )

  ps_predictor_cols <- c(
    "isPlayoff",
    "isHome",
    "xCoordNorm",
    "yCoordNorm",
    "distance",
    "angle",
    "shotType",
    "goalsFor",
    "goalsAgainst",
    "goalDifferential",
    "shotsFor",
    "shotsAgainst",
    "shotDifferential",
    "fenwickFor",
    "fenwickAgainst",
    "fenwickDifferential",
    "corsiFor",
    "corsiAgainst",
    "corsiDifferential",
    "shooterHeight",
    "shooterWeight",
    "shooterHandCode",
    "shooterPositionCode",
    "shooterAge",
    "goalieHeight",
    "goalieWeight",
    "goalieHandCode",
    "goalieAge"
  )

  response_cols <- "isGoal"
  sd_cols <- c(id_cols, sd_predictor_cols, response_cols)
  ev_cols <- c(id_cols, sd_predictor_cols, ev_extra_predictor_cols, response_cols)
  en_cols <- setdiff(
    ev_cols,
    c("goalieHeight", "goalieWeight", "goalieHandCode", "goalieAge")
  )
  ps_cols <- c(id_cols, ps_predictor_cols, response_cols)

  list(
    sd = sd_cols,
    ev = ev_cols,
    pp = ev_cols,
    sh = ev_cols,
    en = en_cols,
    ps = ps_cols
  )
}

assert_required_columns <- function(data, cols, label) {
  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      paste(
        "Missing required columns for",
        label,
        ":",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}

build_xg_partitions <- function(shots) {
  partition_cols <- get_xg_partition_columns()

  partitions <- list(
    sd = shots %>% dplyr::filter(situation == "sd"),
    ev = shots %>% dplyr::filter(situation == "ev"),
    pp = shots %>% dplyr::filter(situation == "pp"),
    sh = shots %>% dplyr::filter(situation == "sh"),
    en = shots %>% dplyr::filter(situation == "en"),
    ps = shots %>% dplyr::filter(situation == "ps")
  )

  purrr::imap(
    partitions,
    function(data, key) {
      cols <- partition_cols[[key]]
      assert_required_columns(data, cols, key)
      data %>%
        dplyr::select(dplyr::all_of(cols))
    }
  )
}

write_partition_csv <- function(data, cols, path, label) {
  assert_required_columns(data, cols, label)

  data %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    readr::write_csv(path)
}

season_id_from_game_id <- function(game_id) {
  season_start <- as.integer(substr(as.character(game_id), 1, 4))
  as.integer(paste0(season_start, season_start + 1L))
}
