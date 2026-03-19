suppressPackageStartupMessages(library(arrow))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(nhlscraper))

CONTROL_MAX_DIST <- 5
CONTROL_SECURE_DIST <- 3.5
CONTROL_CLEARANCE_MIN <- 1
MIN_PASS_DISTANCE <- 4
MAX_LOOSE_FRAMES <- 12L
NEUTRAL_SAME_SECOND_EVENT_TYPES <- c("stoppage", "faceoff", "period-end", "period-start")

get_nhlscraper_internal <- function(name) {
  getFromNamespace(name, "nhlscraper")
}

calculate_distance_internal <- get_nhlscraper_internal(".calculate_distance")
calculate_angle_internal <- get_nhlscraper_internal(".calculate_angle")
apply_shot_context_internal <- get_nhlscraper_internal(".apply_shot_context")

first_non_na <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0L) {
    return(NA)
  }
  vals[[1]]
}

compute_replay_windows <- function(replays) {
  replays %>%
    dplyr::count(gameId, eventId, name = "n_frames") %>%
    dplyr::mutate(
      replay_span_seconds = (n_frames - 1) / 10,
      replay_sampled_seconds = n_frames / 10
    )
}

infer_game_context <- function(pbps) {
  pbps %>%
    dplyr::filter(!is.na(eventOwnerTeamId), !is.na(isHome)) %>%
    dplyr::group_by(gameId) %>%
    dplyr::summarise(
      homeTeamId = first_non_na(eventOwnerTeamId[isHome %in% TRUE]),
      awayTeamId = first_non_na(eventOwnerTeamId[isHome %in% FALSE]),
      homeTeamDefendingSideP1 = first_non_na(homeTeamDefendingSide),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      homeTeamId = as.integer(homeTeamId),
      awayTeamId = as.integer(awayTeamId),
      homeTeamDefendingSideP1 = as.character(homeTeamDefendingSideP1)
    )
}

derive_period_context <- function(seconds_elapsed_in_game, game_type_id) {
  if (is.na(seconds_elapsed_in_game) || is.na(game_type_id)) {
    return(list(
      periodNumber = NA_integer_,
      periodType = NA_character_,
      secondsElapsedInPeriod = NA_real_
    ))
  }

  seconds_elapsed_in_game <- as.numeric(seconds_elapsed_in_game)
  game_type_id <- as.integer(game_type_id)

  if (seconds_elapsed_in_game < 3600) {
    period_number <- floor(seconds_elapsed_in_game / 1200) + 1L
    period_base <- (period_number - 1L) * 1200
    period_type <- "REG"
  } else if (game_type_id == 3L) {
    period_number <- floor((seconds_elapsed_in_game - 3600) / 1200) + 4L
    period_base <- 3600 + (period_number - 4L) * 1200
    period_type <- "OT"
  } else if (seconds_elapsed_in_game < 3900) {
    period_number <- 4L
    period_base <- 3600
    period_type <- "OT"
  } else {
    period_number <- floor((seconds_elapsed_in_game - 3900) / 300) + 5L
    period_base <- 3900 + (period_number - 5L) * 300
    period_type <- "SO"
  }

  list(
    periodNumber = as.integer(period_number),
    periodType = as.character(period_type),
    secondsElapsedInPeriod = seconds_elapsed_in_game - period_base
  )
}

normalize_coordinates_for_team <- function(
    x_coord,
    y_coord,
    team_id,
    home_team_id,
    home_team_defending_side_p1,
    period_number
) {
  is_home <- !is.na(team_id) && !is.na(home_team_id) && team_id == home_team_id
  home_defends_left <- identical(home_team_defending_side_p1, "left")
  home_attacks_positive_x <- if (period_number %% 2L == 1L) {
    home_defends_left
  } else {
    !home_defends_left
  }
  event_owner_attacks_positive_x <- if (is_home) {
    home_attacks_positive_x
  } else {
    !home_attacks_positive_x
  }
  flip_xy <- !event_owner_attacks_positive_x

  list(
    isHome = is_home,
    xCoordNorm = if (flip_xy) -x_coord else x_coord,
    yCoordNorm = if (flip_xy) -y_coord else y_coord
  )
}

derive_zone_code <- function(x_coord_norm) {
  if (is.na(x_coord_norm)) {
    return(NA_character_)
  }

  if (x_coord_norm >= 25) {
    return("O")
  }

  if (x_coord_norm <= -25) {
    return("D")
  }

  "N"
}

get_frame_players <- function(frame_row, player_slot_count) {
  player_rows <- lapply(seq_len(player_slot_count), function(slot) {
    player_id <- frame_row[[paste0("player", slot, "PlayerId")]]
    team_id <- frame_row[[paste0("player", slot, "TeamId")]]
    if (is.na(player_id) || is.na(team_id)) {
      return(NULL)
    }

    data.frame(
      slot = slot,
      playerId = as.integer(player_id),
      teamId = as.integer(team_id)
    )
  })

  players <- dplyr::bind_rows(player_rows)

  if (nrow(players) == 0L) {
    return(players)
  }

  players %>%
    dplyr::distinct(teamId, playerId, .keep_all = TRUE) %>%
    dplyr::arrange(slot)
}

build_on_ice_columns <- function(
    frame_row,
    player_slot_count,
    home_team_id,
    away_team_id,
    home_goalie_hint,
    away_goalie_hint,
    is_home_event
) {
  out <- as.list(setNames(rep(NA_integer_, 28L), c(
    "homeGoaliePlayerId",
    "awayGoaliePlayerId",
    "goaliePlayerIdFor",
    "goaliePlayerIdAgainst",
    paste0("homeSkater", 1:6, "PlayerId"),
    paste0("awaySkater", 1:6, "PlayerId"),
    paste0("skater", 1:6, "PlayerIdFor"),
    paste0("skater", 1:6, "PlayerIdAgainst")
  )))

  players <- get_frame_players(frame_row, player_slot_count)

  if (nrow(players) == 0L) {
    return(out)
  }

  home_players <- players %>%
    dplyr::filter(teamId == home_team_id) %>%
    dplyr::pull(playerId)

  away_players <- players %>%
    dplyr::filter(teamId == away_team_id) %>%
    dplyr::pull(playerId)

  home_goalie <- if (!is.na(home_goalie_hint) && home_goalie_hint %in% home_players) {
    as.integer(home_goalie_hint)
  } else {
    NA_integer_
  }

  away_goalie <- if (!is.na(away_goalie_hint) && away_goalie_hint %in% away_players) {
    as.integer(away_goalie_hint)
  } else {
    NA_integer_
  }

  home_skaters <- setdiff(home_players, home_goalie)[1:min(6L, length(setdiff(home_players, home_goalie)))]
  away_skaters <- setdiff(away_players, away_goalie)[1:min(6L, length(setdiff(away_players, away_goalie)))]

  out$homeGoaliePlayerId <- home_goalie
  out$awayGoaliePlayerId <- away_goalie
  out$goaliePlayerIdFor <- if (is_home_event) home_goalie else away_goalie
  out$goaliePlayerIdAgainst <- if (is_home_event) away_goalie else home_goalie

  for (idx in seq_along(home_skaters)) {
    out[[paste0("homeSkater", idx, "PlayerId")]] <- as.integer(home_skaters[[idx]])
  }

  for (idx in seq_along(away_skaters)) {
    out[[paste0("awaySkater", idx, "PlayerId")]] <- as.integer(away_skaters[[idx]])
  }

  skaters_for <- if (is_home_event) home_skaters else away_skaters
  skaters_against <- if (is_home_event) away_skaters else home_skaters

  for (idx in seq_along(skaters_for)) {
    out[[paste0("skater", idx, "PlayerIdFor")]] <- as.integer(skaters_for[[idx]])
  }

  for (idx in seq_along(skaters_against)) {
    out[[paste0("skater", idx, "PlayerIdAgainst")]] <- as.integer(skaters_against[[idx]])
  }

  out
}

resolve_event_player_id <- function(event_row) {
  player_cols <- c(
    "playerId",
    "shootingPlayerId",
    "scoringPlayerId",
    "committedByPlayerId",
    "drawnByPlayerId",
    "winningPlayerId",
    "losingPlayerId",
    "hittingPlayerId",
    "hitteePlayerId",
    "blockingPlayerId"
  )

  for (nm in intersect(player_cols, names(event_row))) {
    val <- event_row[[nm]][[1]]
    if (!is.na(val)) {
      return(as.integer(val))
    }
  }

  NA_integer_
}

classify_candidate_pbp_overlap <- function(
    anchor_game_rows,
    pass_time,
    receive_time,
    receiver_player_id
) {
  events <- anchor_game_rows %>%
    dplyr::filter(
      !is.na(eventId),
      secondsElapsedInGame >= pass_time,
      secondsElapsedInGame <= receive_time
    )

  if (nrow(events) == 0L) {
    return(list(
      keep = TRUE,
      overlapType = "none",
      passSortTime = pass_time,
      receiveSortTime = receive_time
    ))
  }

  has_strict_between_second <- any(
    events$secondsElapsedInGame > floor(pass_time) &
      events$secondsElapsedInGame < floor(receive_time)
  )

  if (has_strict_between_second) {
    return(list(
      keep = FALSE,
      overlapType = "strict_between_second"
    ))
  }

  event_player_ids <- vapply(
    seq_len(nrow(events)),
    function(idx) {
      resolve_event_player_id(events[idx, , drop = FALSE])
    },
    integer(1L)
  )

  compatible_same_second <- vapply(
    seq_len(nrow(events)),
    function(idx) {
      event_type <- as.character(events$eventTypeDescKey[[idx]])
      event_player_id <- event_player_ids[[idx]]

      (!is.na(event_player_id) && event_player_id == receiver_player_id) ||
        (is.na(event_player_id) && event_type %in% NEUTRAL_SAME_SECOND_EVENT_TYPES)
    },
    logical(1L)
  )

  if (!all(compatible_same_second)) {
    return(list(
      keep = FALSE,
      overlapType = "same_second_incompatible"
    ))
  }

  conflict_second <- min(events$secondsElapsedInGame, na.rm = TRUE)
  receive_sort_time <- min(receive_time, conflict_second - 0.01)
  pass_sort_time <- min(pass_time, receive_sort_time - 0.01)

  list(
    keep = TRUE,
    overlapType = "same_second_compatible",
    passSortTime = pass_sort_time,
    receiveSortTime = receive_sort_time
  )
}

sync_public_shot_columns <- function(pbps) {
  if ("homeSOG" %in% names(pbps)) {
    pbps$homeShots <- pbps$homeSOG
    pbps$awayShots <- pbps$awaySOG
    pbps$shotsFor <- pbps$SOGFor
    pbps$shotsAgainst <- pbps$SOGAgainst
    pbps$shotDifferential <- pbps$SOGDifferential
    pbps$homeSOG <- NULL
    pbps$awaySOG <- NULL
    pbps$SOGFor <- NULL
    pbps$SOGAgainst <- NULL
    pbps$SOGDifferential <- NULL
  }

  pbps
}

find_anchor_row <- function(anchor_game_rows, event_time) {
  idx <- findInterval(event_time, anchor_game_rows$secondsElapsedInGame)
  if (idx < 1L) {
    idx <- 1L
  }
  anchor_game_rows[idx, , drop = FALSE]
}

make_synthetic_row <- function(
    anchor_row,
    frame_row,
    event_time,
    event_type_desc_key,
    player_id,
    team_id,
    game_context_row,
    player_slot_count,
    excluded_cols,
    trailing_cols
) {
  row <- anchor_row

  period_ctx <- derive_period_context(
    seconds_elapsed_in_game = event_time,
    game_type_id = row$gameTypeId[[1]]
  )

  norm_coords <- normalize_coordinates_for_team(
    x_coord = as.numeric(frame_row$puckXCoord),
    y_coord = as.numeric(frame_row$puckYCoord),
    team_id = as.integer(team_id),
    home_team_id = as.integer(game_context_row$homeTeamId),
    home_team_defending_side_p1 = as.character(game_context_row$homeTeamDefendingSideP1),
    period_number = period_ctx$periodNumber
  )

  row$eventTypeDescKey <- event_type_desc_key
  row$eventId <- NA_integer_
  row$eventTypeCode <- NA_integer_
  row$periodNumber <- period_ctx$periodNumber
  row$periodType <- period_ctx$periodType
  row$secondsElapsedInPeriod <- period_ctx$secondsElapsedInPeriod
  row$secondsElapsedInGame <- as.numeric(event_time)
  row$eventOwnerTeamId <- as.integer(team_id)
  row$isHome <- as.logical(norm_coords$isHome)
  row$homeTeamDefendingSide <- as.character(game_context_row$homeTeamDefendingSideP1)
  row$xCoord <- as.numeric(frame_row$puckXCoord)
  row$yCoord <- as.numeric(frame_row$puckYCoord)
  row$xCoordNorm <- as.numeric(norm_coords$xCoordNorm)
  row$yCoordNorm <- as.numeric(norm_coords$yCoordNorm)
  row$zoneCode <- derive_zone_code(row$xCoordNorm[[1]])
  row$playerId <- as.integer(player_id)

  on_ice_cols <- build_on_ice_columns(
    frame_row = frame_row,
    player_slot_count = player_slot_count,
    home_team_id = as.integer(game_context_row$homeTeamId),
    away_team_id = as.integer(game_context_row$awayTeamId),
    home_goalie_hint = anchor_row$homeGoaliePlayerId[[1]],
    away_goalie_hint = anchor_row$awayGoaliePlayerId[[1]],
    is_home_event = row$isHome[[1]]
  )

  for (nm in names(on_ice_cols)) {
    if (nm %in% names(row)) {
      row[[nm]] <- on_ice_cols[[nm]]
    }
  }

  derived_cols <- c(
    "distance",
    "angle",
    "isRush",
    "isRebound",
    "homeGoals",
    "awayGoals",
    "goalsFor",
    "goalsAgainst",
    "homeShots",
    "awayShots",
    "shotsFor",
    "shotsAgainst",
    "homeFenwick",
    "awayFenwick",
    "fenwickFor",
    "fenwickAgainst",
    "homeCorsi",
    "awayCorsi",
    "corsiFor",
    "corsiAgainst",
    "goalDifferential",
    "shotDifferential",
    "fenwickDifferential",
    "corsiDifferential"
  )

  for (nm in intersect(derived_cols, names(row))) {
    row[[nm]] <- NA
  }

  for (nm in intersect(excluded_cols, names(row))) {
    row[[nm]] <- NA
  }

  for (nm in intersect(trailing_cols, names(row))) {
    row[[nm]] <- NA
  }

  row
}

build_pass_rows_for_event <- function(
    replay_event,
    goal_window_row,
    anchor_game_rows,
    game_context_row,
    player_slot_count,
    excluded_cols,
    trailing_cols
) {
  n_frames <- nrow(replay_event)
  if (n_frames < 2L) {
    return(NULL)
  }

  goalie_ids_to_ignore <- unique(as.integer(c(
    first_non_na(anchor_game_rows$homeGoaliePlayerId),
    first_non_na(anchor_game_rows$awayGoaliePlayerId)
  )))
  goalie_ids_to_ignore <- goalie_ids_to_ignore[!is.na(goalie_ids_to_ignore)]

  candidate_passes <- extract_pass_candidates(
    replay_event = replay_event,
    goal_time = goal_window_row$goalSecondsElapsedInGame[[1]],
    player_slot_count = player_slot_count,
    exclude_player_ids = goalie_ids_to_ignore
  )

  if (nrow(candidate_passes) == 0L) {
    return(NULL)
  }

  synthetic_rows <- vector("list", length = 0L)
  synthetic_order <- 0L

  for (candidate_idx in seq_len(nrow(candidate_passes))) {
    candidate <- candidate_passes[candidate_idx, , drop = FALSE]

    pass_time <- candidate$passTime[[1]]
    receive_time <- candidate$receiveTime[[1]]

    overlap_check <- classify_candidate_pbp_overlap(
      anchor_game_rows = anchor_game_rows,
      pass_time = pass_time,
      receive_time = receive_time,
      receiver_player_id = candidate$receiverPlayerId[[1]]
    )

    if (!isTRUE(overlap_check$keep)) {
      next
    }

    pass_frame <- replay_event[candidate$passFrameIdx[[1]], , drop = FALSE]
    receive_frame <- replay_event[candidate$receiveFrameIdx[[1]], , drop = FALSE]

    pass_anchor <- find_anchor_row(anchor_game_rows, pass_time)
    receive_anchor <- find_anchor_row(anchor_game_rows, receive_time)

    synthetic_order <- synthetic_order + 1L
    pass_row <- make_synthetic_row(
      anchor_row = pass_anchor,
      frame_row = pass_frame,
      event_time = pass_time,
      event_type_desc_key = "pass",
      player_id = candidate$passerPlayerId[[1]],
      team_id = candidate$teamId[[1]],
      game_context_row = game_context_row,
      player_slot_count = player_slot_count,
      excluded_cols = excluded_cols,
      trailing_cols = trailing_cols
    )
    pass_row$sortTimeExact <- overlap_check$passSortTime
    pass_row$syntheticPriority <- 2L
    pass_row$syntheticOrder <- synthetic_order
    pass_row$eventIdGoal <- as.integer(goal_window_row$goalEventId[[1]])

    synthetic_order <- synthetic_order + 1L
    receive_row <- make_synthetic_row(
      anchor_row = receive_anchor,
      frame_row = receive_frame,
      event_time = receive_time,
      event_type_desc_key = "reception",
      player_id = candidate$receiverPlayerId[[1]],
      team_id = candidate$teamId[[1]],
      game_context_row = game_context_row,
      player_slot_count = player_slot_count,
      excluded_cols = excluded_cols,
      trailing_cols = trailing_cols
    )
    receive_row$sortTimeExact <- overlap_check$receiveSortTime
    receive_row$syntheticPriority <- 1L
    receive_row$syntheticOrder <- synthetic_order
    receive_row$eventIdGoal <- as.integer(goal_window_row$goalEventId[[1]])

    synthetic_rows[[length(synthetic_rows) + 1L]] <- pass_row
    synthetic_rows[[length(synthetic_rows) + 1L]] <- receive_row
  }

  if (length(synthetic_rows) == 0L) {
    return(NULL)
  }

  dplyr::bind_rows(synthetic_rows)
}

extract_pass_candidates <- function(
    replay_event,
    goal_time,
    player_slot_count,
    exclude_player_ids = integer(0)
) {
  n_frames <- nrow(replay_event)
  if (n_frames < 2L) {
    return(data.frame())
  }

  player_id_cols <- paste0("player", seq_len(player_slot_count), "PlayerId")
  team_id_cols <- paste0("player", seq_len(player_slot_count), "TeamId")
  x_cols <- paste0("player", seq_len(player_slot_count), "XCoord")
  y_cols <- paste0("player", seq_len(player_slot_count), "YCoord")

  player_ids <- as.matrix(replay_event[, player_id_cols, drop = FALSE])
  team_ids <- as.matrix(replay_event[, team_id_cols, drop = FALSE])
  player_x <- as.matrix(replay_event[, x_cols, drop = FALSE])
  player_y <- as.matrix(replay_event[, y_cols, drop = FALSE])
  puck_x <- as.numeric(replay_event$puckXCoord)
  puck_y <- as.numeric(replay_event$puckYCoord)

  dx <- sweep(player_x, 1L, puck_x, "-")
  dy <- sweep(player_y, 1L, puck_y, "-")
  dist_mat <- sqrt(dx ^ 2 + dy ^ 2)
  dist_mat[is.na(player_ids)] <- Inf
  if (length(exclude_player_ids) > 0L) {
    dist_mat[player_ids %in% exclude_player_ids] <- Inf
  }

  nearest_idx <- max.col(-dist_mat, ties.method = "first")
  nearest_dist <- dist_mat[cbind(seq_len(n_frames), nearest_idx)]

  second_dist <- apply(
    replace(
      dist_mat,
      cbind(seq_len(n_frames), nearest_idx),
      Inf
    ),
    1L,
    min
  )

  nearest_player_id <- player_ids[cbind(seq_len(n_frames), nearest_idx)]
  nearest_team_id <- team_ids[cbind(seq_len(n_frames), nearest_idx)]

  controlled <- is.finite(nearest_dist) &
    nearest_dist <= CONTROL_MAX_DIST &
    (
      nearest_dist <= CONTROL_SECURE_DIST |
      (second_dist - nearest_dist) >= CONTROL_CLEARANCE_MIN |
      is.infinite(second_dist)
    )

  control_key <- ifelse(
    controlled,
    paste(nearest_team_id, nearest_player_id, sep = "|"),
    "NA"
  )

  runs <- rle(control_key)
  seg_end <- cumsum(runs$lengths)
  seg_start <- seg_end - runs$lengths + 1L

  segments <- data.frame(
    segmentIndex = seq_along(runs$lengths),
    startFrame = seg_start,
    endFrame = seg_end,
    key = runs$values,
    nFrames = runs$lengths,
    stringsAsFactors = FALSE
  )

  segments$isControlled <- !is.na(segments$key) & segments$key != "NA"
  segments$teamId <- NA_integer_
  segments$playerId <- NA_integer_

  controlled_idx <- which(segments$isControlled)
  if (length(controlled_idx) > 0L) {
    parts <- do.call(
      rbind,
      strsplit(segments$key[controlled_idx], "\\|", fixed = FALSE)
    )
    segments$teamId[controlled_idx] <- as.integer(parts[, 1])
    segments$playerId[controlled_idx] <- as.integer(parts[, 2])
  }

  segments$minDist <- vapply(
    seq_len(nrow(segments)),
    function(idx) {
      vals <- nearest_dist[segments$startFrame[idx]:segments$endFrame[idx]]
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0L) {
        Inf
      } else {
        min(vals)
      }
    },
    numeric(1L)
  )

  segments$isStableControl <- segments$isControlled &
    (segments$nFrames >= 2L | segments$minDist <= CONTROL_SECURE_DIST)

  max_time_stamp <- max(replay_event$timeStamp, na.rm = TRUE)
  candidates <- vector("list", length = 0L)

  for (seg_idx in seq_len(nrow(segments) - 1L)) {
    if (!segments$isStableControl[seg_idx]) {
      next
    }

    next_idx <- seg_idx + 1L
    while (next_idx <= nrow(segments) && !segments$isControlled[next_idx]) {
      next_idx <- next_idx + 1L
    }

    if (next_idx > nrow(segments) || !segments$isStableControl[next_idx]) {
      next
    }

    if (segments$teamId[seg_idx] != segments$teamId[next_idx]) {
      next
    }

    if (segments$playerId[seg_idx] == segments$playerId[next_idx]) {
      next
    }

    loose_frames <- segments$startFrame[next_idx] - segments$endFrame[seg_idx] - 1L
    if (loose_frames > MAX_LOOSE_FRAMES) {
      next
    }

    pass_frame_idx <- segments$endFrame[seg_idx]
    receive_frame_idx <- segments$startFrame[next_idx]

    travel_distance <- sqrt(
      (replay_event$puckXCoord[receive_frame_idx] - replay_event$puckXCoord[pass_frame_idx]) ^ 2 +
      (replay_event$puckYCoord[receive_frame_idx] - replay_event$puckYCoord[pass_frame_idx]) ^ 2
    )

    if (is.na(travel_distance) || travel_distance < MIN_PASS_DISTANCE) {
      next
    }

    pass_time <- goal_time - ((max_time_stamp - replay_event$timeStamp[pass_frame_idx]) / 10)
    receive_time <- goal_time - ((max_time_stamp - replay_event$timeStamp[receive_frame_idx]) / 10)

    candidates[[length(candidates) + 1L]] <- data.frame(
      teamId = as.integer(segments$teamId[seg_idx]),
      passerPlayerId = as.integer(segments$playerId[seg_idx]),
      receiverPlayerId = as.integer(segments$playerId[next_idx]),
      passFrameIdx = as.integer(pass_frame_idx),
      receiveFrameIdx = as.integer(receive_frame_idx),
      looseFrames = as.integer(loose_frames),
      travelDistance = as.numeric(travel_distance),
      passTime = as.numeric(pass_time),
      receiveTime = as.numeric(receive_time)
    )
  }

  if (length(candidates) == 0L) {
    return(data.frame())
  }

  dplyr::bind_rows(candidates)
}

trim_pbps_to_replay_windows <- function(pbps, replay_goal_windows) {
  pbps %>%
    dplyr::filter(gameTypeId %in% c(2L, 3L)) %>%
    dplyr::mutate(
      gameId = as.integer(gameId),
      eventId = as.integer(eventId),
      sortOrder = as.integer(sortOrder),
      secondsElapsedInGame = as.numeric(secondsElapsedInGame)
    ) %>%
    dplyr::inner_join(
      replay_goal_windows %>%
        dplyr::select(
          gameId,
          eventIdGoal = goalEventId,
          goalSecondsElapsedInGame,
          replay_start_seconds_elapsed
        ),
      by = "gameId",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      secondsElapsedInGame >= replay_start_seconds_elapsed,
      secondsElapsedInGame <= goalSecondsElapsedInGame
    ) %>%
    dplyr::mutate(eventIdGoal = as.integer(eventIdGoal)) %>%
    dplyr::select(-goalSecondsElapsedInGame, -replay_start_seconds_elapsed) %>%
    dplyr::arrange(gameId, eventIdGoal, sortOrder)
}

detect_passes_for_season <- function(season_id, replay_path) {
  cat(sprintf("Loading pbps for %s...\n", season_id))
  pbps <- nhlscraper::gc_pbps(season_id)

  cat(sprintf("Loading replays for %s...\n", season_id))
  replays <- arrow::read_parquet(replay_path, as_data_frame = TRUE)

  player_slot_count <- length(grep("^player[0-9]+PlayerId$", names(replays)))
  game_context <- infer_game_context(pbps)
  replay_windows <- compute_replay_windows(replays)

  goal_windows <- pbps %>%
    dplyr::filter(
      gameTypeId %in% c(2L, 3L),
      eventTypeDescKey == "goal",
      !(as.character(situationCode) %in% c("1010", "0101"))
    ) %>%
    dplyr::select(
      gameId,
      goalEventId = eventId,
      seasonId,
      gameTypeId,
      gameNumber,
      sortOrder,
      periodNumber,
      periodType,
      secondsElapsedInPeriod,
      secondsElapsedInGame,
      eventOwnerTeamId,
      isHome,
      situationCode
    ) %>%
    dplyr::left_join(game_context, by = "gameId") %>%
    dplyr::rename(
      goalSecondsElapsedInGame = secondsElapsedInGame,
      goalSortOrder = sortOrder,
      goalPeriodNumber = periodNumber,
      goalPeriodType = periodType,
      goalEventOwnerTeamId = eventOwnerTeamId,
      goalIsHome = isHome,
      goalSituationCode = situationCode
    )

  replay_goal_windows <- replay_windows %>%
    dplyr::inner_join(
      goal_windows,
      by = c("gameId", "eventId" = "goalEventId")
    ) %>%
    dplyr::rename(goalEventId = eventId) %>%
    dplyr::mutate(
      replay_start_seconds_elapsed = pmax(
        goalSecondsElapsedInGame - replay_span_seconds,
        0
      )
    ) %>%
    dplyr::arrange(gameId, goalEventId)

  cat("\nReplay window summary:\n")
  print(
    replay_goal_windows %>%
      dplyr::summarise(
        matched_goals = dplyr::n(),
        min_frames = min(n_frames),
        median_frames = median(n_frames),
        mean_frames = mean(n_frames),
        max_frames = max(n_frames),
        min_span_seconds = min(replay_span_seconds),
        median_span_seconds = median(replay_span_seconds),
        mean_span_seconds = mean(replay_span_seconds),
        max_span_seconds = max(replay_span_seconds)
      )
  )

  pbps_trimmed <- trim_pbps_to_replay_windows(pbps, replay_goal_windows)

  pbps_by_game <- split(
    pbps %>%
      dplyr::mutate(
        secondsElapsedInGame = as.numeric(secondsElapsedInGame),
        sortOrder = as.integer(sortOrder)
      ) %>%
      dplyr::arrange(gameId, secondsElapsedInGame, sortOrder),
    pbps$gameId
  )

  replays_by_event <- split(
    replays,
    interaction(replays$gameId, replays$eventId, drop = TRUE)
  )

  excluded_cols <- c(
    "eventId",
    "eventTypeCode",
    "situationCode",
    "homeIsEmptyNet",
    "awayIsEmptyNet",
    "isEmptyNetFor",
    "isEmptyNetAgainst",
    "homeSkaterCount",
    "awaySkaterCount",
    "skaterCountFor",
    "skaterCountAgainst",
    "manDifferential",
    "strengthState",
    "shotType",
    "createdRebound"
  )

  winning_idx <- match("winningPlayerId", names(pbps_trimmed))
  trailing_cols <- if (!is.na(winning_idx)) {
    names(pbps_trimmed)[winning_idx:length(names(pbps_trimmed))]
  } else {
    character()
  }

  cat("\nDetecting complete passes from replay possession changes...\n")
  synthetic_rows <- vector("list", length = nrow(replay_goal_windows))

  for (idx in seq_len(nrow(replay_goal_windows))) {
    goal_window_row <- replay_goal_windows[idx, , drop = FALSE]
    event_key <- paste(goal_window_row$gameId[[1]], goal_window_row$goalEventId[[1]], sep = ".")
    replay_event <- replays_by_event[[event_key]]
    anchor_game_rows <- pbps_by_game[[as.character(goal_window_row$gameId[[1]])]]
    game_context_row <- goal_window_row %>%
      dplyr::select(gameId, homeTeamId, awayTeamId, homeTeamDefendingSideP1)

    synthetic_rows[[idx]] <- build_pass_rows_for_event(
      replay_event = replay_event,
      goal_window_row = goal_window_row,
      anchor_game_rows = anchor_game_rows,
      game_context_row = game_context_row,
      player_slot_count = player_slot_count,
      excluded_cols = excluded_cols,
      trailing_cols = trailing_cols
    )
  }

  synthetic_rows <- dplyr::bind_rows(synthetic_rows)

  if (nrow(synthetic_rows) > 0L) {
    synthetic_rows <- synthetic_rows %>%
      dplyr::distinct(
        gameId,
        eventIdGoal,
        eventTypeDescKey,
        eventOwnerTeamId,
        playerId,
        secondsElapsedInGame,
        xCoord,
        yCoord,
        .keep_all = TRUE
      )
  }

  cat("\nSynthetic pass/reception summary:\n")
  print(
    if (nrow(synthetic_rows) == 0L) {
      tibble::tibble(
        synthetic_row_count = 0L,
        pass_rows = 0L,
        reception_rows = 0L
      )
    } else {
      tibble::tibble(
        synthetic_row_count = nrow(synthetic_rows),
        pass_rows = sum(synthetic_rows$eventTypeDescKey == "pass", na.rm = TRUE),
        reception_rows = sum(synthetic_rows$eventTypeDescKey == "reception", na.rm = TRUE)
      )
    }
  )

  combined_pbps <- pbps_trimmed %>%
    dplyr::mutate(
      sortTimeExact = as.numeric(secondsElapsedInGame),
      syntheticPriority = 3L,
      syntheticOrder = as.integer(sortOrder)
    )

  if (nrow(synthetic_rows) > 0L) {
    combined_pbps <- dplyr::bind_rows(combined_pbps, synthetic_rows)
  }

  combined_pbps <- combined_pbps %>%
    dplyr::arrange(gameId, eventIdGoal, sortTimeExact, syntheticPriority, syntheticOrder, sortOrder) %>%
    dplyr::group_by(gameId, eventIdGoal) %>%
    dplyr::mutate(sortOrder = dplyr::row_number()) %>%
    dplyr::ungroup()

  combined_pbps <- calculate_distance_internal(combined_pbps)
  combined_pbps <- calculate_angle_internal(combined_pbps)
  combined_pbps <- apply_shot_context_internal(combined_pbps)
  combined_pbps <- sync_public_shot_columns(combined_pbps)

  final_cols <- append(
    names(pbps),
    values = "eventIdGoal",
    after = match("eventId", names(pbps))
  )

  pbps_trimmed <- combined_pbps %>%
    dplyr::select(-sortTimeExact, -syntheticPriority, -syntheticOrder) %>%
    dplyr::select(dplyr::all_of(final_cols))

  cat("\nFinal trimmed pbps summary:\n")
  print(
    tibble::tibble(
      original_pbp_rows = nrow(pbps),
      final_trimmed_rows = nrow(pbps_trimmed),
      synthetic_pass_rows = sum(pbps_trimmed$eventTypeDescKey == "pass", na.rm = TRUE),
      synthetic_reception_rows = sum(pbps_trimmed$eventTypeDescKey == "reception", na.rm = TRUE),
      trimmed_games = dplyr::n_distinct(pbps_trimmed$gameId)
    )
  )

  cat("\nReplay coordinate formulas:\n")
  cat("  xCoord = (xCoordRaw - 1200) / 12\n")
  cat("  yCoord = (510 - yCoordRaw) / 12\n")
  cat("  nhlscraper::.normalize_coordinates() flips BOTH x and y when the event owner's team is attacking toward negative x.\n")

  list(
    pbps = pbps,
    replays = replays,
    pbps_trimmed = pbps_trimmed
  )
}

run_detect_batch <- function() {
  season_paths <- sort(Sys.glob(file.path("models", "passes", "data", "NHL_REPLAYS_*.parquet")))
  season_ids <- sub("^.*NHL_REPLAYS_([0-9]{8})\\.parquet$", "\\1", season_paths)

  if (length(season_paths) == 0L) {
    stop("No replay parquet files found in models/passes/data.")
  }

  pbps <- NULL
  replays <- NULL
  pbps_trimmed <- NULL

  for (idx in seq_along(season_paths)) {
    season_id <- season_ids[[idx]]
    replay_path <- season_paths[[idx]]
    result <- detect_passes_for_season(season_id = season_id, replay_path = replay_path)

    output_path <- file.path(
      "models",
      "passes",
      "data",
      sprintf("pbps_trimmed_%s.csv", season_id)
    )

    utils::write.csv(result$pbps_trimmed, output_path, row.names = FALSE)
    cat(sprintf("\nWrote %s\n\n", output_path))

    if (season_id == "20232024") {
      pbps <- result$pbps
      replays <- result$replays
      pbps_trimmed <- result$pbps_trimmed
    }

    rm(result)
  }

  keep <- c("pbps", "replays", "pbps_trimmed")
  rm(list = setdiff(ls(), keep))
  invisible(list(pbps = pbps, replays = replays, pbps_trimmed = pbps_trimmed))
}

if (sys.nframe() == 0L) {
  run_detect_batch()
}
