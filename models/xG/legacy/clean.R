suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

source(file.path("models", "xG", "prepare.R"))

LEGACY_SEASON_MAP <- list(
  `1` = c(20102011L, 20112012L),
  `2` = c(20162017L, 20172018L)
)
OUTPUT_DIR <- file.path("models", "xG", "legacy")
OUTPUT_DATA_DIR <- file.path(OUTPUT_DIR, "data")

get_legacy_seasons <- function(version) {
  version_key <- as.character(as.integer(version))
  seasons <- LEGACY_SEASON_MAP[[version_key]]

  if (is.null(seasons)) {
    stop(sprintf("Unknown legacy version: %s", version))
  }

  seasons
}

prepare_legacy_xg_shots <- function(seasons) {
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

run_legacy_clean <- function(version) {
  seasons <- get_legacy_seasons(version)
  shots <- prepare_legacy_xg_shots(seasons)
  partitions <- build_xg_partitions(shots)
  partition_cols <- get_xg_partition_columns()

  dir.create(OUTPUT_DATA_DIR, recursive = TRUE, showWarnings = FALSE)

  purrr::iwalk(
    partitions,
    function(data, key) {
      write_partition_csv(
        data = data,
        cols = partition_cols[[key]],
        path = file.path(OUTPUT_DATA_DIR, paste0(key, version, "_train.csv")),
        label = paste0(key, version)
      )
    }
  )
}

run_all_legacy_clean <- function() {
  purrr::walk(names(LEGACY_SEASON_MAP), run_legacy_clean)
}
