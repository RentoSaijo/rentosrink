suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

MIN_OUTPUT_SEASON_ID <- 20132014L
MIN_TRANSACTION_SEASON_ID <- 20092010L

OUTPUT_SKATERS <- "models/contracts/data/skater_contracts.csv"
OUTPUT_GOALIES <- "models/contracts/data/goalie_contracts.csv"

caps <- tibble::tibble(
  season = c(
    "20132014",
    "20142015",
    "20152016",
    "20162017",
    "20172018",
    "20182019",
    "20192020",
    "20202021",
    "20212022",
    "20222023",
    "20232024",
    "20242025",
    "20252026",
    "20262027",
    "20272028"
  ),
  cap_millions = c(
    64.3,
    69.0,
    71.4,
    73.0,
    75.0,
    79.5,
    81.5,
    81.5,
    81.5,
    82.5,
    83.5,
    88.0,
    95.5,
    104.0,
    113.5
  )
) %>%
  dplyr::mutate(
    seasonId = as.integer(season),
    cap = cap_millions * 1e6
  ) %>%
  dplyr::select(seasonId, cap)

normalize_text <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\b(st|jr|sr|ii|iii|iv|v)\\b", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

compact_text <- function(x) {
  gsub(" ", "", normalize_text(x), fixed = TRUE)
}

first_token <- function(x) {
  stringr::word(x, 1)
}

last_token <- function(x) {
  stringr::word(x, -1)
}

safe_detect_word <- function(text, token) {
  escaped <- stringr::str_replace_all(token, "([^[:alnum:]])", "\\\\\\1")
  !is.na(token) &
    token != "" &
    stringr::str_detect(text, stringr::regex(paste0("\\b", escaped, "\\b")))
}

season_start_year <- function(season_id) {
  season_id %/% 10000L
}

map_espn_team_id <- function(display_name, tx_date) {
  dplyr::case_when(
    display_name == "Anaheim Ducks" ~ 24L,
    display_name == "Arizona Coyotes" ~ 53L,
    display_name == "Atlanta Thrashers" ~ 11L,
    display_name == "Boston Bruins" ~ 6L,
    display_name == "Buffalo Sabres" ~ 7L,
    display_name == "Calgary Flames" ~ 20L,
    display_name == "Carolina Hurricanes" ~ 12L,
    display_name == "Chicago Blackhawks" ~ 16L,
    display_name == "Colorado Avalanche" ~ 21L,
    display_name == "Columbus Blue Jackets" ~ 29L,
    display_name == "Dallas Stars" ~ 25L,
    display_name == "Detroit Red Wings" ~ 17L,
    display_name == "Edmonton Oilers" ~ 22L,
    display_name == "Florida Panthers" ~ 13L,
    display_name == "Los Angeles Kings" ~ 26L,
    display_name == "Minnesota Wild" ~ 30L,
    display_name == "Montreal Canadiens" ~ 8L,
    display_name == "Nashville Predators" ~ 18L,
    display_name == "New Jersey Devils" ~ 1L,
    display_name == "New York Islanders" ~ 2L,
    display_name == "New York Rangers" ~ 3L,
    display_name == "Ottawa Senators" ~ 9L,
    display_name == "Philadelphia Flyers" ~ 4L,
    display_name == "Phoenix Coyotes" & tx_date < as.Date("2014-07-01") ~ 27L,
    display_name == "Phoenix Coyotes" ~ 53L,
    display_name == "Pittsburgh Penguins" ~ 5L,
    display_name == "San Jose Sharks" ~ 28L,
    display_name == "Seattle Kraken" ~ 55L,
    display_name == "St. Louis Blues" ~ 19L,
    display_name == "Tampa Bay Lightning" ~ 14L,
    display_name == "Toronto Maple Leafs" ~ 10L,
    display_name %in% c("Utah Hockey Club", "Utah Mammoth") ~ 59L,
    display_name == "Vancouver Canucks" ~ 23L,
    display_name == "Vegas Golden Knights" ~ 54L,
    display_name == "Washington Capitals" ~ 15L,
    display_name == "Winnipeg Jets" ~ 52L,
    TRUE ~ NA_integer_
  )
}

map_team_id_from_tricode <- function(team_tricode) {
  dplyr::case_when(
    team_tricode %in% c("LA", "LAK") ~ 26L,
    team_tricode %in% c("NJ", "NJD") ~ 1L,
    team_tricode %in% c("SJ", "SJS") ~ 28L,
    team_tricode %in% c("TB", "TBL") ~ 14L,
    team_tricode == "UTA" ~ 59L,
    TRUE ~ teams()$teamId[match(team_tricode, teams()$teamTriCode)]
  )
}

build_signing_clauses <- function(season_ids) {
  clauses <- purrr::map_dfr(season_ids, function(season_id) {
    tx <- suppressMessages(nhlscraper::espn_transactions(season_id))

    if (!nrow(tx) || !all(c("date", "teamDisplayName", "description") %in% names(tx))) {
      return(tibble::tibble())
    }

    tx %>%
      dplyr::transmute(
        tx_date = as.Date(substr(.data[["date"]], 1, 10)),
        teamDisplayName = .data[["teamDisplayName"]],
        description = .data[["description"]]
      ) %>%
      dplyr::mutate(
        signedWithTeamId = map_espn_team_id(teamDisplayName, tx_date),
        clause = stringr::str_split(description, "(?<=\\.)\\s+")
      ) %>%
      tidyr::unnest_longer(clause) %>%
      dplyr::mutate(clause = stringr::str_squish(clause)) %>%
      dplyr::filter(!is.na(signedWithTeamId)) %>%
      dplyr::filter(stringr::str_detect(clause, "^(Signed|Agreed to terms with|Re-signed) ")) %>%
      dplyr::filter(
        stringr::str_detect(
          clause,
          stringr::regex("contract|remainder of the season", ignore_case = TRUE)
        )
      ) %>%
      dplyr::filter(
        !stringr::str_detect(
          clause,
          stringr::regex(
            paste(
              c(
                "professional tryout",
                "amateur tryout",
                "coach",
                "scouting staff",
                "general manager",
                "assistant coach",
                "skating coach",
                "affiliation",
                "qualifying offers",
                "president",
                "vice president",
                "advisor",
                "business operations",
                "chief operating officer",
                "director of player personnel",
                "hockey administration"
              ),
              collapse = "|"
            ),
            ignore_case = TRUE
          )
        )
      ) %>%
      dplyr::mutate(
        clause_norm = normalize_text(clause),
        clause_compact = compact_text(clause),
        term_tx = dplyr::case_when(
          stringr::str_detect(clause, "one-year") ~ 1L,
          stringr::str_detect(clause, "two-year") ~ 2L,
          stringr::str_detect(clause, "three-year") ~ 3L,
          stringr::str_detect(clause, "four-year") ~ 4L,
          stringr::str_detect(clause, "five-year") ~ 5L,
          stringr::str_detect(clause, "six-year") ~ 6L,
          stringr::str_detect(clause, "seven-year") ~ 7L,
          stringr::str_detect(clause, "eight-year") ~ 8L,
          TRUE ~ NA_integer_
        )
      ) %>%
      dplyr::distinct(signedWithTeamId, tx_date, clause, .keep_all = TRUE)
  })

  through_match <- stringr::str_match(clauses$clause, "through the (\\d{4})-(\\d{2}) season")
  start_year <- suppressWarnings(as.integer(through_match[, 2]))
  end_suffix <- suppressWarnings(as.integer(through_match[, 3]))
  end_year <- ifelse(
    is.na(start_year) | is.na(end_suffix),
    NA_integer_,
    (start_year %/% 100L) * 100L + end_suffix
  )
  end_year <- ifelse(
    !is.na(end_year) & !is.na(start_year) & end_year < start_year,
    end_year + 100L,
    end_year
  )
  end_season_id_tx <- rep(NA_integer_, length(start_year))
  valid_end_seasons <- !is.na(start_year) & !is.na(end_year)
  end_season_id_tx[valid_end_seasons] <- as.integer(
    paste0(start_year[valid_end_seasons], end_year[valid_end_seasons])
  )

  clauses %>%
    dplyr::mutate(endSeasonId_tx = end_season_id_tx)
}

match_signing_dates <- function(contracts_tbl, signing_clauses) {
  candidates <- contracts_tbl %>%
    dplyr::filter(startSeasonId >= MIN_TRANSACTION_SEASON_ID) %>%
    dplyr::transmute(
      contractRowId,
      playerFullName,
      signedWithTeamId,
      startSeasonId,
      endSeasonId,
      term,
      startYear = season_start_year(startSeasonId),
      windowStart = as.Date(sprintf("%d-07-01", startYear - 2L)),
      windowEnd = as.Date(sprintf("%d-06-30", startYear + 1L)),
      nameNorm = normalize_text(playerFullName),
      nameCompact = compact_text(playerFullName),
      firstName = first_token(nameNorm),
      lastName = last_token(nameNorm)
    ) %>%
    dplyr::inner_join(signing_clauses, by = "signedWithTeamId", relationship = "many-to-many") %>%
    dplyr::filter(tx_date >= windowStart, tx_date <= windowEnd) %>%
    dplyr::mutate(
      lastHit = safe_detect_word(clause_norm, lastName),
      fullHit = stringr::str_detect(clause_compact, stringr::fixed(nameCompact)),
      firstHit = nchar(firstName) > 1 & safe_detect_word(clause_norm, firstName),
      firstInitialHit = nchar(firstName) > 0 &
        stringr::str_detect(
          clause_norm,
          stringr::regex(paste0("\\b", substr(firstName, 1, 1), "\\.?\\b"))
        ),
      termOk = is.na(term_tx) | term_tx == term,
      endOk = is.na(endSeasonId_tx) | endSeasonId_tx == endSeasonId,
      resignHint = stringr::str_detect(clause, "^Re-signed ")
    ) %>%
    dplyr::filter(lastHit, termOk, endOk) %>%
    dplyr::mutate(
      nameScore = dplyr::case_when(
        fullHit ~ 100L,
        firstHit ~ 90L,
        firstInitialHit ~ 80L,
        TRUE ~ 70L
      ),
      dateTarget = as.Date(sprintf("%d-07-01", startYear)),
      dateGap = abs(as.integer(tx_date - dateTarget)),
      score = nameScore +
        dplyr::if_else(!is.na(term_tx), 10L, 0L) +
        dplyr::if_else(!is.na(endSeasonId_tx), 15L, 0L) +
        dplyr::if_else(resignHint, 3L, 0L)
    ) %>%
    dplyr::filter(nameScore >= 80L)

  candidates %>%
    dplyr::arrange(contractRowId, dplyr::desc(score), dateGap, tx_date) %>%
    dplyr::group_by(contractRowId) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      nextScore = dplyr::nth(score, 2, default = NA_integer_),
      nextGap = dplyr::nth(dateGap, 2, default = NA_integer_),
      nextDate = dplyr::nth(tx_date, 2, default = as.Date(NA)),
      isAccepted = is.na(nextScore) |
        score > nextScore |
        (score == nextScore & dateGap < nextGap) |
        (score == nextScore & dateGap == nextGap & tx_date < nextDate)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(rank == 1L, isAccepted) %>%
    dplyr::transmute(contractRowId, dateOfSigning = tx_date)
}

safe_read_gbg_file <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }

  readr::read_csv(path, show_col_types = FALSE)
}

extract_gbg_dates <- function(path, player_ids) {
  df <- safe_read_gbg_file(path)

  if (!nrow(df) || !all(c("playerId", "gameId", "gameDate") %in% names(df))) {
    return(tibble::tibble())
  }

  df %>%
    dplyr::filter(playerId %in% player_ids) %>%
    dplyr::transmute(
      playerId = as.integer(playerId),
      gameId = as.integer(gameId),
      gameDate = as.Date(gameDate)
    )
}

safe_skater_game_log <- function(season_id, game_type_id) {
  out <- tryCatch(
    suppressMessages(nhlscraper::skater_game_report(season_id, game_type_id, "timeonice")),
    error = function(e) tibble::tibble()
  )

  if (!nrow(out)) {
    return(tibble::tibble())
  }

  out %>%
    dplyr::transmute(
      playerId = as.integer(playerId),
      gameId = as.integer(gameId),
      gameDate = as.Date(gameDate),
      teamTriCode = as.character(teamTriCode)
    )
}

safe_goalie_game_log <- function(season_id, game_type_id) {
  out <- tryCatch(
    suppressMessages(nhlscraper::goalie_game_report(season_id, game_type_id, "summary")),
    error = function(e) tibble::tibble()
  )

  if (!nrow(out)) {
    return(tibble::tibble())
  }

  out %>%
    dplyr::transmute(
      playerId = as.integer(playerId),
      gameId = as.integer(gameId),
      gameDate = as.Date(gameDate),
      teamTriCode = as.character(teamTriCode)
    )
}

build_gbg_team_game_log <- function(player_ids, season_ids) {
  player_ids <- unique(as.integer(player_ids))

  gbg_dates <- purrr::map_dfr(season_ids, function(season_id) {
    skaters_path <- sprintf("data/gbgs/basic/skaters_%s.csv", season_id)
    goalies_path <- sprintf("data/gbgs/basic/goalies_%s.csv", season_id)

    skater_dates <- extract_gbg_dates(skaters_path, player_ids)
    goalie_dates <- extract_gbg_dates(goalies_path, player_ids)

    dplyr::bind_rows(skater_dates, goalie_dates)
  }) %>%
    dplyr::distinct(playerId, gameId, .keep_all = TRUE)

  report_teams <- purrr::map_dfr(season_ids, function(season_id) {
    dplyr::bind_rows(
      safe_skater_game_log(season_id, 2L),
      safe_skater_game_log(season_id, 3L),
      safe_goalie_game_log(season_id, 2L),
      safe_goalie_game_log(season_id, 3L)
    )
  }) %>%
    dplyr::filter(playerId %in% player_ids, !is.na(teamTriCode), teamTriCode != "") %>%
    dplyr::distinct(playerId, gameId, .keep_all = TRUE)

  gbg_dates %>%
    dplyr::left_join(report_teams, by = c("playerId", "gameId"), relationship = "many-to-one") %>%
    dplyr::mutate(
      gameDate = dplyr::coalesce(gameDate.x, gameDate.y),
      lastPlayedTeamId = map_team_id_from_tricode(teamTriCode)
    ) %>%
    dplyr::filter(!is.na(gameDate), !is.na(lastPlayedTeamId)) %>%
    dplyr::transmute(playerId, gameId, gameDate, lastPlayedTeamId)
}

contracts_raw <- nhlscraper::contracts() %>%
  dplyr::group_by(playerId) %>%
  dplyr::arrange(startSeasonId, .by_group = TRUE) %>%
  dplyr::mutate(
    contractNumber = dplyr::row_number(),
    isFirst = startSeasonId == min(startSeasonId, na.rm = TRUE),
    isLast = startSeasonId == max(startSeasonId, na.rm = TRUE),
    prevStartSeasonId = dplyr::lag(startSeasonId),
    prevTerm = dplyr::lag(term),
    prevAAV = dplyr::lag(aav)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(ageAtSigning), !is.na(aav))

bios <- nhlscraper::players() %>%
  dplyr::filter(playerId %in% contracts_raw$playerId) %>%
  dplyr::select(playerId, birthDate, height, weight, handCode, positionCode_bio = positionCode)

signing_clauses <- build_signing_clauses(
  sort(unique(contracts_raw$startSeasonId[contracts_raw$startSeasonId >= MIN_TRANSACTION_SEASON_ID]))
)

contracts_shared <- contracts_raw %>%
  dplyr::mutate(contractRowId = dplyr::row_number()) %>%
  dplyr::left_join(
    caps %>% dplyr::rename(startSeasonId = seasonId, cap = cap),
    by = "startSeasonId",
    relationship = "many-to-one"
  ) %>%
  dplyr::left_join(
    caps %>% dplyr::rename(prevStartSeasonId = seasonId, capPrev = cap),
    by = "prevStartSeasonId",
    relationship = "many-to-one"
  ) %>%
  dplyr::left_join(bios, by = "playerId", relationship = "many-to-one") %>%
  dplyr::mutate(
    positionCode = dplyr::coalesce(positionCode, positionCode_bio),
    aavP = aav / cap,
    prevAAVP = prevAAV / capPrev
  ) %>%
  dplyr::left_join(
    match_signing_dates(., signing_clauses),
    by = "contractRowId",
    relationship = "one-to-one"
  ) %>%
  dplyr::mutate(
    dateOfSigning = dplyr::coalesce(
      dateOfSigning,
      as.Date(sprintf("%d-07-01", season_start_year(startSeasonId)))
    )
  )

gbg_team_game_log <- build_gbg_team_game_log(
  player_ids = unique(contracts_shared$playerId),
  season_ids = sort(unique(contracts_shared$startSeasonId[contracts_shared$startSeasonId >= MIN_TRANSACTION_SEASON_ID]))
)

last_team_before_contract <- contracts_shared %>%
  dplyr::mutate(
    contractDecisionDate = dplyr::coalesce(
      dateOfSigning,
      as.Date(sprintf("%d-07-01", season_start_year(startSeasonId)))
    )
  ) %>%
  dplyr::select(contractRowId, playerId, contractDecisionDate) %>%
  dplyr::left_join(gbg_team_game_log, by = "playerId", relationship = "many-to-many") %>%
  dplyr::filter(gameDate < contractDecisionDate) %>%
  dplyr::arrange(contractRowId, dplyr::desc(gameDate), dplyr::desc(gameId)) %>%
  dplyr::group_by(contractRowId) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(contractRowId, lastPlayedTeamId)

contracts_shared <- contracts_shared %>%
  dplyr::left_join(last_team_before_contract, by = "contractRowId", relationship = "one-to-one") %>%
  dplyr::mutate(
    isResign = dplyr::if_else(
      isFirst,
      FALSE,
      dplyr::coalesce(signedWithTeamId == lastPlayedTeamId, FALSE),
      missing = FALSE
    )
  ) %>%
  dplyr::filter(startSeasonId >= MIN_OUTPUT_SEASON_ID) %>%
  dplyr::transmute(
    playerId,
    playerFullName,
    positionCode,
    signedWithTeamId,
    signedWithTeamTriCode,
    startSeasonId,
    endSeasonId,
    dateOfSigning,
    isFirst,
    isLast,
    birthDate,
    cap,
    ageAtSigning,
    contractNumber,
    prevStartSeasonId,
    prevTerm,
    prevAAVP,
    height,
    weight,
    handCode,
    term,
    aav,
    aavP,
    isResign
  )

contracts_skaters <- contracts_shared %>%
  dplyr::filter(positionCode != "G")

contracts_goalies <- contracts_shared %>%
  dplyr::filter(positionCode == "G")

dir.create("models/contracts/data", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(contracts_skaters, OUTPUT_SKATERS)
readr::write_csv(contracts_goalies, OUTPUT_GOALIES)

cat(sprintf("Wrote %s rows to %s\n", nrow(contracts_skaters), OUTPUT_SKATERS))
cat(sprintf("Wrote %s rows to %s\n", nrow(contracts_goalies), OUTPUT_GOALIES))
cat(sprintf("Matched signing dates for %s skater contracts\n", sum(!is.na(contracts_skaters$dateOfSigning))))
cat(sprintf("Matched signing dates for %s goalie contracts\n", sum(!is.na(contracts_goalies$dateOfSigning))))
