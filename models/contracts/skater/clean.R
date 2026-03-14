suppressMessages(library(tidyverse))

END_SEASON_ID <- 20252026L
VALIDATE_SEASON_ID <- END_SEASON_ID + 10001L
AGE_REF_DATE <- as.Date("2026-03-01")
FREE_AGENCY_STARTS <- tibble::tibble(
  seasonId = c(
    20052006L, 20062007L, 20072008L, 20082009L, 20092010L,
    20102011L, 20112012L, 20122013L, 20132014L, 20142015L,
    20152016L, 20162017L, 20172018L, 20182019L, 20192020L,
    20202021L, 20212022L, 20222023L, 20232024L, 20242025L,
    20252026L
  ),
  freeAgencyStart = as.Date(c(
    "2005-08-01", "2006-07-01", "2007-07-01", "2008-07-01", "2009-07-01",
    "2010-07-01", "2011-07-01", "2012-07-01", "2013-07-05", "2014-07-01",
    "2015-07-01", "2016-07-01", "2017-07-01", "2018-07-01", "2019-07-01",
    "2020-10-09", "2021-07-28", "2022-07-13", "2023-07-01", "2024-07-01",
    "2025-07-01"
  ))
)

INPUT_PATH <- "models/contracts/data/skater_contracts.csv"
TRAIN_PATH <- "models/contracts/data/skater_train.csv"
VALIDATE_PATH <- "models/contracts/data/skater_validate.csv"
TEST_PATH <- "models/contracts/data/skater_test.csv"

GBGS_BASIC_DIR <- "data/gbgs/basic"
GBGS_ADVANCED_DIR <- "data/gbgs/advanced"

REGULAR_WINDOW_SIZES <- c(regLast20 = 20L, regLast40 = 40L, regLast82 = 82L)
PLAYOFF_WINDOW_SIZES <- c(poLast5 = 5L, poLast10 = 10L)
TREND_METRICS <- c(
  "toiPerGame",
  "toiShare_pp",
  "toiShare_sh",
  "iCF_evPer60",
  "iSF_evPer60",
  "iGF_evPer60",
  "ixGF_evPer60",
  "oCF_evPer60",
  "oCA_evPer60",
  "oGF_evPer60",
  "oGA_evPer60",
  "oxGF_evPer60",
  "oxGA_evPer60",
  "cfPct_ev",
  "xGPct_ev",
  "iCFShare_ev",
  "ixGFShare_ev",
  "finishingAbovexG_evPer60"
)

CONTRACT_BASE_COLS <- c(
  "playerId",
  "cap",
  "capPrev",
  "startSeasonId",
  "startSeasonIdPrev",
  "term",
  "termPrev",
  "aav",
  "aavPrev",
  "aavPerc",
  "aavPercPrev",
  "dateOfSigning",
  "signedWithTeamId",
  "isResign",
  "contractNumber",
  "isLast",
  "birthDate",
  "ageAtSigning",
  "height",
  "weight",
  "handCode",
  "positionCode"
)

BASIC_GBG_COLS <- c(
  "playerId",
  "gameId",
  "gameDate",
  "mP_ev",
  "mP_pp",
  "mP_sh",
  "iFW_ev",
  "iFW_pp",
  "iFW_sh",
  "iFL_ev",
  "iFL_pp",
  "iFL_sh",
  "iHG_ev",
  "iHG_pp",
  "iHG_sh",
  "iHT_ev",
  "iHT_pp",
  "iHT_sh",
  "iTW_ev",
  "iTW_pp",
  "iTW_sh",
  "iGW_ev",
  "iGW_pp",
  "iGW_sh",
  "iMD_ev",
  "iMD_pp",
  "iMD_sh",
  "iMC_ev",
  "iMC_pp",
  "iMC_sh",
  "iCF_ev",
  "iCF_pp",
  "iCF_sh",
  "iSF_ev",
  "iSF_pp",
  "iSF_sh",
  "iGF_ev",
  "iGF_pp",
  "iGF_sh",
  "iAPF_ev",
  "iAPF_pp",
  "iAPF_sh",
  "iASF_ev",
  "iASF_pp",
  "iASF_sh",
  "oCF_ev",
  "oCF_pp",
  "oCF_sh",
  "oCA_ev",
  "oCA_pp",
  "oCA_sh",
  "oSF_ev",
  "oSF_pp",
  "oSF_sh",
  "oSA_ev",
  "oSA_pp",
  "oSA_sh",
  "oGF_ev",
  "oGF_pp",
  "oGF_sh",
  "oGA_ev",
  "oGA_pp",
  "oGA_sh"
)
ADVANCED_GBG_COLS <- c(
  "playerId",
  "gameId",
  "ixGF_ev",
  "ixGF_pp",
  "ixGF_sh",
  "oxGF_ev",
  "oxGF_pp",
  "oxGF_sh",
  "oxGA_ev",
  "oxGA_pp",
  "oxGA_sh"
)
GAME_VALUE_COLS <- setdiff(union(BASIC_GBG_COLS, ADVANCED_GBG_COLS), c("playerId", "gameId", "gameDate"))

season_start_year <- function(season_id) {
  season_id %/% 10000L
}

free_agency_start_date <- function(season_id) {
  mapped <- FREE_AGENCY_STARTS$freeAgencyStart[match(season_id, FREE_AGENCY_STARTS$seasonId)]
  fallback <- rep(as.Date(NA), length(season_id))
  valid <- !is.na(season_id)
  fallback[valid] <- as.Date(sprintf("%d-07-01", season_start_year(season_id[valid])))
  dplyr::coalesce(mapped, fallback)
}

age_on_date <- function(birth_date, reference_date) {
  birth_date <- as.Date(birth_date)
  reference_date <- as.Date(reference_date)

  birth_lt <- as.POSIXlt(birth_date)
  ref_lt <- as.POSIXlt(reference_date)

  years <- ref_lt$year - birth_lt$year
  before_birthday <- (ref_lt$mon < birth_lt$mon) |
    ((ref_lt$mon == birth_lt$mon) & (ref_lt$mday < birth_lt$mday))

  dplyr::if_else(
    is.na(birth_date) | is.na(reference_date),
    NA_real_,
    as.numeric(years - before_birthday)
  )
}

cap_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

safe_sum <- function(x) {
  sum(tidyr::replace_na(x, 0), na.rm = TRUE)
}

safe_rate <- function(value, minutes) {
  if (is.na(minutes) || minutes <= 0) {
    return(0)
  }

  60 * tidyr::replace_na(value, 0) / minutes
}

safe_pct <- function(for_value, against_value) {
  total_value <- tidyr::replace_na(for_value, 0) + tidyr::replace_na(against_value, 0)

  if (total_value <= 0) {
    return(0)
  }

  tidyr::replace_na(for_value, 0) / total_value
}

safe_share <- function(part_value, total_value) {
  if (is.na(total_value) || total_value <= 0) {
    return(0)
  }

  tidyr::replace_na(part_value, 0) / total_value
}

prefix_metric_name <- function(prefix, metric_name) {
  keep_lower_lead <- stringr::str_detect(metric_name, "^(i[A-Z]|o[A-Z]|x[A-Z]|ix[A-Z]|ox[A-Z])")

  metric_part <- if (keep_lower_lead) {
    metric_name
  } else {
    cap_first(metric_name)
  }

  paste0(prefix, metric_part)
}

load_skater_games <- function() {
  basic_paths <- list.files(
    path = GBGS_BASIC_DIR,
    pattern = "^skaters_[0-9]{8}\\.csv$",
    full.names = TRUE
  ) %>%
    sort()
  advanced_paths <- list.files(
    path = GBGS_ADVANCED_DIR,
    pattern = "^skaters_[0-9]{8}\\.csv$",
    full.names = TRUE
  ) %>%
    sort()

  basic <- purrr::map_dfr(
    basic_paths,
    ~ readr::read_csv(.x, show_col_types = FALSE, col_select = dplyr::all_of(BASIC_GBG_COLS))
  )
  advanced <- purrr::map_dfr(
    advanced_paths,
    ~ readr::read_csv(.x, show_col_types = FALSE, col_select = dplyr::all_of(ADVANCED_GBG_COLS))
  )

  basic %>%
    dplyr::left_join(advanced, by = c("playerId", "gameId"), relationship = "one-to-one") %>%
    dplyr::mutate(
      gameDate = as.Date(gameDate),
      gameType = dplyr::if_else(substr(as.character(gameId), 5, 6) == "03", "playoff", "regular"),
      totalToi = tidyr::replace_na(mP_ev, 0) + tidyr::replace_na(mP_pp, 0) + tidyr::replace_na(mP_sh, 0)
    ) %>%
    dplyr::arrange(playerId, gameDate, gameId)
}

slice_recent_window <- function(games, n_games) {
  if (!nrow(games)) {
    return(games)
  }

  games %>%
    dplyr::slice_tail(n = min(n_games, nrow(games)))
}

slice_prior_window <- function(games, skip_games, take_games) {
  if (!nrow(games) || nrow(games) <= skip_games) {
    return(games[0, , drop = FALSE])
  }

  end_idx <- nrow(games) - skip_games
  start_idx <- max(1L, end_idx - take_games + 1L)
  games[start_idx:end_idx, , drop = FALSE]
}

build_window_metrics <- function(games, window_size) {
  gp <- nrow(games)
  raw <- stats::setNames(rep(0, length(GAME_VALUE_COLS)), GAME_VALUE_COLS)

  if (gp > 0) {
    raw[names(games[GAME_VALUE_COLS])] <- colSums(games[GAME_VALUE_COLS], na.rm = TRUE)
  }

  total_toi <- tidyr::replace_na(raw[["mP_ev"]], 0) + tidyr::replace_na(raw[["mP_pp"]], 0) + tidyr::replace_na(raw[["mP_sh"]], 0)
  total_faceoff_wins <- tidyr::replace_na(raw[["iFW_ev"]], 0) + tidyr::replace_na(raw[["iFW_pp"]], 0) + tidyr::replace_na(raw[["iFW_sh"]], 0)
  total_faceoff_losses <- tidyr::replace_na(raw[["iFL_ev"]], 0) + tidyr::replace_na(raw[["iFL_pp"]], 0) + tidyr::replace_na(raw[["iFL_sh"]], 0)
  total_hits_given <- tidyr::replace_na(raw[["iHG_ev"]], 0) + tidyr::replace_na(raw[["iHG_pp"]], 0) + tidyr::replace_na(raw[["iHG_sh"]], 0)
  total_hits_taken <- tidyr::replace_na(raw[["iHT_ev"]], 0) + tidyr::replace_na(raw[["iHT_pp"]], 0) + tidyr::replace_na(raw[["iHT_sh"]], 0)
  total_takeaways <- tidyr::replace_na(raw[["iTW_ev"]], 0) + tidyr::replace_na(raw[["iTW_pp"]], 0) + tidyr::replace_na(raw[["iTW_sh"]], 0)
  total_giveaways <- tidyr::replace_na(raw[["iGW_ev"]], 0) + tidyr::replace_na(raw[["iGW_pp"]], 0) + tidyr::replace_na(raw[["iGW_sh"]], 0)
  total_penalties_drawn <- tidyr::replace_na(raw[["iMD_ev"]], 0) + tidyr::replace_na(raw[["iMD_pp"]], 0) + tidyr::replace_na(raw[["iMD_sh"]], 0)
  total_penalties_committed <- tidyr::replace_na(raw[["iMC_ev"]], 0) + tidyr::replace_na(raw[["iMC_pp"]], 0) + tidyr::replace_na(raw[["iMC_sh"]], 0)

  metrics <- list(
    gp = gp,
    windowFillRatio = gp / window_size,
    toiTotal = total_toi,
    toiPerGame = dplyr::if_else(gp > 0, total_toi / gp, 0),
    toi_ev = tidyr::replace_na(raw[["mP_ev"]], 0),
    toi_pp = tidyr::replace_na(raw[["mP_pp"]], 0),
    toi_sh = tidyr::replace_na(raw[["mP_sh"]], 0),
      toiShare_ev = safe_share(raw[["mP_ev"]], total_toi),
      toiShare_pp = safe_share(raw[["mP_pp"]], total_toi),
      toiShare_sh = safe_share(raw[["mP_sh"]], total_toi),
      iTwPer60 = safe_rate(total_takeaways, total_toi),
      iGwPer60 = safe_rate(total_giveaways, total_toi),
      iMdPer60 = safe_rate(total_penalties_drawn, total_toi),
      iMcPer60 = safe_rate(total_penalties_committed, total_toi),
      iHgPer60 = safe_rate(total_hits_given, total_toi),
      iHtPer60 = safe_rate(total_hits_taken, total_toi),
      faceoffPct = safe_pct(total_faceoff_wins, total_faceoff_losses)
    )

  ev_toi <- tidyr::replace_na(raw[["mP_ev"]], 0)
  pp_toi <- tidyr::replace_na(raw[["mP_pp"]], 0)
  sh_toi <- tidyr::replace_na(raw[["mP_sh"]], 0)

  metrics <- c(
    metrics,
    list(
      iCF_evPer60 = safe_rate(raw[["iCF_ev"]], ev_toi),
      iSF_evPer60 = safe_rate(raw[["iSF_ev"]], ev_toi),
      iGF_evPer60 = safe_rate(raw[["iGF_ev"]], ev_toi),
      iAPF_evPer60 = safe_rate(raw[["iAPF_ev"]], ev_toi),
      iASF_evPer60 = safe_rate(raw[["iASF_ev"]], ev_toi),
      ixGF_evPer60 = safe_rate(raw[["ixGF_ev"]], ev_toi),
      oCF_evPer60 = safe_rate(raw[["oCF_ev"]], ev_toi),
      oCA_evPer60 = safe_rate(raw[["oCA_ev"]], ev_toi),
      oSF_evPer60 = safe_rate(raw[["oSF_ev"]], ev_toi),
      oSA_evPer60 = safe_rate(raw[["oSA_ev"]], ev_toi),
      oGF_evPer60 = safe_rate(raw[["oGF_ev"]], ev_toi),
      oGA_evPer60 = safe_rate(raw[["oGA_ev"]], ev_toi),
      oxGF_evPer60 = safe_rate(raw[["oxGF_ev"]], ev_toi),
      oxGA_evPer60 = safe_rate(raw[["oxGA_ev"]], ev_toi),
      cfPct_ev = safe_pct(raw[["oCF_ev"]], raw[["oCA_ev"]]),
      sfPct_ev = safe_pct(raw[["oSF_ev"]], raw[["oSA_ev"]]),
      gfPct_ev = safe_pct(raw[["oGF_ev"]], raw[["oGA_ev"]]),
      xGPct_ev = safe_pct(raw[["oxGF_ev"]], raw[["oxGA_ev"]]),
      iCFShare_ev = safe_share(raw[["iCF_ev"]], raw[["oCF_ev"]]),
      iSFShare_ev = safe_share(raw[["iSF_ev"]], raw[["oSF_ev"]]),
      iGFShare_ev = safe_share(raw[["iGF_ev"]], raw[["oGF_ev"]]),
      ixGFShare_ev = safe_share(raw[["ixGF_ev"]], raw[["oxGF_ev"]]),
      finishingAbovexG_evPer60 = safe_rate(raw[["iGF_ev"]], ev_toi) - safe_rate(raw[["ixGF_ev"]], ev_toi),
      onIceGoalsAbovexG_evPer60 = (
        safe_rate(raw[["oGF_ev"]], ev_toi) - safe_rate(raw[["oGA_ev"]], ev_toi)
      ) - (
        safe_rate(raw[["oxGF_ev"]], ev_toi) - safe_rate(raw[["oxGA_ev"]], ev_toi)
      ),
      iCF_ppPer60 = safe_rate(raw[["iCF_pp"]], pp_toi),
      iSF_ppPer60 = safe_rate(raw[["iSF_pp"]], pp_toi),
      iGF_ppPer60 = safe_rate(raw[["iGF_pp"]], pp_toi),
      iAPF_ppPer60 = safe_rate(raw[["iAPF_pp"]], pp_toi),
      iASF_ppPer60 = safe_rate(raw[["iASF_pp"]], pp_toi),
      ixGF_ppPer60 = safe_rate(raw[["ixGF_pp"]], pp_toi),
      oGF_ppPer60 = safe_rate(raw[["oGF_pp"]], pp_toi),
      oxGF_ppPer60 = safe_rate(raw[["oxGF_pp"]], pp_toi),
      oCA_shPer60 = safe_rate(raw[["oCA_sh"]], sh_toi),
      oSA_shPer60 = safe_rate(raw[["oSA_sh"]], sh_toi),
      oGA_shPer60 = safe_rate(raw[["oGA_sh"]], sh_toi),
      oxGA_shPer60 = safe_rate(raw[["oxGA_sh"]], sh_toi),
      iTw_shPer60 = safe_rate(raw[["iTW_sh"]], sh_toi),
      iGw_shPer60 = safe_rate(raw[["iGW_sh"]], sh_toi),
      iMd_shPer60 = safe_rate(raw[["iMD_sh"]], sh_toi),
      iMc_shPer60 = safe_rate(raw[["iMC_sh"]], sh_toi)
    )
  )

  metrics
}

prefix_metrics <- function(metrics, prefix) {
  names(metrics) <- vapply(
    names(metrics),
    function(metric_name) prefix_metric_name(prefix, metric_name),
    character(1)
  )
  metrics
}

build_history_metrics <- function(games, signing_date) {
  gp_total <- nrow(games)
  regular_games <- games %>%
    dplyr::filter(gameType == "regular")
  playoff_games <- games %>%
    dplyr::filter(gameType == "playoff")

  if (gp_total > 0) {
    last_game_date <- games$gameDate[[nrow(games)]]
    last_game_type <- games$gameType[[nrow(games)]]
  } else {
    last_game_date <- as.Date(NA)
    last_game_type <- NA_character_
  }

  if (nrow(regular_games) > 0) {
    last_regular_game_date <- regular_games$gameDate[[nrow(regular_games)]]
  } else {
    last_regular_game_date <- as.Date(NA)
  }

  if (nrow(playoff_games) > 0) {
    last_playoff_game_date <- playoff_games$gameDate[[nrow(playoff_games)]]
  } else {
    last_playoff_game_date <- as.Date(NA)
  }

  list(
    careerTotalGpPreSigning = gp_total,
    careerRegularGpPreSigning = nrow(regular_games),
    careerPlayoffGpPreSigning = nrow(playoff_games),
    careerPlayoffGpSharePreSigning = safe_share(nrow(playoff_games), gp_total),
    careerTotalToiPreSigning = safe_sum(games$totalToi),
    careerRegularToiPreSigning = safe_sum(regular_games$totalToi),
    careerPlayoffToiPreSigning = safe_sum(playoff_games$totalToi),
    careerPlayoffToiSharePreSigning = safe_share(safe_sum(playoff_games$totalToi), safe_sum(games$totalToi)),
    daysSinceLastGame = as.numeric(signing_date - last_game_date),
    daysSinceLastRegularGame = as.numeric(signing_date - last_regular_game_date),
    daysSinceLastPlayoffGame = as.numeric(signing_date - last_playoff_game_date),
    lastGameWasPlayoff = dplyr::case_when(
      is.na(last_game_type) ~ NA,
      last_game_type == "playoff" ~ TRUE,
      TRUE ~ FALSE
    )
  )
}

build_trend_metrics <- function(regular_games) {
  recent20_games <- slice_recent_window(regular_games, 20L)
  prior20_games <- slice_prior_window(regular_games, skip_games = 20L, take_games = 20L)
  recent40_games <- slice_recent_window(regular_games, 40L)
  prior42_games <- slice_prior_window(regular_games, skip_games = 40L, take_games = 42L)

  recent20_metrics <- build_window_metrics(recent20_games, 20L)
  prior20_metrics <- build_window_metrics(prior20_games, 20L)
  recent40_metrics <- build_window_metrics(recent40_games, 40L)
  prior42_metrics <- build_window_metrics(prior42_games, 42L)

  trend_metrics <- list(
    regTrend20v20RecentGp = recent20_metrics[["gp"]],
    regTrend20v20PriorGp = prior20_metrics[["gp"]],
    regTrend40v42RecentGp = recent40_metrics[["gp"]],
    regTrend40v42PriorGp = prior42_metrics[["gp"]]
  )

  for (metric_name in TREND_METRICS) {
    trend_metrics[[prefix_metric_name("regTrend20v20", metric_name)]] <- recent20_metrics[[metric_name]] - prior20_metrics[[metric_name]]
    trend_metrics[[prefix_metric_name("regTrend40v42", metric_name)]] <- recent40_metrics[[metric_name]] - prior42_metrics[[metric_name]]
  }

  trend_metrics
}

build_contract_predictors <- function(player_games, signing_date) {
  pre_sign_games <- player_games[player_games$gameDate < signing_date, , drop = FALSE]
  regular_games <- pre_sign_games[pre_sign_games$gameType == "regular", , drop = FALSE]
  playoff_games <- pre_sign_games[pre_sign_games$gameType == "playoff", , drop = FALSE]

  predictor_values <- build_history_metrics(pre_sign_games, signing_date)

  for (window_name in names(REGULAR_WINDOW_SIZES)) {
    window_games <- slice_recent_window(regular_games, REGULAR_WINDOW_SIZES[[window_name]])
    predictor_values <- c(
      predictor_values,
      prefix_metrics(build_window_metrics(window_games, REGULAR_WINDOW_SIZES[[window_name]]), window_name)
    )
  }

  for (window_name in names(PLAYOFF_WINDOW_SIZES)) {
    window_games <- slice_recent_window(playoff_games, PLAYOFF_WINDOW_SIZES[[window_name]])
    predictor_values <- c(
      predictor_values,
      prefix_metrics(build_window_metrics(window_games, PLAYOFF_WINDOW_SIZES[[window_name]]), window_name)
    )
  }

  c(predictor_values, build_trend_metrics(regular_games))
}

add_predictors <- function(contracts_tbl, games_by_player, predictor_cols) {
  predictor_rows <- purrr::pmap(
    list(contracts_tbl$playerId, contracts_tbl$dateOfSigning),
    function(player_id, signing_date) {
      player_games <- games_by_player[[as.character(player_id)]]

      if (is.null(player_games)) {
        player_games <- games_by_player[["__empty__"]]
      }

      predictor_values <- build_contract_predictors(player_games, signing_date)
      tibble::as_tibble(predictor_values)
    }
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::select(dplyr::all_of(predictor_cols))

  dplyr::bind_cols(
    contracts_tbl %>% dplyr::select(dplyr::all_of(CONTRACT_BASE_COLS)),
    predictor_rows
  )
}

games <- load_skater_games()
games_by_player <- split(games, games$playerId)
games_by_player[["__empty__"]] <- games[0, , drop = FALSE]

prototype_predictors <- build_contract_predictors(
  player_games = games_by_player[["__empty__"]],
  signing_date = AGE_REF_DATE
)
PREDICTOR_COLS <- names(prototype_predictors)
OUTPUT_COLS <- c(CONTRACT_BASE_COLS, PREDICTOR_COLS)

contracts_all <- readr::read_csv(INPUT_PATH, show_col_types = FALSE) %>%
  dplyr::mutate(
    dateOfSigning = as.Date(dateOfSigning),
    birthDate = as.Date(birthDate)
  )

contracts <- contracts_all %>%
  dplyr::filter(contractNumber != 1L)

cap_lookup <- contracts %>%
  dplyr::distinct(startSeasonId, cap) %>%
  dplyr::filter(!is.na(startSeasonId), !is.na(cap))

future_cap <- cap_lookup %>%
  dplyr::filter(startSeasonId == VALIDATE_SEASON_ID) %>%
  dplyr::pull(cap) %>%
  unique()

if (length(future_cap) == 0L) {
  future_cap <- 104000000
} else {
  future_cap <- future_cap[[1]]
}

contracts_validate <- contracts %>%
  dplyr::filter(startSeasonId == VALIDATE_SEASON_ID)

contracts_train <- contracts %>%
  dplyr::filter(startSeasonId != VALIDATE_SEASON_ID)

last_contracts <- contracts_all %>%
  dplyr::filter(
    isLast,
    startSeasonId + ((term - 1L) * 10001L) == END_SEASON_ID
  )

contracts_test <- last_contracts %>%
  dplyr::transmute(
    playerId,
    cap = future_cap,
    capPrev = cap,
    startSeasonId = VALIDATE_SEASON_ID,
    startSeasonIdPrev = startSeasonId,
    termPrev = term,
    aavPrev = aav,
    aavPercPrev = aavPerc,
    dateOfSigning = AGE_REF_DATE,
    signedWithTeamId = NA_integer_,
    contractNumber = contractNumber + 1L,
    isLast = TRUE,
    birthDate,
    ageAtSigning = age_on_date(birthDate, free_agency_start_date(VALIDATE_SEASON_ID)),
    height,
    weight,
    handCode,
    positionCode
  ) %>%
  tidyr::expand_grid(
    tibble::tibble(
      isResign = c(rep(TRUE, 8L), rep(FALSE, 7L)),
      term = c(1L:8L, 1L:7L)
    )
  ) %>%
  dplyr::mutate(
    aav = NA_real_,
    aavPerc = NA_real_
  )

contracts_train <- add_predictors(contracts_train, games_by_player, PREDICTOR_COLS) %>%
  dplyr::select(dplyr::all_of(OUTPUT_COLS))
contracts_validate <- add_predictors(contracts_validate, games_by_player, PREDICTOR_COLS) %>%
  dplyr::select(dplyr::all_of(OUTPUT_COLS))
contracts_test <- add_predictors(contracts_test, games_by_player, PREDICTOR_COLS) %>%
  dplyr::select(dplyr::all_of(OUTPUT_COLS))

readr::write_csv(contracts_train, TRAIN_PATH)
readr::write_csv(contracts_validate, VALIDATE_PATH)
readr::write_csv(contracts_test, TEST_PATH)

cat(sprintf("Wrote %s rows to %s\n", nrow(contracts_train), TRAIN_PATH))
cat(sprintf("Wrote %s rows to %s\n", nrow(contracts_validate), VALIDATE_PATH))
cat(sprintf("Wrote %s rows to %s\n", nrow(contracts_test), TEST_PATH))
cat(sprintf("Output columns: %s\n", ncol(contracts_train)))
