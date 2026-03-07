# ----- Setup ----- #

# Load libraries.
suppressMessages(library(glue))
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Define constant.
SEASONS <- c(20212022, 20232024, 20252026)

# ----- Helpers ----- #

read_lgbm_xg <- function(season, version) {
  readr::read_csv(glue::glue('data/shots_{season}_v{version}.csv'), show_col_types = FALSE) %>%
    dplyr::transmute(
      gameId,
      eventId,
      !!rlang::sym(glue::glue('xG_v{version}_lightgbm')) := xG
    )
}

calc_metrics <- function(df, season_label) {
  xg_cols <- setdiff(
    names(df),
    c('gameId', 'eventId', 'situationCode', 'typeDescKey')
  )
  df_long <- df %>%
    dplyr::mutate(y = dplyr::if_else(typeDescKey == 'goal', 1, 0)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(xg_cols),
      names_to = 'model',
      values_to = 'p'
    ) %>%
    dplyr::filter(!is.na(p)) %>%
    dplyr::mutate(p = pmin(pmax(p, 1e-15), 1 - 1e-15)) # avoid log(0)
  summarise_scores <- function(d) {
    d %>%
      dplyr::summarise(
        season = season_label,
        goals = sum(y),
        n = dplyr::n(),
        total_xg = sum(p),
        calibration = goals / total_xg,
        brier = mean((p - y)^2),
        log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
        .by = 'model'
      )
  }
  all_situations <- summarise_scores(df_long) %>%
    dplyr::mutate(scope = 'all')
  five_on_five <- summarise_scores(df_long %>% dplyr::filter(situationCode == '1551')) %>%
    dplyr::mutate(scope = '5v5')
  dplyr::bind_rows(all_situations, five_on_five) %>%
    dplyr::select(season, scope, model, n, goals, total_xg, calibration, brier, log_loss)
}

# ----- Comparison ----- #

# Load data.
for (season in SEASONS) {
  v1_lightgbm <- read_lgbm_xg(season, 1)
  v2_lightgbm <- read_lgbm_xg(season, 2)
  v3_lightgbm <- read_lgbm_xg(season, 3)
  v4_lightgbm <- read_lgbm_xg(season, 4)
  shots <- nhlscraper::gc_pbps(season) %>%
    dplyr::filter(!(period == 5 & gameTypeId == 5)) %>% 
    nhlscraper::calculate_xG(model = 1) %>% dplyr::mutate(xG_v1_logistic = xG) %>%
    nhlscraper::calculate_xG(model = 2) %>% dplyr::mutate(xG_v2_logistic = xG) %>%
    nhlscraper::calculate_xG(model = 3) %>% dplyr::mutate(xG_v3_logistic = xG) %>%
    nhlscraper::calculate_xG(model = 4) %>% dplyr::mutate(xG_v4_logistic = xG)
  shots_season <- shots %>%
    dplyr::left_join(v1_lightgbm, by = c('gameId', 'eventId')) %>%
    dplyr::left_join(v2_lightgbm, by = c('gameId', 'eventId')) %>%
    dplyr::left_join(v3_lightgbm, by = c('gameId', 'eventId')) %>%
    dplyr::left_join(v4_lightgbm, by = c('gameId', 'eventId')) %>%
    dplyr::filter(typeDescKey %in% c('goal', 'shot-on-goal', 'missed-shot')) %>%
    dplyr::select(gameId, eventId, situationCode, typeDescKey, xG_v1_logistic, xG_v2_logistic,  xG_v3_logistic,  xG_v4_logistic, xG_v1_lightgbm, xG_v2_lightgbm, xG_v3_lightgbm, xG_v4_lightgbm)
  assign(glue::glue('shots_{season}'), shots_season, envir = globalenv())
}
rm(shots, shots_season, v1_lightgbm, v2_lightgbm, v3_lightgbm, v4_lightgbm, season, SEASONS, read_lgbm_xg)

# Drop logistic regression v4 (needs fixing).
shots_20212022 <- dplyr::select(shots_20212022, -xG_v4_logistic)
shots_20232024 <- dplyr::select(shots_20232024, -xG_v4_logistic)
shots_20252026 <- dplyr::select(shots_20252026, -xG_v4_logistic)

# Calculate model comparison metrics.
model_metrics <- dplyr::bind_rows(
  calc_metrics(shots_20212022, '20212022'),
  calc_metrics(shots_20232024, '20232024'),
  calc_metrics(shots_20252026, '20252026')
)
