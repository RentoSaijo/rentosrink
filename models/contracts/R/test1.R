# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(nhlscraper))

# Set constant.
SEASON = 20262027

# --- INPUTS --- #

# Read from CSV (all historical contracts).
contracts <- read_csv(
  'models/contracts/data/contracts.csv',
  show_col_types = FALSE
) %>%
  select(
    # IDs
    playerId,
    isFirst,
    isLast,
    # Predictors
    startSeason,
    age,
    prevTerm,
    prevAAV,
    # Responses
    term,
    AAV
  )

# Get bios.
bios <- players() %>%
  select(
    playerId = id,
    fullName,
    position,
    height,
    weight,
    hand     = shootsCatches
  )

# Merge bios.
contracts <- left_join(contracts, bios, by = 'playerId')
rm(bios)

# Keep a copy of ALL historical contracts (with bios) for later.
contracts_all <- contracts

# Pull cap for SEASON.
cap_SEASON <- read_csv('models/contracts/data/caps.csv', show_col_types = FALSE) %>%
  transmute(startSeason = season, cap) %>%
  filter(startSeason == SEASON) %>%
  pull(cap) %>%
  .[[1]]

# --- BUILD FREE AGENT PREDICTION BASE (1 ROW PER PLAYER) --- #

# Identify free agents whose most-recent deal ends in SEASON.
fa_base <- contracts_all %>%
  filter(isLast) %>%
  mutate(endSeason = startSeason + term * 10001) %>%
  filter(endSeason == SEASON) %>%
  group_by(playerId) %>%
  arrange(startSeason, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Keep ALL historical contracts for those free agents.
contracts_hist_fa <- contracts_all %>%
  filter(playerId %in% fa_base$playerId) %>%
  select(playerId, startSeason, age, term, AAV)

# Build the SEASON row to predict (prevTerm/prevAAV come from last contract in fa_base).
contracts <- fa_base %>%
  transmute(
    playerId,
    fullName,
    isFirst     = FALSE,
    isLast      = TRUE,
    startSeason = SEASON,
    cap         = cap_SEASON,
    prevTerm    = term,
    prevAAV     = AAV,
    age         = age + ((SEASON - startSeason) / 10001),
    position,
    height,
    weight,
    hand
  )

rm(cap_SEASON, contracts_all, fa_base)

# Split data.
skater_contracts <- contracts %>%
  filter(position != 'G')
goalie_contracts <- contracts %>%
  filter(position == 'G') %>%
  select(-position)
rm(contracts)

# --- SKATER FEATURES (ONLY 3 SEASONS PRIOR TO SEASON) --- #

# Get skater time-on-ice data.
skater_toi_reports <- purrr::map_dfr(
  c(SEASON - 10001, SEASON - 20002, SEASON - 30003),
  \(s) {
    nhlscraper::skater_season_report(
      season    = s,
      game_type = 2,
      category  = 'timeonice'
    ) %>%
      dplyr::select(
        playerId,
        seasonId,
        gamesPlayed,
        evTimeOnIcePerGame,
        ppTimeOnIcePerGame,
        shTimeOnIcePerGame
      ) %>%
      dplyr::mutate(
        eTOI_per82 = evTimeOnIcePerGame / 60 * 82,
        pTOI_per82 = ppTimeOnIcePerGame / 60 * 82,
        sTOI_per82 = shTimeOnIcePerGame / 60 * 82
      ) %>%
      dplyr::select(playerId, seasonId, gamesPlayed, eTOI_per82, pTOI_per82, sTOI_per82) %>%
      dplyr::mutate(
        gamesPlayed = dplyr::coalesce(gamesPlayed, 0),
        across(c(eTOI_per82, pTOI_per82, sTOI_per82), \(x) dplyr::coalesce(x, 0))
      )
  }
) %>%
  as.data.frame()

# Merge skater time-on-ice data.
skater_contracts <- skater_contracts %>%
  mutate(
    season_t1 = startSeason - 10001,
    season_t2 = startSeason - 20002,
    season_t3 = startSeason - 30003
  ) %>%
  left_join(
    skater_toi_reports %>%
      transmute(
        playerId,
        season_t1 = seasonId,
        gp_t1 = gamesPlayed,
        e_t1  = eTOI_per82,
        p_t1  = pTOI_per82,
        s_t1  = sTOI_per82
      ),
    by = c('playerId', 'season_t1')
  ) %>%
  left_join(
    skater_toi_reports %>%
      transmute(
        playerId,
        season_t2 = seasonId,
        gp_t2 = gamesPlayed,
        e_t2  = eTOI_per82,
        p_t2  = pTOI_per82,
        s_t2  = sTOI_per82
      ),
    by = c('playerId', 'season_t2')
  ) %>%
  left_join(
    skater_toi_reports %>%
      transmute(
        playerId,
        season_t3 = seasonId,
        gp_t3 = gamesPlayed,
        e_t3  = eTOI_per82,
        p_t3  = pTOI_per82,
        s_t3  = sTOI_per82
      ),
    by = c('playerId', 'season_t3')
  ) %>%
  mutate(
    gp_t1 = dplyr::coalesce(gp_t1, 0), gp_t2 = dplyr::coalesce(gp_t2, 0), gp_t3 = dplyr::coalesce(gp_t3, 0),
    e_t1  = dplyr::coalesce(e_t1, 0),  e_t2  = dplyr::coalesce(e_t2, 0),  e_t3  = dplyr::coalesce(e_t3, 0),
    p_t1  = dplyr::coalesce(p_t1, 0),  p_t2  = dplyr::coalesce(p_t2, 0),  p_t3  = dplyr::coalesce(p_t3, 0),
    s_t1  = dplyr::coalesce(s_t1, 0),  s_t2  = dplyr::coalesce(s_t2, 0),  s_t3  = dplyr::coalesce(s_t3, 0),

    GP_3yr_wavg =
      (3 * gp_t1 + 2 * gp_t2 + 1 * gp_t3) /
      (3 * (gp_t1 != 0) + 2 * (gp_t2 != 0) + 1 * (gp_t3 != 0)),

    eTOI_per82_3yr_wavg =
      (3 * e_t1 + 2 * e_t2 + 1 * e_t3) /
      (3 * (e_t1 != 0) + 2 * (e_t2 != 0) + 1 * (e_t3 != 0)),
    pTOI_per82_3yr_wavg =
      (3 * p_t1 + 2 * p_t2 + 1 * p_t3) /
      (3 * (p_t1 != 0) + 2 * (p_t2 != 0) + 1 * (p_t3 != 0)),
    sTOI_per82_3yr_wavg =
      (3 * s_t1 + 2 * s_t2 + 1 * s_t3) /
      (3 * (s_t1 != 0) + 2 * (s_t2 != 0) + 1 * (s_t3 != 0))
  ) %>%
  mutate(
    GP_3yr_wavg           = dplyr::coalesce(GP_3yr_wavg, 0),
    eTOI_per82_3yr_wavg   = dplyr::coalesce(eTOI_per82_3yr_wavg, 0),
    pTOI_per82_3yr_wavg   = dplyr::coalesce(pTOI_per82_3yr_wavg, 0),
    sTOI_per82_3yr_wavg   = dplyr::coalesce(sTOI_per82_3yr_wavg, 0)
  ) %>%
  select(
    -season_t1, -season_t2, -season_t3,
    -gp_t1, -gp_t2, -gp_t3,
    -e_t1,  -e_t2,  -e_t3,
    -p_t1,  -p_t2,  -p_t3,
    -s_t1,  -s_t2,  -s_t3
  )
rm(skater_toi_reports)

# Get skater shot analysis data.
skater_shot_reports <- purrr::map_dfr(
  c(SEASON - 10001, SEASON - 20002, SEASON - 30003),
  \(s) {
    readr::read_csv(
      glue::glue('data/skater_shot_analysis_{s}.csv'),
      show_col_types = FALSE
    ) %>%
      dplyr::transmute(
        playerId,
        seasonId    = s,
        ixGF_per82  = dplyr::coalesce(ixGF_2_per82, 0),
        oxGF_per82  = dplyr::coalesce(oxGF_2_per82, 0),
        oxGA_per82  = dplyr::coalesce(oxGA_2_per82, 0)
      )
  }
) %>%
  as.data.frame()

# Merge skater shot analysis data.
skater_contracts <- skater_contracts %>%
  mutate(
    season_t1 = startSeason - 10001,
    season_t2 = startSeason - 20002,
    season_t3 = startSeason - 30003
  ) %>%
  left_join(
    skater_shot_reports %>%
      transmute(
        playerId,
        season_t1 = seasonId,
        ix_t1   = ixGF_per82,
        oxGF_t1 = oxGF_per82,
        oxGA_t1 = oxGA_per82
      ),
    by = c('playerId', 'season_t1')
  ) %>%
  left_join(
    skater_shot_reports %>%
      transmute(
        playerId,
        season_t2 = seasonId,
        ix_t2   = ixGF_per82,
        oxGF_t2 = oxGF_per82,
        oxGA_t2 = oxGA_per82
      ),
    by = c('playerId', 'season_t2')
  ) %>%
  left_join(
    skater_shot_reports %>%
      transmute(
        playerId,
        season_t3 = seasonId,
        ix_t3   = ixGF_per82,
        oxGF_t3 = oxGF_per82,
        oxGA_t3 = oxGA_per82
      ),
    by = c('playerId', 'season_t3')
  ) %>%
  mutate(
    ix_t1 = dplyr::coalesce(ix_t1, 0), ix_t2 = dplyr::coalesce(ix_t2, 0), ix_t3 = dplyr::coalesce(ix_t3, 0),
    oxGF_t1 = dplyr::coalesce(oxGF_t1, 0), oxGF_t2 = dplyr::coalesce(oxGF_t2, 0), oxGF_t3 = dplyr::coalesce(oxGF_t3, 0),
    oxGA_t1 = dplyr::coalesce(oxGA_t1, 0), oxGA_t2 = dplyr::coalesce(oxGA_t2, 0), oxGA_t3 = dplyr::coalesce(oxGA_t3, 0),

    ixGF_per82_3yr_wavg =
      (3 * ix_t1 + 2 * ix_t2 + 1 * ix_t3) /
      (3 * (ix_t1 != 0) + 2 * (ix_t2 != 0) + 1 * (ix_t3 != 0)),
    oxGF_per82_3yr_wavg =
      (3 * oxGF_t1 + 2 * oxGF_t2 + 1 * oxGF_t3) /
      (3 * (oxGF_t1 != 0) + 2 * (oxGF_t2 != 0) + 1 * (oxGF_t3 != 0)),
    oxGA_per82_3yr_wavg =
      (3 * oxGA_t1 + 2 * oxGA_t2 + 1 * oxGA_t3) /
      (3 * (oxGA_t1 != 0) + 2 * (oxGA_t2 != 0) + 1 * (oxGA_t3 != 0))
  ) %>%
  mutate(
    ixGF_per82_3yr_wavg = dplyr::coalesce(ixGF_per82_3yr_wavg, 0),
    oxGF_per82_3yr_wavg = dplyr::coalesce(oxGF_per82_3yr_wavg, 0),
    oxGA_per82_3yr_wavg = dplyr::coalesce(oxGA_per82_3yr_wavg, 0)
  ) %>%
  select(
    -season_t1, -season_t2, -season_t3,
    -ix_t1, -ix_t2, -ix_t3,
    -oxGF_t1, -oxGF_t2, -oxGF_t3,
    -oxGA_t1, -oxGA_t2, -oxGA_t3
  )
rm(skater_shot_reports)

# --- PREDICT --- #

# Prepare columns once (both term models + AAV model use the same predictors here).
skater_contracts <- skater_contracts %>%
  mutate(
    position = factor(position),
    hand     = factor(hand)
  ) %>%
  select(
    # IDs
    playerId,
    fullName,
    isFirst,
    isLast,
    # Predictors
    startSeason,
    cap,
    prevTerm,
    prevAAV,
    age,
    position,
    height,
    weight,
    hand,
    GP_3yr_wavg,
    eTOI_per82_3yr_wavg,
    pTOI_per82_3yr_wavg,
    sTOI_per82_3yr_wavg,
    ixGF_per82_3yr_wavg,
    oxGF_per82_3yr_wavg,
    oxGA_per82_3yr_wavg
  )

# --- TERM (REGRESSION) FOR PROJECTED CONTRACT --- #
term_reg_model <- readRDS('models/contracts/skater_term_model1.rds')

skater_contracts <- skater_contracts %>%
  mutate(
    # IMPORTANT: do NOT clip the regression prediction itself.
    xTerm_reg_raw = predict(term_reg_model, new_data = ., type = 'numeric')$.pred,

    # Only create a 1..8 integer version for AAV lookup (grid is 1..8).
    xTerm_reg_int = pmin(pmax(round(xTerm_reg_raw), 1), 8)
  )

rm(term_reg_model)

# --- TERM PROBABILITIES (CLASSIFICATION) FOR POSSIBILITIES --- #
term_prob_model <- readRDS('models/contracts/skater_termProb_model1.rds')

term_probs <- predict(term_prob_model, new_data = skater_contracts, type = 'prob') %>%
  dplyr::as_tibble() %>%
  {
    nm <- names(.)
    nm <- gsub("^\\.pred_", "termProb_", nm)
    nm <- gsub("^pred_", "termProb_", nm)
    names(.) <- nm
    .
  }

# ensure termProb_1..termProb_8 exist (in case model returns only seen classes)
for (k in 1:8) {
  col <- paste0('termProb_', k)
  if (!col %in% names(term_probs)) term_probs[[col]] <- 0
}
term_probs <- term_probs %>%
  select(termProb_1, termProb_2, termProb_3, termProb_4, termProb_5, termProb_6, termProb_7, termProb_8)

skater_contracts <- bind_cols(skater_contracts, term_probs) %>%
  mutate(
    xTerm_cls = max.col(
      dplyr::select(., dplyr::starts_with('termProb_')) %>% as.matrix(),
      ties.method = 'first'
    )
  )

rm(term_prob_model, term_probs)

# --- AAV (REGRESSION) USING TERM GRID --- #
AAV_model <- readRDS('models/contracts/skater_AAV_model1.rds')

# Build a term grid: duplicate each player row 8 times, set term = 1..8.
skater_contracts_termgrid <- skater_contracts %>%
  select(
    -xTerm_reg_raw,
    -xTerm_reg_int,
    -xTerm_cls,
    -dplyr::starts_with('termProb_')
  ) %>%
  tidyr::crossing(term = 1:8)

# Predict AAV on the grid.
skater_contracts_termgrid <- skater_contracts_termgrid %>%
  mutate(
    AAV = predict(AAV_model, new_data = ., type = 'numeric')$.pred
  )

# Pivot wide to get xAAV_1 ... xAAV_8.
skater_contracts_aav_wide <- skater_contracts_termgrid %>%
  transmute(playerId, term, AAV) %>%
  mutate(term = as.integer(term)) %>%
  tidyr::pivot_wider(
    names_from   = term,
    values_from  = AAV,
    names_prefix = 'xAAV_'
  )

# Attach xAAV_* columns back, and compute:
# - xAAV_reg using xTerm_reg_int (for projected contract)
# - xAAV_cls using xTerm_cls (for reference, if you want it later)
skater_contracts <- skater_contracts %>%
  left_join(skater_contracts_aav_wide, by = 'playerId') %>%
  mutate(
    xAAV_reg = dplyr::case_when(
      xTerm_reg_int == 1 ~ xAAV_1,
      xTerm_reg_int == 2 ~ xAAV_2,
      xTerm_reg_int == 3 ~ xAAV_3,
      xTerm_reg_int == 4 ~ xAAV_4,
      xTerm_reg_int == 5 ~ xAAV_5,
      xTerm_reg_int == 6 ~ xAAV_6,
      xTerm_reg_int == 7 ~ xAAV_7,
      TRUE               ~ xAAV_8
    ),
    xAAV_cls = dplyr::case_when(
      xTerm_cls == 1 ~ xAAV_1,
      xTerm_cls == 2 ~ xAAV_2,
      xTerm_cls == 3 ~ xAAV_3,
      xTerm_cls == 4 ~ xAAV_4,
      xTerm_cls == 5 ~ xAAV_5,
      xTerm_cls == 6 ~ xAAV_6,
      xTerm_cls == 7 ~ xAAV_7,
      TRUE           ~ xAAV_8
    )
  )

rm(AAV_model, skater_contracts_termgrid, skater_contracts_aav_wide)

# --- OUTPUTS --- #

# 1) Contract projection:
#    - term uses regression model output (raw, not clipped)
#    - AAV uses regression AAV grid lookup at xTerm_reg_int (1..8)
contracts_new <- skater_contracts %>%
  transmute(
    playerId,
    startSeason,
    age,
    term = xTerm_reg_raw,
    AAV  = xAAV_reg
  )

contract_projection <- bind_rows(
  contracts_hist_fa,
  contracts_new
) %>%
  arrange(playerId, startSeason) %>%
  as.data.frame()

write_csv(
  contract_projection,
  glue::glue('data/contract_projection_{SEASON}.csv')
)

# 2) Contract possibilities (AAV for each possible term 1..8 + classification term probabilities).
contract_possibility <- skater_contracts %>%
  select(
    playerId,
    xAAV_1, xAAV_2, xAAV_3, xAAV_4, xAAV_5, xAAV_6, xAAV_7, xAAV_8,
    termProb_1, termProb_2, termProb_3, termProb_4, termProb_5, termProb_6, termProb_7, termProb_8
  ) %>%
  arrange(playerId) %>%
  as.data.frame()

write_csv(
  contract_possibility,
  glue::glue('data/contract_possibility_{SEASON}.csv')
)

rm(contracts_hist_fa, contracts_new)
