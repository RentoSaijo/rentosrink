# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(nhlscraper))

# Set constant.
SEASON = 20262027

# Read from CSV.
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

# Pull cap for SEASON.
cap_SEASON <- read_csv('models/contracts/data/caps.csv', show_col_types = FALSE) %>%
  transmute(startSeason = season, cap) %>%
  filter(startSeason == SEASON) %>%
  pull(cap) %>%
  .[[1]]

# Keep only free agents.
contracts <- contracts %>%
  filter(isLast) %>% 
  mutate(endSeason = startSeason + term * 10001) %>%
  filter(endSeason == SEASON) %>%
  group_by(playerId) %>%
  arrange(startSeason, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    playerId,
    fullName,
    isFirst    = FALSE,
    isLast     = TRUE,
    startSeason= SEASON,
    cap        = cap_SEASON,
    prevTerm   = term,
    prevAAV    = AAV,
    age        = age + ((SEASON - startSeason) / 10001),
    position,
    height,
    weight,
    hand
  )
rm(cap_SEASON)

# Split data.
skater_contracts <- contracts %>% 
  filter(position != 'G')
goalie_contracts <- contracts %>% 
  filter(position == 'G') %>% 
  select(-position)
rm(contracts)

# Get skater time-on-ice data (only the 3 seasons prior to SEASON).
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
      dplyr::mutate(across(c(eTOI_per82, pTOI_per82, sTOI_per82), \(x) dplyr::coalesce(x, 0)))
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

# Get skater shot analysis data (only the 3 seasons prior to SEASON).
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

# Predict terms.
term_model <- readRDS('models/contracts/skater_term_model1.rds')
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
  ) %>%
  mutate(
    term = predict(term_model, new_data = ., type = 'numeric')$.pred
  )
rm(term_model)

# Predict AAVs.
AAV_model <- readRDS('models/contracts/skater_AAV_model1.rds')
skater_contracts <- skater_contracts %>% 
  mutate(
    AAV   = predict(AAV_model, new_data = ., type = 'numeric')$.pred
  )
rm(AAV_model)

# Attach back to contracts.
contracts <- read_csv(
  'models/contracts/data/contracts.csv', 
  show_col_types = FALSE
) %>% 
  select(
    playerId,
    startSeason,
    age,
    term,
    AAV
  ) %>% 
  filter(playerId %in% skater_contracts$playerId)
contracts <- dplyr::bind_rows(
  contracts,
  skater_contracts %>%
    transmute(
      playerId,
      startSeason,
      age,
      term,
      AAV
    )
) %>%
  arrange(playerId, startSeason) %>%
  group_by(playerId) %>%
  arrange(startSeason, .by_group = TRUE) %>%
  ungroup() %>%
  as.data.frame()

# Write to CSV.
write_csv(contracts, 'data/contract_projection_20262027.csv')
