# Load library.
suppressMessages(library(tidyverse))

# Read from CSV.
skaters <- read_csv('models/xG/data/skaters.csv', show_col_types = FALSE)
goalies <- read_csv('models/xG/data/goalies.csv', show_col_types = FALSE)

# Scrape supplemental data.
skaters_summary_20242025_2 <- nhlscraper::skater_season_report(
  season    = 20242025,
  game_type = 2,
  category  = 'summary'
) %>% 
  select(
    playerId,
    playerFullName         = skaterFullName,
    iGF_20242025_2         = goals,
    gamesPlayed_20242025_2 = gamesPlayed
  )
skaters_toi_20242025_2 <- nhlscraper::skater_season_report(
  season    = 20242025,
  game_type = 2,
  category  = 'timeonice'
) %>% 
  mutate(timeOnIce = timeOnIce / 60) %>% 
  select(
    playerId,
    playerFullName       = skaterFullName,
    timeOnIce_20242025_2 = timeOnIce
  )
skaters_summary_20242025_3 <- nhlscraper::skater_season_report(
  season    = 20242025,
  game_type = 3,
  category  = 'summary'
) %>% 
  select(
    playerId,
    playerFullName         = skaterFullName,
    iGF_20242025_3         = goals,
    gamesPlayed_20242025_3 = gamesPlayed
  )
skaters_toi_20242025_3 <- nhlscraper::skater_season_report(
  season    = 20242025,
  game_type = 3,
  category  = 'timeonice'
) %>% 
  mutate(timeOnIce = timeOnIce / 60) %>% 
  select(
    playerId,
    playerFullName       = skaterFullName,
    timeOnIce_20242025_3 = timeOnIce
  )
goalies_summary_20242025_2 <- nhlscraper::goalie_season_report(
  season    = 20242025,
  game_type = 2,
  category  = 'summary'
) %>% 
  mutate(timeOnIce = timeOnIce / 60) %>% 
  select(
    playerId,
    playerFullName         = goalieFullName,
    gamesPlayed_20242025_2 = gamesPlayed,
    timeOnIce_20242025_2   = timeOnIce,
    iGA_20242025_2         = goalsAgainst
  )
goalies_summary_20242025_3 <- nhlscraper::goalie_season_report(
  season    = 20242025,
  game_type = 3,
  category  = 'summary'
) %>% 
  mutate(timeOnIce = timeOnIce / 60) %>% 
  select(
    playerId,
    playerFullName         = goalieFullName,
    gamesPlayed_20242025_3 = gamesPlayed,
    timeOnIce_20242025_3   = timeOnIce,
    iGA_20242025_3         = goalsAgainst
  )

# Merge skater data.frames.
skaters_xG <- list(
  skaters,
  skaters_summary_20242025_2,
  skaters_toi_20242025_2,
  skaters_summary_20242025_3,
  skaters_toi_20242025_3
) %>%
  reduce(full_join, by = 'playerId') %>%
  mutate(
    playerFullName = coalesce(
      !!!syms(grep('^playerFullName(\\.|$)', names(.), value = TRUE))
    )
  ) %>%
  select(-matches('^playerFullName\\.')) %>%
  filter(!is.na(playerFullName)) %>% 
  mutate(
    # 20242025_2 Pace
    iCorsiFper82_20242025_2 = 
      iCorsiF_20242025_2 / gamesPlayed_20242025_2 * 82,
    iCorsiFper60_20242025_2 = 
      iCorsiF_20242025_2 / timeOnIce_20242025_2 * 60,
    iFenwickFper82_20242025_2 = 
      iFenwickF_20242025_2 / gamesPlayed_20242025_2 * 82,
    iFenwickFper60_20242025_2 = 
      iFenwickF_20242025_2 / timeOnIce_20242025_2 * 60,
    iSOGFper82_20242025_2 = 
      iSOGF_20242025_2 / gamesPlayed_20242025_2 * 82,
    iSOGFper60_20242025_2 = 
      iSOGF_20242025_2 / timeOnIce_20242025_2 * 60,
    iGFper82_20242025_2 = 
      iGF_20242025_2 / gamesPlayed_20242025_2 * 82,
    iGFper60_20242025_2 = 
      iGF_20242025_2 / timeOnIce_20242025_2 * 60,
    ixGFper82_20242025_2 = 
      ixGF_20242025_2 / gamesPlayed_20242025_2 * 82,
    ixGFper60_20242025_2 = 
      ixGF_20242025_2 / timeOnIce_20242025_2 * 60,
    across(everything(), ~replace_na(.x, 0)),
    iGFaX_20242025_2 = iGF_20242025_2 - ixGF_20242025_2,
    iGFaXper82_20242025_2 = 
      iGFaX_20242025_2 / gamesPlayed_20242025_2 * 82,
    iGFaXper60_20242025_2 = 
      iGFaX_20242025_2 / timeOnIce_20242025_2 * 60,
    # 20242025_3 Pace
    iCorsiFper82_20242025_3 =
      iCorsiF_20242025_3 / gamesPlayed_20242025_3 * 82,
    iCorsiFper60_20242025_3 =
      iCorsiF_20242025_3 / timeOnIce_20242025_3 * 60,
    iFenwickFper82_20242025_3 =
      iFenwickF_20242025_3 / gamesPlayed_20242025_3 * 82,
    iFenwickFper60_20242025_3 =
      iFenwickF_20242025_3 / timeOnIce_20242025_3 * 60,
    iSOGFper82_20242025_3 =
      iSOGF_20242025_3 / gamesPlayed_20242025_3 * 82,
    iSOGFper60_20242025_3 =
      iSOGF_20242025_3 / timeOnIce_20242025_3 * 60,
    iGFper82_20242025_3 =
      iGF_20242025_3 / gamesPlayed_20242025_3 * 82,
    iGFper60_20242025_3 =
      iGF_20242025_3 / timeOnIce_20242025_3 * 60,
    ixGFper82_20242025_3 =
      ixGF_20242025_3 / gamesPlayed_20242025_3 * 82,
    ixGFper60_20242025_3 =
      ixGF_20242025_3 / timeOnIce_20242025_3 * 60,
    across(everything(), ~replace_na(.x, 0)),
    iGFaX_20242025_3 = iGF_20242025_3 - ixGF_20242025_3,
    iGFaXper82_20242025_3 =
      iGFaX_20242025_3 / gamesPlayed_20242025_3 * 82,
    iGFaXper60_20242025_3 =
      iGFaX_20242025_3 / timeOnIce_20242025_3 * 60,
    across(everything(), ~replace_na(.x, 0))
  ) %>% 
  select(
    playerId,
    playerFullName,
    # 20242025_2
    gamesPlayed_20242025_2,
    timeOnIce_20242025_2,
    iCorsiF_20242025_2,
    iCorsiFper82_20242025_2,
    iCorsiFper60_20242025_2,
    iFenwickF_20242025_2,
    iFenwickFper82_20242025_2,
    iFenwickFper60_20242025_2,
    iSOGF_20242025_2,
    iSOGFper82_20242025_2,
    iSOGFper60_20242025_2,
    iGF_20242025_2,
    iGFper82_20242025_2,
    iGFper60_20242025_2,
    ixGF_20242025_2,
    ixGFper82_20242025_2,
    ixGFper60_20242025_2,
    iGFaX_20242025_2,
    iGFaXper82_20242025_2,
    iGFaXper60_20242025_2,
    # 20242025_3
    gamesPlayed_20242025_3,
    timeOnIce_20242025_3,
    iCorsiF_20242025_3,
    iCorsiFper82_20242025_3,
    iCorsiFper60_20242025_3,
    iFenwickF_20242025_3,
    iFenwickFper82_20242025_3,
    iFenwickFper60_20242025_3,
    iSOGF_20242025_3,
    iSOGFper82_20242025_3,
    iSOGFper60_20242025_3,
    iGF_20242025_3,
    iGFper82_20242025_3,
    iGFper60_20242025_3,
    ixGF_20242025_3,
    ixGFper82_20242025_3,
    ixGFper60_20242025_3,
    iGFaX_20242025_3,
    iGFaXper82_20242025_3,
    iGFaXper60_20242025_3,
  )
rm(
  skaters,
  skaters_summary_20242025_2,
  skaters_summary_20242025_3,
  skaters_toi_20242025_2,
  skaters_toi_20242025_3
)

# Merge goalie data.frames.
goalies_xG <- list(
  goalies,
  goalies_summary_20242025_2,
  goalies_summary_20242025_3
) %>%
  reduce(full_join, by = 'playerId') %>%
  mutate(
    playerFullName = coalesce(
      !!!syms(grep('^playerFullName(\\.|$)', names(.), value = TRUE))
    )
  ) %>%
  select(-matches('^playerFullName\\.')) %>%
  filter(!is.na(playerFullName)) %>% 
  mutate(
    # 20242025_2 Pace
    iFenwickAper82_20242025_2 = 
      iFenwickA_20242025_2 / gamesPlayed_20242025_2 * 82,
    iFenwickAper60_20242025_2 = 
      iFenwickA_20242025_2 / timeOnIce_20242025_2 * 60,
    iSOGAper82_20242025_2 = 
      iSOGA_20242025_2 / gamesPlayed_20242025_2 * 82,
    iSOGAper60_20242025_2 = 
      iSOGA_20242025_2 / timeOnIce_20242025_2 * 60,
    iGAper82_20242025_2 = 
      iGA_20242025_2 / gamesPlayed_20242025_2 * 82,
    iGAper60_20242025_2 = 
      iGA_20242025_2 / timeOnIce_20242025_2 * 60,
    ixGAper82_20242025_2 = 
      ixGA_20242025_2 / gamesPlayed_20242025_2 * 82,
    ixGAper60_20242025_2 = 
      ixGA_20242025_2 / timeOnIce_20242025_2 * 60,
    across(everything(), ~replace_na(.x, 0)),
    iGSaX_20242025_2 = ixGA_20242025_2 - iGA_20242025_2,
    iGSaXper82_20242025_2 = 
      iGSaX_20242025_2 / gamesPlayed_20242025_2 * 82,
    iGSaXper60_20242025_2 = 
      iGSaX_20242025_2 / timeOnIce_20242025_2 * 60,
    # 20242025_3 Pace
    iFenwickAper82_20242025_3 =
      iFenwickA_20242025_3 / gamesPlayed_20242025_3 * 82,
    iFenwickAper60_20242025_3 =
      iFenwickA_20242025_3 / timeOnIce_20242025_3 * 60,
    iSOGAper82_20242025_3 =
      iSOGA_20242025_3 / gamesPlayed_20242025_3 * 82,
    iSOGAper60_20242025_3 =
      iSOGA_20242025_3 / timeOnIce_20242025_3 * 60,
    iGAper82_20242025_3 =
      iGA_20242025_3 / gamesPlayed_20242025_3 * 82,
    iGAper60_20242025_3 =
      iGA_20242025_3 / timeOnIce_20242025_3 * 60,
    ixGAper82_20242025_3 =
      ixGA_20242025_3 / gamesPlayed_20242025_3 * 82,
    ixGAper60_20242025_3 =
      ixGA_20242025_3 / timeOnIce_20242025_3 * 60,
    across(everything(), ~replace_na(.x, 0)),
    iGSaX_20242025_3 = ixGA_20242025_3 - iGA_20242025_3,
    iGSaXper82_20242025_3 =
      iGSaX_20242025_3 / gamesPlayed_20242025_3 * 82,
    iGSaXper60_20242025_3 =
      iGSaX_20242025_3 / timeOnIce_20242025_3 * 60,
    across(everything(), ~replace_na(.x, 0))
  ) %>% 
  select(
    playerId,
    playerFullName,
    # 20242025_2
    gamesPlayed_20242025_2,
    timeOnIce_20242025_2,
    iFenwickA_20242025_2,
    iFenwickAper82_20242025_2,
    iFenwickAper60_20242025_2,
    iSOGA_20242025_2,
    iSOGAper82_20242025_2,
    iSOGAper60_20242025_2,
    iGA_20242025_2,
    iGAper82_20242025_2,
    iGAper60_20242025_2,
    ixGA_20242025_2,
    ixGAper82_20242025_2,
    ixGAper60_20242025_2,
    iGSaX_20242025_2,
    iGSaXper82_20242025_2,
    iGSaXper60_20242025_2,
    # 20242025_3
    gamesPlayed_20242025_3,
    timeOnIce_20242025_3,
    iFenwickA_20242025_3,
    iFenwickAper82_20242025_3,
    iFenwickAper60_20242025_3,
    iSOGA_20242025_3,
    iSOGAper82_20242025_3,
    iSOGAper60_20242025_3,
    iGA_20242025_3,
    iGAper82_20242025_3,
    iGAper60_20242025_3,
    ixGA_20242025_3,
    ixGAper82_20242025_3,
    ixGAper60_20242025_3,
    iGSaX_20242025_3,
    iGSaXper82_20242025_3,
    iGSaXper60_20242025_3
  )
rm(
  goalies,
  goalies_summary_20242025_2,
  goalies_summary_20242025_3
)

# Write to CSV.
write_csv(skaters_xG, 'data/skaters_xG.csv')
write_csv(goalies_xG, 'data/goalies_xG.csv')
