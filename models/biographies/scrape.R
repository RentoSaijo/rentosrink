# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(nhlscraper))

# Define constants.
START_SEASON <- 20242025
END_SEASON   <- 20242025

# Get season IDs.
start_year <- START_SEASON %/% 1e4
end_year   <- END_SEASON %/% 1e4
years      <- start_year : end_year
seasonIds  <- as.integer(paste0(years, years + 1))
rm(start_year, end_year, years, START_SEASON, END_SEASON)

# Get player IDs.
playerIds <- c()
gameTypeIds <- 2 : 3
for (seasonId in seasonIds) {
  for (gameTypeId in gameTypeIds) {
    playerIds <- append(playerIds, nhlscraper::skater_season_report(
      season = seasonId, game_type = gameTypeId, category = 'bios'
    )$playerId)
    playerIds <- append(playerIds, nhlscraper::goalie_season_report(
      season = seasonId, game_type = gameTypeId, category = 'bios'
    )$playerId)
  }
}
playerIds <- unique(playerIds)
rm(gameTypeId, gameTypeIds, seasonId, seasonIds)

# Get biographies.
biographies <- nhlscraper::players() %>% 
  filter(id %in% playerIds) %>% 
  mutate(
    playerId = id,
    hand     = shootsCatches,
    position = case_when(
      centralRegistryPosition == 'C' ~ 'CR',
      centralRegistryPosition == 'L' ~ 'LW',
      centralRegistryPosition == 'R' ~ 'RW',
      centralRegistryPosition == 'D' ~ 'DF',
      centralRegistryPosition == 'G' ~ 'GT',
      TRUE                           ~ NA_character_
    ),
    number   = sweaterNumber,
    teamId   = lastNHLTeamId
  ) %>% 
  group_by(fullName) %>%
  arrange(playerId, .by_group = TRUE) %>%
  mutate(menuName = if (n() == 1) fullName else str_c(
    fullName, ' ', row_number()
  )) %>%
  ungroup() %>% 
  select(
    playerId,
    fullName,
    menuName,
    nationality,
    birthDate,
    height,
    weight,
    hand,
    position,
    number,
    teamId
  )
rm(playerIds)

# Write to CSV.
write_csv(biographies, 'data/biographies.csv')
