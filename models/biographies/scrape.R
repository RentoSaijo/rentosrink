# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(nhlscraper))

# Define constants.
START_SEASON <- 20112012
END_SEASON   <- 20252026

# Get season IDs.
start_year <- START_SEASON %/% 1e4
end_year   <- END_SEASON %/% 1e4
years      <- start_year : end_year
seasonIds  <- as.integer(paste0(years, years + 1))
rm(start_year, end_year, years, START_SEASON, END_SEASON)

# Get player IDs.
playerIds <- c()
gameTypeIds <- 2:3
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
  dplyr::filter(playerId %in% playerIds) %>% 
  dplyr::mutate(
    handCode = shootsCatches,
    positionCode,
    number   = sweaterNumber,
    teamId   = lastNHLTeamId
  ) %>% 
  dplyr::group_by(fullName) %>%
  dplyr::arrange(playerId, .by_group = TRUE) %>%
  dplyr::mutate(menuName = if (n() == 1) fullName else stringr::str_c(
    fullName, ' ', dplyr::row_number()
  )) %>%
  dplyr::ungroup() %>% 
  dplyr::select(
    playerId,
    fullName,
    menuName,
    nationality,
    birthDate,
    height,
    weight,
    handCode,
    positionCode,
    number,
    teamId
  )
rm(playerIds)

# Write to CSV.
readr::write_csv(biographies, 'data/biographies.csv')
