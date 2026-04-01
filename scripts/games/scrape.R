# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Get all games.
START_SEASON <- 20102011
END_SEASON   <- 20252026
NHL_GAMES    <- nhlscraper::games() %>% 
  dplyr::filter(seasonId >= START_SEASON) %>% 
  dplyr::filter(seasonId <= END_SEASON) %>% 
  dplyr::filter(gameTypeId %in% 2:3) %>% 
  dplyr::filter(gameStateId == 7) %>%
  dplyr::arrange(gameId)
readr::write_csv(NHL_GAMES, 'data/games.csv')
