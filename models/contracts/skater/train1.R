# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Load data.
contracts   <- readr::read_csv('models/contracts/data/contracts.csv', show_col_types = FALSE)
caps <- readr::read_csv('models/contracts/data/caps.csv', show_col_types = FALSE)
bios <- dplyr::filter(nhlscraper::players(), playerId %in% contracts$playerId)
sss  <- nhlscraper::skater_season_stats() %>% 
  dplyr::filter(playerId %in% contracts$playerId & seasonId >= 20122013 & gameTypeId == 2)
ssa  <- readr::read_csv('models/contracts/data/skater_shot_analysis.csv', show_col_types = FALSE) %>% 
  dplyr::select(playerId, seasonId, dplyr::contains('_2_') & !dplyr::contains('all'))

# ----- Create Training Set ----- #

# Remove ELCs and convert raw AAV to percentages.
contracts <- contracts %>% 
  dplyr::filter(!isFirst & !is.na(prevAAV) & prevStartSeasonId >= 20132014) %>% 
  dplyr::left_join(
    caps %>% dplyr::transmute(startSeasonId = as.integer(season), capStart = cap),
    by = 'startSeasonId'
  ) %>%
  dplyr::left_join(
    caps %>% dplyr::transmute(prevStartSeasonId = as.integer(season), capPrev = cap),
    by = 'prevStartSeasonId'
  ) %>%
  dplyr::mutate(
    aavP     = aav / capStart,
    prevAAVP = prevAAV / capPrev
  ) %>%
  dplyr::select(-prevStartSeasonId, -prevAAV, -capStart, -capPrev)

# Add biographies.
contracts <- contracts %>%
  dplyr::left_join(
    bios %>% dplyr::select(playerId, positionCode, birthDate, height, weight, handCode)
    , by = 'playerId'
  ) %>% 
  dplyr::filter(positionCode != 'G')

# Add basic and advanced statistics.

