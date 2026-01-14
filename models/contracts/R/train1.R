# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Set constants.
START_SEASON = 20142015
END_SEASON   = 20252026

# Read from CSV.
contracts <- read_csv(
  'models/contracts/data/contracts.csv', 
  show_col_type = FALSE
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

# Get supplemental data.
bios <- players() %>% 
  select(
    playerId = id,
    fullName,
    position,
    height,
    weight,
    hand     = shootsCatches
  )

# Merge data.
contracts <- left_join(contracts, bios, by = 'playerId')
rm(bios)

# Split data.
skater_contracts <- contracts %>% 
  filter(position != 'G')
goalie_contracts <- contracts %>% 
  filter(position == 'G') %>% 
  select(-position)
rm(contracts)


