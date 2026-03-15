# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Get all teams.
NHL_TEAMS <- nhlscraper::teams()
readr::write_csv(NHL_TEAMS, 'data/teams.csv')
