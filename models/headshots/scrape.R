# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Define constant.
SEASON <- 20242025

# Get all teams.
teamTriCodes <- nhlscraper::standings(paste0(
  SEASON %% 1e4, '-01-01'
))$teamAbbrev.default

# Get all headshot URLs.
headshots <- c()
positionCodes    <- c('F', 'D', 'G')
for (teamTriCode in teamTriCodes) {
  for (positionCode in positionCodes) {
    headshots <- append(headshots, nhlscraper::roster(
      team     = teamTriCode, 
      season   = SEASON, 
      position = positionCode
    )$headshot)
  }
}

# Save all headshots.
for (headshot in headshots) {
  playerId <- sub('\\.png$', '', basename(headshot))
  dir.create('assets/headshots', recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path('assets/headshots', paste0(playerId, '.png'))
  download.file(headshot, destfile = out_path, mode = 'wb', quiet = TRUE)
}
