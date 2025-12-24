# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(nhlscraper))

# Get all teams.
teams_20242025 <- nhlscraper::standings('2025-01-01')$teamAbbrev.default
positions      <- c('F', 'D', 'G')

# Get all headshot URLs.
headshots <- c()
for (teamTriCode in teams_20242025) {
  for (positionCode in positions) {
    headshots <- append(headshots, nhlscraper::roster(
      team     = teamTriCode, 
      season   = 20242025, 
      position = positionCode
    )$headshot)
  }
}

# Save all headshots.
for (headshot in headshots) {
  playerId <- sub('\\.png$', '', basename(headshot))
  dir.create('assets/headshots', recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path('assets/headshots', paste0(playerId, '.png'))
  if (file.exists(out_path)) next
  download.file(headshot, destfile = out_path, mode = 'wb', quiet = TRUE)
}
