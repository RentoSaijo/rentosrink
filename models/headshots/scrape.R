# Load library.
suppressMessages(library(nhlscraper))

# Define constants.
START_SEASON <- 20112012
END_SEASON   <- 20252026

# Loop through seasons.
season_seq <- function(start_season, end_season) {
  start_year <- start_season %/% 1e4
  end_year   <- end_season   %/% 1e4
  years <- seq.int(start_year, end_year)
  years * 1e4 + (years + 1L)
}
SEASONS <- season_seq(START_SEASON, END_SEASON)
seen_out_paths <- new.env(parent = emptyenv())
for (SEASON in SEASONS) {
  teamTriCodes <- nhlscraper::standings(paste0(
    SEASON %% 1e4, '-01-01'
  ))$teamAbbrev.default

  # Collect headshot URLs for this season.
  headshots <- c()
  positionCodes <- c('F', 'D', 'G')
  for (teamTriCode in teamTriCodes) {
    for (positionCode in positionCodes) {
      headshots <- append(headshots, nhlscraper::roster(
        team     = teamTriCode,
        season   = SEASON,
        position = positionCode
      )$headshot)
    }
  }
  headshots <- unique(headshots[!is.na(headshots)])

  # Save headshots.
  for (headshot in headshots) {
    playerId <- sub('\\.png$', '', basename(headshot))
    out_path <- file.path('assets/headshots', paste0(playerId, '.png'))
    already_seen <- isTRUE(exists(out_path, envir = seen_out_paths, inherits = FALSE))
    if (file.exists(out_path) && !already_seen) {
      assign(out_path, TRUE, envir = seen_out_paths)
      next
    }
    download.file(headshot, destfile = out_path, mode = 'wb', quiet = TRUE)
    assign(out_path, TRUE, envir = seen_out_paths)
  }
}
