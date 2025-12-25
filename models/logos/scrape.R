# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(rsvg))
suppressMessages(library(magick))
suppressMessages(library(nhlscraper))

# Define constant.
SEASON <- 20242025

# Get all logo URLs.
teamIds    <- nhlscraper::teams()$id
team_logos <- nhlscraper::team_logos() %>% 
  filter(
    background == 'dark' & 
      teamId %in% teamIds & 
      startSeason <= SEASON & 
      endSeason >= SEASON
  ) %>% 
  select(teamId, url = secureUrl)
rm(SEASON, teamIds)

# Get all logos.
pwalk(team_logos, function(teamId, url) {
  svg_path <- file.path('assets/logos', paste0(teamId, '.svg'))
  png_path <- file.path('assets/logos', paste0(teamId, '.png'))
  tmp_png  <- tempfile(fileext = '.png')
  download.file(url, destfile = svg_path, mode = 'wb', quiet = TRUE)
  rsvg_png(svg_path, file = tmp_png, width = 336)
  img <- image_read(tmp_png) |>
    image_scale('336x336') |>
    image_extent('336x336', gravity = 'center', color = 'none')
  image_write(img, path = png_path, format = 'png')
  file.remove(svg_path)
  unlink(tmp_png)
})
