# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(readr))
suppressMessages(library(nhlscraper))

# Helper: normalize strings for matching.
normalize_str <- function(x) {
  x %>%
    stringi::stri_trans_general('Latin-ASCII') %>%
    str_to_lower() %>%
    str_squish()
}

# Helper: 'last name' = everything after the first word.
last_name <- function(x) {
  x %>%
    str_squish() %>%
    str_replace('^\\S+\\s+', '') %>%
    str_squish()
}

# Helper: first initial = first character of first word.
first_initial <- function(x) {
  x %>%
    str_squish() %>%
    str_extract('^\\S+') %>%
    str_sub(1, 1)
}

# Helper: last initial = first character of the last name.
last_initial <- function(x) {
  x %>%
    last_name() %>%
    str_sub(1, 1)
}

# Helper: map positions to F/D/G.
pos_fdg <- function(x) {
  case_when(
    substring(x, 1, 1) %in% c('C', 'L', 'R', 'F') ~ 'F',
    substring(x, 1, 1) == 'D'                     ~ 'D',
    TRUE                                          ~ 'G'
  )
}

# Helper: apply manual name aliases on already-normalized strings.
apply_aliases <- function(x) {
  case_when(
    x == 'jonathan audy-marchessault' ~ 'jonathan marchessault',
    TRUE                              ~ x
  )
}

# Get contracts.
contracts <- read_csv(
  'models/contracts/data/NHL_Contracts.csv',
  show_col_types = FALSE
) %>%
  mutate(across(
    c('Value', 'AAV', 'Signing Bonus', '2-Year Cash', '3-Year Cash'),
    \(x) parse_number(x)
  )) %>%
  select(
    fullName    = Player,
    position    = Pos,
    triCode     = `Team                     Signed With`,
    age         = `Age                     At Signing`,
    startSeason = Start,
    endSeason   = End,
    term        = Yrs,
    AAV,
    value       = Value,
    bonus       = `Signing Bonus`,
    SYC         = `2-Year Cash`,
    TYC         = `3-Year Cash`
  ) %>%
  mutate(
    across(contains('Season'), \(x) x * 1e4 + x + 1),
    position      = pos_fdg(position),
    triCode       = substring(triCode, 1, 3),
    fullName_key  = apply_aliases(normalize_str(fullName)),
    lastName_key  = normalize_str(last_name(fullName_key)),
    firstInit_key = normalize_str(first_initial(fullName_key)),
    lastInit_key  = normalize_str(last_initial(fullName_key)),
    initInit_key  = str_c(firstInit_key, lastInit_key, sep = ' '),
    initLast_key  = str_c(firstInit_key, lastName_key, sep = ' ')
  )

# Get players.
players <- nhlscraper::players() %>%
  filter(!is.na(ageSignelFa))

# Build players key table.
players_key <- bind_rows(
  players %>%
    transmute(
      fullName_key  = apply_aliases(normalize_str(fullName)),
      lastName_key  = normalize_str(last_name(fullName_key)),
      firstInit_key = normalize_str(first_initial(fullName_key)),
      lastInit_key  = normalize_str(last_initial(fullName_key)),
      initInit_key  = str_c(firstInit_key, lastInit_key, sep = ' '),
      initLast_key  = str_c(firstInit_key, lastName_key, sep = ' '),
      position      = pos_fdg(position),
      playerId      = id
    ),
  players %>%
    transmute(
      fullName_key  = apply_aliases(normalize_str(fullName)),
      lastName_key  = normalize_str(last_name(fullName_key)),
      firstInit_key = normalize_str(first_initial(fullName_key)),
      lastInit_key  = normalize_str(last_initial(fullName_key)),
      initInit_key  = str_c(firstInit_key, lastInit_key, sep = ' '),
      initLast_key  = str_c(firstInit_key, lastName_key, sep = ' '),
      position      = pos_fdg(centralRegistryPosition),
      playerId      = id
    )
) %>%
  filter(!is.na(position), position != '') %>%
  distinct(
    lastName_key,
    initInit_key, 
    initLast_key, 
    fullName_key, 
    position, 
    playerId
  )

# Step 1: lastName + position is unique -> use
players_lastpos_unique <- players_key %>%
  group_by(lastName_key, position) %>%
  filter(n_distinct(playerId) == 1) %>%
  ungroup() %>%
  distinct(lastName_key, position, playerId)

# Step 2: for the rest, firstInitial + lastInitial + position is unique -> use
players_initinitpos_unique <- players_key %>%
  anti_join(players_lastpos_unique, by = c('lastName_key', 'position')) %>%
  group_by(initInit_key, position) %>%
  filter(n_distinct(playerId) == 1) %>%
  ungroup() %>%
  distinct(initInit_key, position, playerId)

# Step 3: for the rest, firstInitial + lastName + position is unique -> use
players_initlastpos_unique <- players_key %>%
  anti_join(players_lastpos_unique, by = c('lastName_key', 'position')) %>%
  anti_join(players_initinitpos_unique, by = c('initInit_key', 'position')) %>%
  group_by(initLast_key, position) %>%
  filter(n_distinct(playerId) == 1) %>%
  ungroup() %>%
  distinct(initLast_key, position, playerId)

# Step 4: for the rest, fullName + position is unique -> use
players_fullpos_unique <- players_key %>%
  anti_join(players_lastpos_unique, by = c('lastName_key', 'position')) %>%
  anti_join(players_initinitpos_unique, by = c('initInit_key', 'position')) %>%
  anti_join(players_initlastpos_unique, by = c('initLast_key', 'position')) %>%
  group_by(fullName_key, position) %>%
  filter(n_distinct(playerId) == 1) %>%
  ungroup() %>%
  distinct(fullName_key, position, playerId)

# Apply merge tactics in order.
contracts <- contracts %>%
  left_join(players_lastpos_unique, by = c('lastName_key', 'position')) %>%
  left_join(
    players_initinitpos_unique, 
    by     = c('initInit_key', 'position'), 
    suffix = c('', '_initinitpos')
  ) %>%
  mutate(playerId = coalesce(playerId, playerId_initinitpos)) %>%
  select(-playerId_initinitpos) %>%
  left_join(
    players_initlastpos_unique, 
    by     = c('initLast_key', 'position'), 
    suffix = c('', '_initlastpos')
  ) %>%
  mutate(playerId = coalesce(playerId, playerId_initlastpos)) %>%
  select(-playerId_initlastpos) %>%
  left_join(
    players_fullpos_unique, 
    by     = c('fullName_key', 'position'), 
    suffix = c('', '_fullpos')) %>%
  mutate(playerId = coalesce(playerId, playerId_fullpos)) %>%
  select(-playerId_fullpos)
contracts <- contracts %>% 
  filter(!is.na(playerId)) %>% 
  select(
    -fullName, -fullName_key, -lastName_key, -firstInit_key, -lastInit_key, 
    -initInit_key, -initLast_key, -position
  )
rm(
  players, players_fullpos_unique, players_initinitpos_unique, 
  players_initlastpos_unique, players_key, players_lastpos_unique, 
  apply_aliases, first_initial, last_initial, last_name, normalize_str, pos_fdg
)

# Flag first and last contracts and note the previous contract's term and AAV.
contracts <- contracts %>%
  group_by(playerId) %>%
  arrange(startSeason, .by_group = TRUE) %>%
  mutate(
    isFirst  = startSeason == min(startSeason, na.rm = TRUE),
    isLast   = startSeason == max(startSeason, na.rm = TRUE),
    prevTerm = lag(term),
    prevAAV  = lag(AAV)
  ) %>%
  ungroup()

# Write to CSV.
write_csv(contracts, 'models/contracts/data/contracts.csv')

# Create data.frame of salary caps.
caps <- tibble::tibble(
  season       = c(
    '20142015',
    '20152016',
    '20162017',
    '20172018',
    '20182019',
    '20192020',
    '20202021',
    '20212022',
    '20222023',
    '20232024',
    '20242025',
    '20252026',
    '20262027',
    '20272028'
  ),
  cap_millions = c(
    69.0,
    71.4,
    73.0,
    75.0,
    79.5,
    81.5,
    81.5,
    81.5,
    82.5,
    83.5,
    88.0,
    95.5,
    104.0,
    113.5
  )
) %>%
  dplyr::mutate(cap = cap_millions * 1e6) %>%
  as.data.frame() %>%
  select(season, cap)

# Write to CSV.
write_csv(caps, 'models/contracts/data/caps.csv')
