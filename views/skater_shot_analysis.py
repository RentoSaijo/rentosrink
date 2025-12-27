# Import libraries.
import streamlit as st
from utils import load_biographies, load_skater_shot_analysis

# Hardcode options and decoders.
SEASONS    = ['20242025']
GAME_TYPES = {'Regular Season': 2, 'Stanley Cup Playoffs': 3}
CATEGORIES = {'Actual': '', 'Per 82': 'per82', 'Per 60': 'per60'}

# Create season displays.
SEASON_LABELS  = {f'{s[:4]}-{s[4:]}': s for s in SEASONS}
SEASON_OPTIONS = list(SEASON_LABELS.keys())

# Load biographies.
bio = load_biographies()

# Format layout.
c_player, c_season, c_game, c_cat = st.columns(4, gap = 'small', vertical_alignment = 'top')

# Create selection box for season (display label, store raw).
with c_season:
    season_label = st.selectbox('Season', SEASON_OPTIONS, index = 0)
season = SEASON_LABELS[season_label]  # '20242025'

# Get available players.
ssa           = load_skater_shot_analysis(season)
available_ids = set(ssa['playerId'].dropna().astype(int).unique())
bio_season    = bio[bio['playerId'].isin(available_ids)].sort_values('menuName')
name_to_id    = dict(zip(bio_season['menuName'], bio_season['playerId']))
player_names  = list(name_to_id.keys())

# Create selection box for player.
with c_player:
    player_name = st.selectbox(
        'Player',
        player_names,
        index       = 0 if player_names else None,
        placeholder = 'N/A' if not player_names else None,
    )
player_id = name_to_id.get(player_name)

# Create selection box for game type.
with c_game:
    game_type_label = st.selectbox('Game Type', list(GAME_TYPES.keys()), index = 0)
game_type = GAME_TYPES[game_type_label]

# Create selection box for category.
with c_cat:
    category_label = st.segmented_control('Category', options = list(CATEGORIES.keys()), default = 'Actual')
category_suffix = CATEGORIES[category_label]
