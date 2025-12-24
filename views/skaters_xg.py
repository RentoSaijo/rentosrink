# Load libraries.
import streamlit as st
import re
from utils import load_skaters_xg

# Configure page defaults.
st.set_page_config(layout = 'wide')

# Load data.
skaters_xG = load_skaters_xg()

# Prepare options.
player_ids = (
    skaters_xG['playerId']
    .astype(int)
    .sort_values()
    .tolist()
)
def season_label(s: str) -> str:
    return f'{s[:4]}-{s[6:]}'
season_matches = re.findall(r'_(\d{8})_(?:2|3)\b', ' '.join(skaters_xG.columns))
seasons = sorted(set(season_matches))

# Create selection menus.
c1, c2, c3, c4 = st.columns(4, gap = 'small')

with c1:
    player_id = st.selectbox('Player ID', options = player_ids, key = 'player_id')

with c2:
    season = st.selectbox(
        'Season',
        options     = seasons,
        format_func = season_label,
        index       = 0,
        key         = 'season',
    )

with c3:
    game_type_label = st.selectbox(
        'Game Type',
        options = ['Regular Season', 'Stanley Cup Playoffs'],
        index   = 0,
        key     = 'game_type_label',
    )
    game_type = 2 if game_type_label == 'Regular Season' else 3

with c4:
    category = st.segmented_control(
        'Category',
        options = ['Actual', 'Per 82', 'Per 60'],
        default = 'Actual',
        key     = 'category',
    )

# Identify suffixes.
cat_to_suffix = {'Actual': '', 'Per 82': 'per82', 'Per 60': 'per60'}
suffix_1 = f'{cat_to_suffix[category]}_{season}_{game_type}'
suffix_2 = f'_{season}_{game_type}'
