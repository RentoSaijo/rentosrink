# Load libraries.
import streamlit as st
import plotly.graph_objects as go
import pandas as pd
import re
from pathlib import Path
from PIL import Image
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

row = skaters_xG.loc[skaters_xG['playerId'].astype(int) == int(player_id)].iloc[0]
player_name = row.get('playerFullName', f'Player {int(player_id)}')

gp_col  = f'gamesPlayed{suffix_2}'
toi_col = f'timeOnIce{suffix_2}'

gp  = row.get(gp_col, 0)
toi = row.get(toi_col, 0.0)

gp  = int(gp) if pd.notna(gp) else 0
toi = float(toi) if pd.notna(toi) else 0.0

headshots_dir = Path('assets/headshots')
headshot_path = headshots_dir / f'{int(player_id)}.png'
if not headshot_path.exists():
    headshot_path = headshots_dir / 'default.png'

# --- iGF (+ delta iGFaX) ---
igf_col   = f'iGF{suffix_1}'
igfax_col = f'iGFaX{suffix_1}'

igf   = row.get(igf_col, float('nan'))
igfax = row.get(igfax_col, float('nan'))

igf   = float(igf) if pd.notna(igf) else None
igfax = float(igfax) if pd.notna(igfax) else None

def fmt_stat(x):
    if x is None:
        return '—'
    # If you want integers for Actual, keep this; otherwise delete the if-block.
    if category == 'Actual':
        return f'{x:,.0f}'
    return f'{x:,.2f}'

def fmt_delta(x):
    if x is None:
        return None
    # show sign explicitly
    return f'{x:+.2f}' if category != 'Actual' else f'{x:+.1f}'

# 5 equal columns now
c1, c2, c3, c4, c5 = st.columns(5, vertical_alignment='top', gap='medium')

with c1:
    # Smaller, less “billboard” than subheader
    st.write(f'### {player_name}')
    # Placeholder bio line (position / handedness / height / weight, etc.)
    st.caption('F - L - 77 - 190 - #33')

with c2:
    st.image(Image.open(headshot_path), width=90)

with c3:
    st.metric('Games Played', f'{gp:,}')

with c4:
    st.metric('Minutes Played', f'{toi:,.1f}')

with c5:
    delta_txt = f'{igfax:+.2f} vs ixGF'
    st.metric('iGF', fmt_stat(igf), delta=delta_txt)

corsi   = float(row.get(f'iCorsiF{suffix_1}', 0))
fenwick = float(row.get(f'iFenwickF{suffix_1}', 0))
shots   = float(row.get(f'iSOGF{suffix_1}', 0))
goals   = float(row.get(f'iGF{suffix_1}', 0))

fenwick = min(fenwick, corsi)
shots   = min(shots, fenwick)
goals   = min(goals, shots)

def fmt(x: float) -> str:
    return f'{x:,.0f}' if category == 'Actual' else f'{x:,.2f}'

labels = [
    f'Corsi ({fmt(corsi)})',
    f'Fenwick ({fmt(fenwick)})',
    f'SOG ({fmt(shots)})',
    f'Goals ({fmt(goals)})',
]

# High-contrast colors that read well on dark backgrounds
node_colors = ['#4C9AFF', '#0052CC', '#FFAB00', '#FF5630']  # blue → blue → amber → red
link_colors = [
    'rgba(76,154,255,0.45)',
    'rgba(0,82,204,0.45)',
    'rgba(255,171,0,0.45)',
]

fig = go.Figure(go.Sankey(
    arrangement='snap',
    node=dict(
        label=labels,
        pad=20,
        thickness=22,
        color=node_colors,
        line=dict(color='rgba(255,255,255,0.35)', width=1),
    ),
    link=dict(
        source=[0, 1, 2],
        target=[1, 2, 3],
        value =[fenwick, shots, goals],
        color=link_colors,
    )
))

fig.update_layout(
    title=dict(
        text='Shot Funnel: Corsi → Fenwick → Shots → Goals',
        font=dict(size=22, color='white'),
        x=0.01,
        xanchor='left',
    ),
    font=dict(size=16, color='white'),   # affects node labels
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)',
    margin=dict(l=10, r=10, t=60, b=10),
)

stats = {
    'iCorsiF': f'iCorsiF{suffix_1}',
    'iFenwickF': f'iFenwickF{suffix_1}',
    'iSOGF': f'iSOGF{suffix_1}',
    'iGF': f'iGF{suffix_1}',
    'ixGF': f'ixGF{suffix_1}',
    'iGFaX': f'iGFaX{suffix_1}',
}

# Population for z-scores: everyone in the same season/game_type/category
# (You can later add min GP / min TOI filters here)
pop = skaters_xG.copy()
pop = pop[(pop[gp_col].fillna(0) > 0) & (pop[toi_col].fillna(0) > 0)]

z_vals = []
z_labels = []
for label, col in stats.items():
    if col not in pop.columns:
        continue

    mu = pop[col].mean()
    sd = pop[col].std(ddof=0)
    val = row.get(col, float('nan'))

    if pd.isna(val) or sd == 0 or pd.isna(sd):
        z = 0.0
    else:
        z = (float(val) - float(mu)) / float(sd)

    z_labels.append(label)
    z_vals.append(z)

# Bar colors: green-ish for positive, red-ish for negative (reads well on dark)
bar_colors = ['rgba(54,179,126,0.85)' if z >= 0 else 'rgba(255,86,48,0.85)' for z in z_vals]

zfig = go.Figure(go.Bar(
    x=z_vals,
    y=z_labels,
    orientation='h',
    marker=dict(color=bar_colors),
))

# Symmetric x-range around 0 for readability
m = max(1.0, max(abs(z) for z in z_vals) if z_vals else 1.0)
zfig.update_layout(
    title=dict(
        text='Z-scores vs League',
        font=dict(size=20, color='white'),
        x=0.01, xanchor='left',
    ),
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)',
    font=dict(color='white', size=14),
    margin=dict(l=10, r=10, t=60, b=10),
    xaxis=dict(
        range=[-m - 0.2, m + 0.2],
        zeroline=True,
        zerolinecolor='rgba(255,255,255,0.35)',
        gridcolor='rgba(255,255,255,0.08)',
        title='z',
    ),
    yaxis=dict(title=''),
)

# --- Put Sankey + Z-bars on the same row ---
left, right = st.columns([1.2, 1.0], gap='medium')
with left:
    st.plotly_chart(fig, use_container_width=True, config={'displayModeBar': False})
with right:
    st.plotly_chart(zfig, use_container_width=True, config={'displayModeBar': False})