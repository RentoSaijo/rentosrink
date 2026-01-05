# Import libraries.
import streamlit as st
import pandas as pd
import plotly.graph_objects as go
from utils import load_biographies, load_skater_shot_analysis

# Hardcode options and decoders.
SEASONS    = ['20252026', '20242025']
GAME_TYPES = {'Regular Season': 2, 'Stanley Cup Playoffs': 3}
CATEGORIES = {'Actual': '', 'Per 82': '_per82', 'Per 60': '_per60'}

# Create season displays.
SEASON_LABELS  = {f'{s[:4]}-{s[4:]}': s for s in SEASONS}
SEASON_OPTIONS = list(SEASON_LABELS.keys())

# Load biographies.
bio = load_biographies()

# Format layout.
c_player, c_season, c_game, c_cat = st.columns(4, gap = 'small', vertical_alignment = 'top')

# Create selection box for season (display label, store raw).
with c_season:
    season_label = st.selectbox('Season', SEASON_OPTIONS, index = 0, key = 'season_label')
season = SEASON_LABELS[season_label]

# Get available players.
ssa           = load_skater_shot_analysis(season)
available_ids = set(ssa['playerId'].dropna().astype(int).unique())
bio_season    = bio[bio['playerId'].isin(available_ids)].sort_values('menuName')
name_to_id    = dict(zip(bio_season['menuName'], bio_season['playerId']))
player_names  = list(name_to_id.keys())

# Create selection box for player.
if 'player_name' not in st.session_state:
    st.session_state['player_name'] = None
if st.session_state['player_name'] is None and player_names:
    st.session_state['player_name'] = player_names[0]
player_index = 0
if st.session_state['player_name'] in player_names:
    player_index = player_names.index(st.session_state['player_name'])
with c_player:
    player_name = st.selectbox(
        'Player',
        player_names,
        index       = player_index if player_names else None,
        placeholder = 'N/A' if not player_names else None,
        key         = 'player_name',
    )
player_id = name_to_id.get(player_name)

# Create selection box for game type.
with c_game:
    game_type_label = st.selectbox('Game Type', list(GAME_TYPES.keys()), index = 0)
game_type = GAME_TYPES[game_type_label]

# Create selection box for category.
with c_cat:
    category_label = st.segmented_control(
        'Category',
        options = list(CATEGORIES.keys()),
        default = 'Actual',
        key     = 'category_label',
    )
category_label  = category_label or 'Actual'
category_suffix = CATEGORIES[category_label]

# Pull selected row once (used for metric + plots).
r = None
if player_id is not None:
    m = (ssa['playerId'] == player_id)
    if m.any():
        r = ssa.loc[m].iloc[0]

def _fmt(v, is_rate):
    if v is None or pd.isna(v):
        return None
    return f'{float(v):.2f}' if is_rate else f'{int(round(float(v)))}'

# ----- TOP ROW: HEADSHOT + BIO + SUMMARY ----- #
c_hs, c_bio, c_summary = st.columns([1, 1.25, 3.75], gap = 'medium', vertical_alignment = 'center')

# Define helpers.
def _fmt_height(inches):
    if inches is None or pd.isna(inches):
        return None
    inches = int(inches)
    return f"{inches//12}'{inches%12}\""
def _safe_int(x):
    return None if x is None or pd.isna(x) else int(x)
def _img(path, fallback=None):
    import os
    if path and os.path.exists(path):
        st.image(path, width='content')
    elif fallback and os.path.exists(fallback):
        st.image(fallback, width='content')

# --- HEADSHOT --- #
with c_hs:
    if player_id is None:
        st.empty()
    else:
        headshot_path = f'assets/headshots/{int(player_id)}.png'
        _img(headshot_path, fallback='assets/headshots/default.png')

# --- BIO --- #
with c_bio:
    if player_id is None:
        st.info('Select a player.')
    else:
        b = bio.loc[bio['playerId'] == player_id].iloc[0]

        full_name = (b.get('fullName', player_name) or '').strip()
        parts     = full_name.split()
        last_name = parts[-1] if parts else full_name

        pos   = (b.get('position') or '').strip() or None
        num   = _safe_int(b.get('number'))
        hand  = (b.get('hand') or '').strip() or None
        nat   = (b.get('nationality') or '').strip() or None
        birth = (b.get('birthDate') or '').strip() or None
        wt    = _safe_int(b.get('weight'))

        # Title: last name only.
        st.markdown(f"### {last_name}")

        # Line 1: LW・Shoots L・#10
        c1 = "・".join([x for x in [
            pos,
            f"Shoots {hand}" if hand else None,
            f"#{num}" if num is not None else None,
        ] if x])

        # Line 2: CAN・1996-12-14
        c2 = "・".join([x for x in [
            nat,
            birth if birth else None,
        ] if x])

        # Line 3: 75 in・209 lb
        c3 = "・".join([x for x in [
            f"{int(b.get('height'))} in" if not pd.isna(b.get('height')) else None,
            f"{wt} lb" if wt is not None else None,
        ] if x])

        if c1:
            st.caption(c1)
        if c2:
            st.caption(c2)
        if c3:
            st.caption(c3)

# --- SUMMARY (METRICS) --- #
with c_summary:
    if r is None:
        st.metric('iGF', value='N/A')
        st.metric('ixGF', value='N/A')
    else:
        goal_col = f'iGF_{game_type}{category_suffix}'
        ixg_col  = f'ixGF_{game_type}{category_suffix}'

        goals = r.get(goal_col, float('nan'))
        ixg   = r.get(ixg_col, float('nan'))

        # Always show integers.
        goals_val = None if pd.isna(goals) else int(round(float(goals)))
        ixg_val   = None if pd.isna(ixg)   else int(round(float(ixg)))

        goals_str = 'N/A' if goals_val is None else f'{goals_val}'
        ixg_str   = 'N/A' if ixg_val is None else f'{ixg_val}'

        st.metric('iGF', value = goals_str)
        st.metric('ixGF', value = ixg_str)

# ----- PLOTS LAYOUT ----- #
c_sanky, c_bar = st.columns(2, gap = 'large', vertical_alignment = 'top')

# ----- SANKEY DIAGRAM ----- #
with c_sanky:
    if r is None:
        st.info('No data available for this selection.')
    else:
        # Build column names.
        corsi_col   = f'iCorsiF_{game_type}{category_suffix}'
        fenwick_col = f'iFenwickF_{game_type}{category_suffix}'
        sog_col     = f'iSOGF_{game_type}{category_suffix}'
        goal_col    = f'iGF_{game_type}{category_suffix}'

        # Pull/compute stage totals/drop-offs.
        corsi   = int(round(float(r[corsi_col])))
        fenwick = int(round(float(r[fenwick_col])))
        sog     = int(round(float(r[sog_col])))
        goals   = int(round(float(r[goal_col])))
        blocked = max(corsi - fenwick, 0)
        missed  = max(fenwick - sog, 0)
        saved   = max(sog - goals, 0)
        values  = [int(v) for v in [fenwick, blocked, sog, missed, goals, saved]]

        # Define nodes and links.
        node_names = ['Corsi', 'Blocked', 'Fenwick', 'Missed', 'SOG', 'Saved', 'Goals']
        node_vals  = [corsi, blocked, fenwick, missed, sog, saved, goals]
        nodes      = [f'{n} ({v})' for n, v in zip(node_names, node_vals)]
        sources    = [0, 0, 2, 2, 4, 4]
        targets    = [2, 1, 4, 3, 6, 5]
        values     = [fenwick, blocked, sog, missed, goals, saved]

        # Define colors.
        NODE_COLORS = {
            'Corsi': 'rgba(255,  59,  48, 0.95)',  # red
            'Blocked': 'rgba(255,  59,  48, 0.95)',  # red
            'Fenwick': 'rgba(255, 149,   0, 0.95)',  # orange
            'Missed': 'rgba(255, 149,   0, 0.95)',  # orange
            'SOG': 'rgba(255, 214,  10, 0.95)',  # yellow
            'Saved': 'rgba(255, 214,  10, 0.95)',  # yellow
            'Goals': 'rgba( 52, 199,  89, 0.97)',  # green
        }
        LINK_COLORS = [
            'rgba(255,  59,  48, 0.35)',  # Corsi -> Fenwick
            'rgba(255,  59,  48, 0.35)',  # Corsi -> Blocked
            'rgba(255, 149,   0, 0.35)',  # Fenwick -> SOG
            'rgba(255, 149,   0, 0.35)',  # Fenwick -> Missed
            'rgba(255, 214,  10, 0.35)',  # SOG -> Goals
            'rgba(255, 214,  10, 0.35)',  # SOG -> Saved
        ]

        # Lock node positions to preserve order.
        eps = 1e-3
        X = [0.01, 0.99, 0.33, 0.99, 0.66, 0.99, 0.99]
        Y = [eps, 0.70, eps, 0.45, eps, 0.20, eps]

        # Create figure.
        fig = go.Figure(
            go.Sankey(
                arrangement = 'snap',
                valueformat = '.0f',
                hoverinfo   = 'skip',
                node = dict(
                    label     = nodes,
                    x         = X,
                    y         = Y,
                    pad       = 18,
                    thickness = 18,
                    color     = [NODE_COLORS[n] for n in node_names],
                    line      = dict(width = 0.5, color = 'rgba(255,255,255,0.25)'),
                ),
                link = dict(
                    source = sources,
                    target = targets,
                    value  = values,
                    color  = LINK_COLORS,
                ),
            )
        )
        fig.update_layout(
            title  = 'Shot Volume and Outcome Flow',
            margin = dict(l = 10, r = 10, t = 50, b = 10),
        )
        st.plotly_chart(fig, width = 'stretch')

# ----- DIVERGING PERCENTILE BAR CHART ----- #
with c_bar:
    if r is None:
        st.info('No data available for this selection.')
    else:
        # Build column names.
        METRICS  = ['iCorsiF', 'iFenwickF', 'iSOGF', 'iGF', 'ixGF', 'iGFaX']
        pct_cols = [f'{m}_{game_type}{category_suffix}_pct' for m in METRICS]
        pcts     = [r.get(c, float('nan')) for c in pct_cols]
        df       = pd.DataFrame({'metric': METRICS, 'pct': pcts})

        # Center at 50th percentile.
        df['delta']      = df['pct'] - 50
        df['delta_plot'] = df['delta'].fillna(0.0)

        def _bar_color(p):
            if pd.isna(p):
                return 'rgba(160,160,160,0.45)'
            return 'rgba( 52,199, 89,0.85)' if p >= 50 else 'rgba(255, 59, 48,0.85)'

        def _label(p):
            if pd.isna(p):
                return 'N/A'
            return f'{int(round(p))}'

        df['color'] = df['pct'].apply(_bar_color)
        df['label'] = df['pct'].apply(_label)

        fig = go.Figure(
            go.Bar(
                x            = df['delta_plot'],
                y            = df['metric'],
                orientation  = 'h',
                marker_color = df['color'],
                text         = df['label'],
                textposition = 'outside',
                cliponaxis   = False,
                hoverinfo    = 'skip',
            )
        )

        # 50th percentile line (delta = 0).
        fig.add_vline(
            x          = 0,
            line_width = 2,
            line_dash  = 'dash',
            line_color = 'rgba(255,255,255,0.35)',
        )

        # Show percentile ticks even though axis is delta-based.
        tickvals = [-50, -25, 0, 25, 50]
        ticktext = ['0', '25', '50', '75', '100']

        fig.update_layout(
            title  = 'Metrics vs. League in Percentiles',
            margin = dict(l=10, r=10, t=50, b=10),
            xaxis  = dict(
                range      = [-50, 50],
                tickmode   = 'array',
                tickvals   = tickvals,
                ticktext   = ticktext,
                ticksuffix = '%',
                zeroline   = False,
            ),
            yaxis  = dict(categoryorder = 'array', categoryarray = METRICS[::-1]),
        )

        # Disable zoom.
        fig.update_xaxes(fixedrange=True)
        fig.update_yaxes(fixedrange=True)

        st.plotly_chart(fig, width = 'stretch')