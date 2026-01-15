# Import libraries.
import streamlit as st
import pandas as pd
import plotly.graph_objects as go
from utils import load_biographies, load_contract_projection

# Hardcode season (only 2026-27).
SEASONS = ['20262027']
SEASON_LABELS  = {'2026-2027': '20262027'}
SEASON_OPTIONS = list(SEASON_LABELS.keys())

# Load biographies.
bio = load_biographies()

# Format selection menu (season first).
c_season, c_player = st.columns(2, gap='small', vertical_alignment='top')

# --- SEASON --- #
with c_season:
    season_label = st.selectbox('Season', SEASON_OPTIONS, index=0, key='cp_season_label')
season = SEASON_LABELS[season_label]

# Load contract projection for the selected season.
cp = load_contract_projection(season)

# Get available players in that season.
available_ids = set(cp['playerId'].dropna().astype(int).unique())

# Map playerId -> menuName (only players present in cp).
bio_season   = bio[bio['playerId'].isin(available_ids)].sort_values('menuName')
name_to_id   = dict(zip(bio_season['menuName'], bio_season['playerId']))
player_names = list(name_to_id.keys())

# --- PLAYER --- #
if 'cp_player_name' not in st.session_state:
    st.session_state['cp_player_name'] = None
if st.session_state['cp_player_name'] is None and player_names:
    st.session_state['cp_player_name'] = player_names[0]

player_index = 0
if st.session_state['cp_player_name'] in player_names:
    player_index = player_names.index(st.session_state['cp_player_name'])

with c_player:
    player_name = st.selectbox(
        'Player',
        player_names,
        index       = player_index if player_names else None,
        placeholder = 'N/A' if not player_names else None,
        key         = 'cp_player_name',
    )

player_id = name_to_id.get(player_name)

# --- TERM + AAV OVER CAREER (SIDE BY SIDE) --- #
dfp = cp.loc[cp['playerId'] == int(player_id)].copy()
dfp = dfp.sort_values(['age', 'startSeason']).reset_index(drop=True)

# Contract number (1..N)
dfp['contract_n'] = dfp.index + 1

# Find the contract row for the selected season (startSeason == season)
sel_idx = dfp.index[dfp['startSeason'].astype(str) == str(season)].tolist()
if not sel_idx:
    st.info('No contract found for the selected season.')
else:
    j = sel_idx[0]
    hist = dfp.iloc[: j + 1].copy()

    def _y_range(series, pad=1.0, lo=0.0):
        s = pd.to_numeric(series, errors='coerce').dropna()
        if s.empty:
            return None  # let plotly decide if no data
        vmin = float(s.min())
        vmax = float(s.max())
        y0 = max(lo, vmin - pad)
        y1 = vmax + pad
        return [y0, y1]

    def _make_series_fig(y_col, title, y_title, y_range=None, y_tickprefix=None):
        fig = go.Figure()

        # --- SOLID up to the point BEFORE the selected contract ---
        if j >= 1:
            hist_pre = hist.iloc[:j].copy()  # ends at j-1
        else:
            hist_pre = hist.iloc[:0].copy()

        if not hist_pre.empty:
            fig.add_trace(
                go.Scatter(
                    x=hist_pre['contract_n'],
                    y=hist_pre[y_col],
                    mode='lines+markers',
                    line=dict(width=3),
                    marker=dict(size=7),
                    hoverinfo='skip',
                    showlegend=False,
                )
            )

        # --- DOTTED only for the last segment (j-1 -> j) ---
        if j >= 1:
            seg = hist.iloc[j - 1: j + 1]
            fig.add_trace(
                go.Scatter(
                    x=seg['contract_n'],
                    y=seg[y_col],
                    mode='lines+markers',
                    line=dict(dash='dot', width=3),  # same color automatically
                    marker=dict(size=7),
                    hoverinfo='skip',
                    showlegend=False,
                )
            )
        else:
            # If the selected contract is the first one, just plot it normally
            only = hist.iloc[:1]
            fig.add_trace(
                go.Scatter(
                    x=only['contract_n'],
                    y=only[y_col],
                    mode='markers',
                    marker=dict(size=7),
                    hoverinfo='skip',
                    showlegend=False,
                )
            )

        fig.update_layout(
            title=title,
            margin=dict(l=10, r=10, t=50, b=10),
            xaxis=dict(
                title='Contract #',
                tickmode='linear',
                dtick=1,
                range=[0.5, float(hist['contract_n'].max()) + 0.5],
            ),
            yaxis=dict(
                title=y_title,
                range=y_range,
                tickprefix=y_tickprefix,
            ),
        )
        return fig

    # TERM figure (clipped to 0..10)
    term_range = _y_range(hist['term'], pad=1.0, lo=0.0)
    fig_term = _make_series_fig(
        y_col='term',
        title='Term Projection',
        y_title='Term (years)',
        y_range=term_range,
    )

    # AAV figure (NO manual scaling)
    fig_aav = go.Figure()

    # Solid history
    fig_aav.add_trace(
        go.Scatter(
            x=hist['contract_n'],
            y=hist['AAV'],  # <- confirm your column name
            mode='lines+markers',
            line=dict(width=3),
            marker=dict(size=7),
            hoverinfo='skip',
            showlegend=False,
        )
    )

    # Dotted last segment only (prev -> selected)
    if j >= 1:
        seg = dfp.iloc[j - 1: j + 1]
        fig_aav.add_trace(
            go.Scatter(
                x=seg['contract_n'],
                y=seg['AAV'],
                mode='lines+markers',
                line=dict(dash='dot', width=3),
                marker=dict(size=7),
                hoverinfo='skip',
                showlegend=False,
            )
        )

    # AAV figure (split so dotted segment has no solid underneath)
    aav_range = _y_range(hist['AAV'], pad=1_000_000.0, lo=0.0)
    fig_aav = _make_series_fig(
        y_col='AAV',
        title='Salary Projection',
        y_title='AAV',
        y_range=aav_range,
        y_tickprefix='$',
    )

    # ----- PLOTS (SIDE BY SIDE) ----- #
    c_left, c_right = st.columns(2, gap='large', vertical_alignment='top')

    with c_left:
        st.plotly_chart(fig_term, width='stretch')

    with c_right:
        st.plotly_chart(fig_aav, width='stretch')