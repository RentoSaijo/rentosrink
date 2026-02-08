# Import libraries.
import os
import streamlit as st
import pandas as pd
import plotly.graph_objects as go
from utils import load_biographies, load_contract_projection, load_contract_possibility

# Set constants.
SEASON_LABELS  = {'2026-2027': '20262027'}
SEASON_OPTIONS = list(SEASON_LABELS.keys())
PLOT_H = 400

# Helpers.
def _img(path, fallback=None):
    if path and os.path.exists(path):
        st.image(path, width='stretch')
    elif fallback and os.path.exists(fallback):
        st.image(fallback, width='stretch')

def _fmt_usd(x):
    if x is None or pd.isna(x):
        return 'N/A'
    x = float(x)
    ax = abs(x)
    if ax >= 1e9:
        return f'${x/1e9:.2f}B'
    if ax >= 1e6:
        return f'${x/1e6:.2f}M'
    if ax >= 1e3:
        return f'${x/1e3:.0f}k'
    return f'${x:,.0f}'

def _fmt_delta_term(x):
    if x is None or pd.isna(x):
        return 'N/A'
    return f"{float(x):+.1f} yrs"

def _fmt_delta_usd(x):
    if x is None or pd.isna(x):
        return 'N/A'
    x = float(x)
    sign = '+' if x >= 0 else '-'
    return f"{sign}${abs(x)/1e6:.2f}M"

def _y_range(series, pad=1.0, lo=0.0):
    s = pd.to_numeric(series, errors='coerce').dropna()
    if s.empty:
        return None
    vmin = float(s.min())
    vmax = float(s.max())
    return [max(lo, vmin - pad), vmax + pad]

def _make_series_fig(hist, j, y_col, title, y_title, y_range=None, y_tickprefix=None):
    fig = go.Figure()

    hist_pre = hist.iloc[:j].copy() if j >= 1 else hist.iloc[:0].copy()
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

    if j >= 1:
        seg = hist.iloc[j - 1: j + 1]
        fig.add_trace(
            go.Scatter(
                x=seg['contract_n'],
                y=seg[y_col],
                mode='lines+markers',
                line=dict(dash='dot', width=3),
                marker=dict(size=[0, 7]),
                hoverinfo='skip',
                showlegend=False,
            )
        )
    else:
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

# Load biographies.
bio = load_biographies()

# Set filters.
c_season, c_player = st.columns(2, gap='small', vertical_alignment='top')

with c_season:
    season_label = st.selectbox('Season', SEASON_OPTIONS, index=0, key='cp_season_label')
season = SEASON_LABELS[season_label]

cp = load_contract_projection(season)

available_ids = set(cp['playerId'].dropna().astype(int).unique())
bio_season    = bio[bio['playerId'].isin(available_ids)].sort_values('menuName')
name_to_id    = dict(zip(bio_season['menuName'], bio_season['playerId']))
player_names  = list(name_to_id.keys())

if 'cp_player_name' not in st.session_state:
    st.session_state['cp_player_name'] = None
if st.session_state['cp_player_name'] is None and player_names:
    st.session_state['cp_player_name'] = player_names[0]

player_index = player_names.index(st.session_state['cp_player_name']) if st.session_state['cp_player_name'] in player_names else 0

with c_player:
    player_name = st.selectbox(
        'Player',
        player_names,
        index=player_index if player_names else None,
        placeholder='N/A' if not player_names else None,
        key='cp_player_name',
    )

player_id = name_to_id.get(player_name)

if player_id is None:
    st.info('Select a player.')
    st.stop()

# Player history in projection table.
dfp = cp.loc[cp['playerId'] == int(player_id)].copy()
dfp = dfp.sort_values(['age', 'startSeason']).reset_index(drop=True)
dfp['contract_n'] = dfp.index + 1

sel_idx = dfp.index[dfp['startSeason'].astype(str) == str(season)].tolist()
if not sel_idx:
    st.info('No contract found for the selected season.')
    st.stop()

j = sel_idx[0]
if j < 1:
    st.info('Need at least 2 contracts for delta calculations.')
    st.stop()

hist = dfp.iloc[: j + 1].copy()
sel  = hist.iloc[j]
prev = hist.iloc[j - 1]

ideal_term = pd.to_numeric(sel.get('term', float('nan')), errors='coerce')
ideal_aav  = pd.to_numeric(sel.get('AAV',  float('nan')), errors='coerce')

curr_term = pd.to_numeric(prev.get('term', float('nan')), errors='coerce')
curr_aav  = pd.to_numeric(prev.get('AAV',  float('nan')), errors='coerce')

d_ideal_term = float(ideal_term) - float(curr_term) if (pd.notna(ideal_term) and pd.notna(curr_term)) else float('nan')
d_ideal_aav  = float(ideal_aav)  - float(curr_aav)  if (pd.notna(ideal_aav)  and pd.notna(curr_aav))  else float('nan')

poss = load_contract_possibility(season)
df_pos = poss.loc[poss['playerId'] == int(player_id)].copy()

likely_term = float('nan')
likely_aav  = float('nan')

if not df_pos.empty:
    row = df_pos.iloc[0]

    recs = []
    for t in range(1, 9):
        recs.append({
            'term': t,
            'prob': row.get(f'termProb_{t}', float('nan')),
            'aav':  row.get(f'xAAV_{t}',     float('nan')),
        })

    dp = pd.DataFrame(recs)
    dp['prob'] = pd.to_numeric(dp['prob'], errors='coerce')
    dp['aav']  = pd.to_numeric(dp['aav'],  errors='coerce')

    pmax = dp['prob'].max(skipna=True)
    if pd.notna(pmax) and pmax > 1.0:
        dp['prob'] = dp['prob'] / 100.0

    dp2 = dp.dropna(subset=['prob']).copy()
    if not dp2.empty:
        t_star = int(dp2.loc[dp2['prob'].idxmax(), 'term'])
        likely_term = float(t_star)
        likely_aav  = float(pd.to_numeric(row.get(f'xAAV_{t_star}', float('nan')), errors='coerce'))

d_likely_term = float(likely_term) - float(curr_term) if (pd.notna(likely_term) and pd.notna(curr_term)) else float('nan')
d_likely_aav  = float(likely_aav)  - float(curr_aav)  if (pd.notna(likely_aav)  and pd.notna(curr_aav))  else float('nan')

# Create row with headshot + metrics.
c_hs, c_m1, c_m2, c_m3, c_m4 = st.columns([0.75, 1, 1, 1, 1], gap='small', vertical_alignment='center')

with c_hs:
    with st.container(border=True):
        headshot_path = f'assets/headshots/{int(player_id)}.png'
        _img(headshot_path, fallback='assets/headshots/default.png')

with c_m1:
    with st.container(border=True):
        st.metric(
            'Likely Term',
            value=('N/A' if pd.isna(likely_term) else f'{float(likely_term):.1f} yrs'),
            delta=_fmt_delta_term(d_likely_term),
            delta_color='normal',
        )

with c_m2:
    with st.container(border=True):
        st.metric(
            'Likely AAV',
            value=_fmt_usd(likely_aav),
            delta=_fmt_delta_usd(d_likely_aav),
            delta_color='normal',
        )

with c_m3:
    with st.container(border=True):
        st.metric(
            'Ideal Term',
            value=('N/A' if pd.isna(ideal_term) else f'{float(ideal_term):.1f} yrs'),
            delta=_fmt_delta_term(d_ideal_term),
            delta_color='normal',
        )

with c_m4:
    with st.container(border=True):
        st.metric(
            'Ideal AAV',
            value=_fmt_usd(ideal_aav),
            delta=_fmt_delta_usd(d_ideal_aav),
            delta_color='normal',
        )

# Create plots.
c1, c2, c3 = st.columns(3, gap='small', vertical_alignment='top')

with c1:
    df_pos = poss.loc[poss['playerId'] == int(player_id)].copy()
    if df_pos.empty:
        st.info('No contract possibility data available for this player.')
    else:
        row = df_pos.iloc[0]

        records = []
        for t in range(1, 9):
            aav = row.get(f'xAAV_{t}', float('nan'))
            p   = row.get(f'termProb_{t}', float('nan'))
            records.append({'term': t, 'aav': aav, 'prob': p})

        dp = pd.DataFrame(records)
        dp['aav']  = pd.to_numeric(dp['aav'], errors='coerce')
        dp['prob'] = pd.to_numeric(dp['prob'], errors='coerce')

        pmax = dp['prob'].max(skipna=True)
        if pd.notna(pmax) and pmax > 1.0:
            dp['prob'] = dp['prob'] / 100.0

        dp['prob_clamped'] = dp['prob'].clip(lower=0.0, upper=1.0).fillna(0.0)
        dp['msize'] = 10 + 30 * dp['prob_clamped']
        dp_plot = dp.dropna(subset=['aav']).copy()

        dp_plot['prob_txt'] = dp_plot['prob_clamped'].apply(lambda p: '' if pd.isna(p) else f'{p:.0%}')

        fig_poss = go.Figure(
            go.Scatter(
                x=dp_plot['term'],
                y=dp_plot['aav'],
                mode='lines+markers+text',
                line=dict(width=3),
                marker=dict(size=dp_plot['msize'], sizemode='diameter'),
                text=dp_plot['prob_txt'],
                textposition='top center',
                textfont=dict(size=12),
                hovertemplate="Term: %{x:.0f} yrs<br>AAV: $%{y:,.0f}<extra></extra>",
                showlegend=False,
                cliponaxis=False,
            )
        )

        fig_poss.update_layout(
            title=dict(text='Contract Possibilities', x=0.5, xanchor='center'),
            margin=dict(l=10, r=10, t=50, b=10),
            xaxis=dict(title='Term (years)', tickmode='linear', dtick=1, range=[0.5, 8.5]),
            yaxis=dict(title='Projected AAV', tickprefix='$'),
            height=PLOT_H,
        )

        fig_poss.update_xaxes(fixedrange=True)
        fig_poss.update_yaxes(fixedrange=True)

        st.plotly_chart(fig_poss, width='stretch', config={'displayModeBar': True})

with c2:
    term_range = _y_range(hist['term'], pad=1.0, lo=0.0)
    aav_range  = _y_range(hist['AAV'],  pad=1_000_000.0, lo=0.0)

    fig_combo = go.Figure()

    fig_term_part = _make_series_fig(
        hist=hist, j=j,
        y_col='term',
        title='',
        y_title='Term (years)',
        y_range=term_range,
    )
    for k, tr in enumerate(fig_term_part.data):
        tr.update(
            yaxis='y',
            name='Term',
            showlegend=(k == 0),
            hovertemplate="Contract: %{x:.0f}<br>Term: %{y:.1f} yrs<extra></extra>",
        )
        fig_combo.add_trace(tr)

    fig_aav_part = _make_series_fig(
        hist=hist, j=j,
        y_col='AAV',
        title='',
        y_title='AAV',
        y_range=aav_range,
        y_tickprefix='$',
    )
    for k, tr in enumerate(fig_aav_part.data):
        tr.update(
            yaxis='y2',
            name='AAV',
            showlegend=(k == 0),
            hovertemplate="Contract: %{x:.0f}<br>AAV: $%{y:,.0f}<extra></extra>",
        )
        fig_combo.add_trace(tr)

    fig_combo.update_layout(
        title=dict(text='Contract Projection', x=0.5, xanchor='center'),
        xaxis=dict(
            title='Contract #',
            tickmode='linear',
            dtick=1,
            range=[0.5, float(hist['contract_n'].max()) + 0.5],
        ),
        yaxis=dict(
            title='Term (years)',
            range=term_range,
        ),
        yaxis2=dict(
            title='AAV',
            range=aav_range,
            overlaying='y',
            side='right',
            tickprefix='$',
        ),
        legend=dict(
            orientation='h',
            x=0.5,
            xanchor='center',
            y=-0.18,
            yanchor='top',
        ),
        margin=dict(l=10, r=10, t=60, b=80),
        hovermode='x unified',
        height=PLOT_H,
    )

    fig_combo.update_xaxes(fixedrange=True)
    fig_combo.update_yaxes(fixedrange=True)
    fig_combo.update_layout(yaxis2=dict(fixedrange=True))

    st.plotly_chart(fig_combo, width='stretch', config={'displayModeBar': True})

with c3:
    season_str = str(season)

    cp_season = cp.copy()
    cp_season['startSeason_str'] = cp_season['startSeason'].astype(str)
    cp_season = cp_season.loc[cp_season['startSeason_str'] == season_str].copy()

    cp_season['term'] = pd.to_numeric(cp_season.get('term'), errors='coerce')
    cp_season['AAV']  = pd.to_numeric(cp_season.get('AAV'),  errors='coerce')
    cp_season = cp_season.dropna(subset=['term', 'AAV', 'playerId'])

    if cp_season.empty:
        st.info('No projected contracts found for this season.')
    else:
        cp_season['is_player'] = (cp_season['playerId'].astype(int) == int(player_id))
        others = cp_season.loc[~cp_season['is_player']].copy()
        mine   = cp_season.loc[ cp_season['is_player']].copy()

        fig_sc = go.Figure()

        fig_sc.add_trace(
            go.Scatter(
                x=others['term'],
                y=others['AAV'],
                mode='markers',
                marker=dict(
                    size=7,
                    opacity=0.55,
                    color='rgba(160,160,160,0.65)',
                    symbol='circle',
                ),
                hovertemplate="Term: %{x:.1f} yrs<br>AAV: $%{y:,.0f}<extra></extra>",
                showlegend=False,
            )
        )

        if not mine.empty:
            fig_sc.add_trace(
                go.Scatter(
                    x=mine['term'],
                    y=mine['AAV'],
                    mode='markers',
                    marker=dict(
                        size=14,
                        opacity=1,
                        symbol='star',
                        color='yellow',
                    ),
                    hovertemplate="Term: %{x:.1f} yrs<br>AAV: $%{y:,.0f}<extra></extra>",
                    showlegend=False,
                )
            )

        fig_sc.update_layout(
            title=dict(text='vs. Other Free Agents', x=0.5, xanchor='center'),
            margin=dict(l=10, r=10, t=50, b=10),
            xaxis=dict(title='Projected Term (years)'),
            yaxis=dict(title='Projected AAV', tickprefix='$'),
            height=PLOT_H,
        )

        fig_sc.update_xaxes(fixedrange=True)
        fig_sc.update_yaxes(fixedrange=True)

        st.plotly_chart(fig_sc, width='stretch', config={'displayModeBar': True})
        