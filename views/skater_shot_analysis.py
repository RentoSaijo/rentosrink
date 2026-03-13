# Import libraries.
import datetime as dt
import math
import os

import pandas as pd
import plotly.graph_objects as go
import streamlit as st

from utils import (
    load_biographies,
    load_games,
    load_gbgs_skater_advanced,
    load_gbgs_skater_basic,
    load_sbss_skaters,
)

# Set constants.
SEASON_START = 20112012
SEASON_END = 20252026

GAME_TYPES = {'Regular Season': 2, 'Playoffs': 3}

CATEGORIES = {'Actual': 'act', 'Per 60': 'p60'}

SITUATIONS = {
    'Even Strength': 'ev',
    'Power Play': 'pp',
    'Penalty Kill': 'sh',
    'All Situations': 'all',
}

GBGS_STRENGTHS = {
    'ev': ['ev'],
    'pp': ['pp'],
    'sh': ['sh'],
    'all': ['ev', 'pp', 'sh'],
}

SBSS_STRENGTHS = {
    'ev': ['even-strength'],
    'pp': ['power-play'],
    'sh': ['penalty-kill'],
    'all': ['even-strength', 'power-play', 'penalty-kill'],
}

METRIC_COLUMN_MAP = {
    'iCorsiF': 'iCF',
    'iFenwickF': 'iFF',
    'iShotF': 'iSF',
    'iGoalF': 'iGF',
    'ixGF': 'ixGF',
}

COUNT_METRICS = {'iCorsiF', 'iFenwickF', 'iShotF', 'iGoalF'}

DISPLAY_METRICS = [
    'iCorsiF',
    'iFenwickF',
    'iShotF',
    'iGoalF',
    'ixGF',
    'iGFAx',
]

SCATTER_METRICS = DISPLAY_METRICS.copy()

DISPLAY_LABELS = {
    'iShotF': 'iShotsF',
    'iGoalF': 'iGoalsF',
    'ixGF': 'ixGoalsF',
}

SHOT_OUTCOME_ORDER = ['Goal', 'Saved', 'Missed', 'Blocked']

PLOT_H = 440
SCATTER_CONTROL_H = 80
SCATTER_PLOT_H = max(PLOT_H - SCATTER_CONTROL_H, 220)
OUTLIER_IQR_MULT = 10


def _season_ids(start: int, end: int) -> list[str]:
    ids = []
    y0 = int(str(start)[:4])
    y1 = int(str(end)[:4])
    for y in range(y0, y1 + 1):
        ids.append(f'{y}{y + 1}')
    return ids


def _season_label(season_id: str) -> str:
    season_id = str(season_id)
    return f'{season_id[:4]}-{season_id[4:]}'


def _coerce_numeric_columns(df_in: pd.DataFrame, exclude: set[str]) -> pd.DataFrame:
    df = df_in.copy()
    for col in df.columns:
        if col not in exclude:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    return df


def _prepare_games(df_in: pd.DataFrame) -> pd.DataFrame:
    games = df_in.copy()
    games['gameId'] = pd.to_numeric(games.get('gameId'), errors='coerce')
    games['seasonId'] = pd.to_numeric(games.get('seasonId'), errors='coerce')
    games['gameTypeId'] = pd.to_numeric(games.get('gameTypeId'), errors='coerce')
    games['gameDate'] = pd.to_datetime(games.get('gameDate'), errors='coerce').dt.normalize()
    games = games.dropna(subset=['gameId', 'seasonId', 'gameTypeId', 'gameDate']).copy()
    games['gameId'] = games['gameId'].astype('int64')
    games['seasonId'] = games['seasonId'].astype('int64')
    games['gameTypeId'] = games['gameTypeId'].astype('int64')
    return games


def _prepare_biographies(df_in: pd.DataFrame) -> pd.DataFrame:
    bio = df_in.copy()
    bio['playerId'] = pd.to_numeric(bio.get('playerId'), errors='coerce')
    bio = bio.dropna(subset=['playerId']).copy()
    bio['playerId'] = bio['playerId'].astype('int64')
    bio['playerMenuName'] = bio.get('playerMenuName', bio.get('playerFullName', '')).astype(str).str.strip()
    bio['positionCode'] = bio.get('positionCode', '').astype(str).str.strip().str.upper()
    bio['positionGroup'] = bio['positionCode'].apply(
        lambda value: 'Defensemen' if value == 'D' else 'Forwards'
    )
    return bio


def _prepare_basic(df_in: pd.DataFrame) -> pd.DataFrame:
    basic = df_in.copy()
    basic['playerId'] = pd.to_numeric(basic.get('playerId'), errors='coerce')
    basic['gameId'] = pd.to_numeric(basic.get('gameId'), errors='coerce')
    basic = basic.dropna(subset=['playerId', 'gameId']).copy()
    basic['playerId'] = basic['playerId'].astype('int64')
    basic['gameId'] = basic['gameId'].astype('int64')
    basic = _coerce_numeric_columns(basic, exclude={'playerId', 'gameId', 'gameDate'})
    basic = basic.drop(columns=['gameDate'], errors='ignore')
    return basic


def _prepare_advanced(df_in: pd.DataFrame) -> pd.DataFrame:
    advanced = df_in.copy()
    advanced['playerId'] = pd.to_numeric(advanced.get('playerId'), errors='coerce')
    advanced['gameId'] = pd.to_numeric(advanced.get('gameId'), errors='coerce')
    advanced = advanced.dropna(subset=['playerId', 'gameId']).copy()
    advanced['playerId'] = advanced['playerId'].astype('int64')
    advanced['gameId'] = advanced['gameId'].astype('int64')
    advanced = _coerce_numeric_columns(advanced, exclude={'playerId', 'gameId'})
    return advanced


def _prepare_shots(df_in: pd.DataFrame) -> pd.DataFrame:
    shots = df_in.copy()
    numeric_cols = {
        'shooterPlayerId',
        'gameId',
        'eventId',
        'xCoordNorm',
        'yCoordNorm',
        'xG',
        'goaliePlayerId',
        'goalieTeamId',
        'shooterTeamId',
    }
    for col in numeric_cols:
        if col in shots.columns:
            shots[col] = pd.to_numeric(shots[col], errors='coerce')

    bool_cols = ['isRush', 'isRebound', 'isCorsi', 'isFenwick', 'isShot', 'isGoal']
    for col in bool_cols:
        if col in shots.columns:
            shots[col] = shots[col].astype(str).str.upper().eq('TRUE')

    shots['strengthState'] = shots.get('strengthState', '').fillna('even-strength')
    shots['strengthState'] = shots['strengthState'].replace('', 'even-strength')

    shots = shots.dropna(subset=['shooterPlayerId', 'gameId', 'eventId']).copy()
    shots['shooterPlayerId'] = shots['shooterPlayerId'].astype('int64')
    shots['gameId'] = shots['gameId'].astype('int64')
    shots['eventId'] = shots['eventId'].astype('int64')
    shots['xG'] = pd.to_numeric(shots.get('xG'), errors='coerce').fillna(0.0)
    return shots


@st.cache_data
def _load_page_data(season_id: str) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    games = _prepare_games(load_games())
    games = games.loc[games['seasonId'] == int(season_id)].copy()

    basic = _prepare_basic(load_gbgs_skater_basic(season_id))
    advanced = _prepare_advanced(load_gbgs_skater_advanced(season_id))
    shots = _prepare_shots(load_sbss_skaters(season_id))

    skater_games = basic.merge(
        advanced,
        on=['playerId', 'gameId'],
        how='left',
        suffixes=('', '_adv'),
    )

    game_meta = games[['gameId', 'gameDate', 'gameTypeId', 'seasonId']].drop_duplicates()
    skater_games = skater_games.merge(game_meta, on='gameId', how='left')
    shots = shots.merge(game_meta, on='gameId', how='left')

    return games, skater_games, shots


def _strength_suffixes(situation_code: str) -> list[str]:
    return GBGS_STRENGTHS.get(situation_code, [situation_code])


def _sbss_strength_values(situation_code: str) -> list[str]:
    return SBSS_STRENGTHS.get(situation_code, SBSS_STRENGTHS['all'])


def _sum_metric(df_in: pd.DataFrame, base_metric: str, situation_code: str) -> pd.Series:
    cols = [f'{base_metric}_{suffix}' for suffix in _strength_suffixes(situation_code) if f'{base_metric}_{suffix}' in df_in.columns]
    if not cols:
        return pd.Series(0.0, index=df_in.index, dtype=float)
    return df_in[cols].fillna(0.0).sum(axis=1).astype(float)


def _minutes_series(df_in: pd.DataFrame, situation_code: str) -> pd.Series:
    return _sum_metric(df_in, 'mP', situation_code)


def _raw_metric_series(df_in: pd.DataFrame, metric_name: str, situation_code: str) -> pd.Series:
    if metric_name == 'iGFAx':
        return (
            _raw_metric_series(df_in, 'iGoalF', situation_code)
            - _raw_metric_series(df_in, 'ixGF', situation_code)
        )

    base_metric = METRIC_COLUMN_MAP.get(metric_name)
    if base_metric is None:
        return pd.Series(0.0, index=df_in.index, dtype=float)
    return _sum_metric(df_in, base_metric, situation_code)


def _display_metric_series(
    df_in: pd.DataFrame,
    metric_name: str,
    category_code: str,
    situation_code: str,
) -> pd.Series:
    values = _raw_metric_series(df_in, metric_name, situation_code)
    if category_code != 'p60':
        return values.fillna(0.0)

    minutes = _minutes_series(df_in, situation_code)
    per60 = values.div(minutes.where(minutes > 0)).mul(60.0)
    return per60.replace([float('inf'), float('-inf')], 0.0).fillna(0.0)


def _aggregate_players(df_in: pd.DataFrame) -> pd.DataFrame:
    if df_in.empty:
        return pd.DataFrame(columns=['playerId'])

    numeric_cols = [
        col for col in df_in.columns
        if col not in {'playerId', 'gameId', 'gameDate', 'gameTypeId', 'seasonId'}
    ]
    if not numeric_cols:
        return df_in[['playerId']].drop_duplicates().copy()

    return df_in.groupby('playerId', as_index=False)[numeric_cols].sum()


def _eligible_population(df_in: pd.DataFrame, situation_code: str) -> pd.DataFrame:
    pop = df_in.copy()
    if pop.empty:
        return pop
    pop['minutes'] = _minutes_series(pop, situation_code)
    return pop.loc[pop['minutes'] > 0].copy()


def _filter_bottom_minutes_quantile(
    df_in: pd.DataFrame,
    situation_code: str,
    quantile: float = 0.25,
) -> pd.DataFrame:
    pop = df_in.copy()
    if pop.empty:
        return pop
    if 'minutes' not in pop.columns:
        pop['minutes'] = _minutes_series(pop, situation_code)
    minutes = pd.to_numeric(pop['minutes'], errors='coerce').fillna(0.0)
    if minutes.empty:
        return pop
    cutoff = float(minutes.quantile(quantile))
    filtered = pop.loc[minutes > cutoff].copy()
    if filtered.empty:
        filtered = pop.loc[minutes >= cutoff].copy()
    return filtered if not filtered.empty else pop


def _default_player_id(df_in: pd.DataFrame, situation_code: str) -> int | None:
    if df_in.empty:
        return None
    ranking = _raw_metric_series(df_in, 'ixGF', situation_code)
    if ranking.empty:
        return None
    ranked = df_in.assign(_default_rank=ranking)
    ranked = ranked.sort_values(['_default_rank', 'playerId'], ascending=[False, True])
    if ranked.empty:
        return None
    return int(ranked.iloc[0]['playerId'])


def _name_lookup(bio_in: pd.DataFrame) -> dict[int, str]:
    return dict(zip(bio_in['playerId'], bio_in['playerMenuName']))


def _position_group_lookup(bio_in: pd.DataFrame) -> dict[int, str]:
    return dict(zip(bio_in['playerId'], bio_in['positionGroup']))


def _safe_name(lookup: dict[int, str], player_id: int) -> str:
    return lookup.get(int(player_id), f'Player {int(player_id)}')


def _safe_position_group(lookup: dict[int, str], player_id: int | None) -> str | None:
    if player_id is None:
        return None
    return lookup.get(int(player_id), 'Forwards')


def _date_tuple(value) -> tuple[dt.date | None, dt.date | None]:
    if isinstance(value, tuple) or isinstance(value, list):
        if len(value) >= 2:
            return value[0], value[1]
        if len(value) == 1:
            return value[0], value[0]
        return None, None
    return value, value


def _clamp_date_range(
    current_value,
    min_date: dt.date,
    max_date: dt.date,
) -> tuple[dt.date, dt.date]:
    start_date, end_date = _date_tuple(current_value)
    if start_date is None or end_date is None:
        return min_date, max_date

    start_date = min(max(start_date, min_date), max_date)
    end_date = min(max(end_date, min_date), max_date)
    if start_date > end_date:
        start_date, end_date = min_date, max_date
    return start_date, end_date


def _fmt_sigma(value: float) -> str:
    if pd.isna(value):
        value = 0.0
    return f'{float(value):+.1f}σ'


def _fmt_metric_value(metric_name: str, value: float, category_code: str) -> str:
    if pd.isna(value):
        value = 0.0

    value = float(value)
    if category_code == 'act' and metric_name in COUNT_METRICS:
        return f'{int(round(value))}'
    return f'{value:.1f}'


def _metric_label(metric_name: str) -> str:
    return DISPLAY_LABELS.get(metric_name, metric_name)


def _z_score(values: pd.Series, player_value: float) -> float:
    series = pd.to_numeric(values, errors='coerce').fillna(0.0)
    if series.empty:
        return 0.0
    sd = float(series.std(ddof=0))
    if sd <= 0 or pd.isna(sd):
        return 0.0
    mu = float(series.mean())
    return (float(player_value) - mu) / sd


def _shot_outcome(row: pd.Series) -> str | None:
    if bool(row.get('isGoal')):
        return 'Goal'
    if bool(row.get('isShot')):
        return 'Saved'
    if bool(row.get('isFenwick')):
        return 'Missed'
    if bool(row.get('isCorsi')):
        return 'Blocked'
    return None


def _volume_outcome_node_y(blocked: float, missed: float, saved: float, goal: float) -> list[float]:
    positions = [1e-3, 0.70, 1e-3, 0.45, 1e-3, 0.20, 1e-3]
    outcome_slots = [1e-3, 0.20, 0.45, 0.70]
    outcomes = [
        (6, goal),
        (5, saved),
        (3, missed),
        (1, blocked),
    ]

    slot_idx = 0
    for node_idx, value in outcomes:
        if float(value) > 0:
            positions[node_idx] = outcome_slots[slot_idx]
            slot_idx += 1

    return positions


def _xg_color_spec(values: pd.Series) -> tuple[float, list[float], list[str], list[list[float | str]]]:
    xg = pd.to_numeric(values, errors='coerce').fillna(0.0)
    pos_xg = xg.loc[xg > 0]
    if pos_xg.empty:
        xg_cap = 0.20
    else:
        xg_cap = float(pos_xg.quantile(0.98))
        if not pd.notna(xg_cap) or xg_cap <= 0:
            xg_cap = 0.20
    xg_cap = min(1.0, xg_cap)

    tick_vals = [0.0, 1.0 / 3.0, 2.0 / 3.0, 1.0]
    legend_vals = [xg_cap * (math.exp(v * math.log(100.0)) - 1.0) / 99.0 for v in tick_vals]
    legend_vals[0] = 0.0
    legend_vals[-1] = xg_cap

    if xg_cap < 0.1:
        tick_text = [f'{value:.3f}' for value in legend_vals[:-1]] + [f'{xg_cap:.3f}+']
    else:
        tick_text = [f'{value:.2f}' for value in legend_vals[:-1]] + [f'{xg_cap:.2f}+']

    colorscale = [
        [0.0, '#2166AC'],
        [0.25, '#67A9CF'],
        [0.5, '#D1E5F0'],
        [0.75, '#FDAE61'],
        [1.0, '#B2182B'],
    ]
    return xg_cap, tick_vals, tick_text, colorscale


def _img(path: str, fallback: str | None = None):
    if path and os.path.exists(path):
        st.image(path, width='stretch')
    elif fallback and os.path.exists(fallback):
        st.image(fallback, width='stretch')
    else:
        st.empty()


def _add_line(fig, x0, y0, x1, y1, color, width=3, dash=None, layer='below'):
    fig.add_shape(
        type='line',
        xref='x',
        yref='y',
        x0=x0,
        y0=y0,
        x1=x1,
        y1=y1,
        line=dict(color=color, width=width, dash=dash),
        layer=layer,
    )


def _add_circle(fig, cx, cy, radius, line_color='rgba(255,255,255,0.40)', width=2, fill='rgba(0,0,0,0)', layer='below'):
    fig.add_shape(
        type='circle',
        xref='x',
        yref='y',
        x0=cx - radius,
        x1=cx + radius,
        y0=cy - radius,
        y1=cy + radius,
        line=dict(color=line_color, width=width),
        fillcolor=fill,
        layer=layer,
    )


def _add_rect(fig, x0, y0, x1, y1, line_color='rgba(255,255,255,0.55)', width=2, fill='rgba(0,0,0,0)', layer='below'):
    fig.add_shape(
        type='rect',
        xref='x',
        yref='y',
        x0=x0,
        y0=y0,
        x1=x1,
        y1=y1,
        line=dict(color=line_color, width=width),
        fillcolor=fill,
        layer=layer,
    )


def _add_path(fig, path, line_color='rgba(255,255,255,0.55)', width=2, fill='rgba(0,0,0,0)', layer='below'):
    fig.add_shape(
        type='path',
        xref='x',
        yref='y',
        path=path,
        line=dict(color=line_color, width=width),
        fillcolor=fill,
        layer=layer,
    )


def _arc_points(
    cx: float,
    cy: float,
    radius: float,
    start_deg: float,
    end_deg: float,
    steps: int = 64,
) -> tuple[list[float], list[float]]:
    angles = [
        math.radians(start_deg + (end_deg - start_deg) * i / (steps - 1))
        for i in range(steps)
    ]
    xs = [cx + radius * math.cos(angle) for angle in angles]
    ys = [cy + radius * math.sin(angle) for angle in angles]
    return xs, ys


def _shot_locations_rink_figure(shots: pd.DataFrame | None = None) -> go.Figure:
    board_color = 'rgba(140,140,145,0.78)'
    red_color = 'rgba(255,0,0,0.90)'
    blue_color = 'rgba(0,70,255,0.95)'
    crease_fill = 'rgba(120,120,120,0.55)'

    x_min, x_max = -42.5, 42.5
    y_min, y_max = 16.0, 100.0

    corner_radius = 28.0
    side_end_y = y_max - corner_radius
    corner_center_y = side_end_y
    corner_center_offset = x_max - corner_radius
    goal_line_y = 89.0
    blue_line_y = 25.0
    neutral_spot_y = 20.0
    circle_y = 69.0
    circle_x = 22.0
    circle_r = 15.0
    crease_half_width = 4.0
    crease_rect_height = 4.47
    crease_arc_radius = 6.0
    crease_arc_join_y = goal_line_y - crease_rect_height
    crease_theta = math.degrees(math.atan2(crease_rect_height, crease_half_width))
    crease_arc_start = 180.0 + crease_theta
    crease_arc_end = 360.0 - crease_theta
    faceoff_dot_radius = 0.5

    left_arc_x, left_arc_y = _arc_points(
        -corner_center_offset,
        corner_center_y,
        corner_radius,
        180.0,
        90.0,
    )
    right_arc_x, right_arc_y = _arc_points(
        corner_center_offset,
        corner_center_y,
        corner_radius,
        90.0,
        0.0,
    )

    def _board_half_width(y_value: float) -> float:
        if y_value <= side_end_y:
            return x_max
        dy = y_value - corner_center_y
        dx = math.sqrt(max(corner_radius**2 - dy**2, 0.0))
        return corner_center_offset + dx

    goal_line_half_width = _board_half_width(goal_line_y)

    board_x = [x_min, x_min, *left_arc_x, *right_arc_x, x_max]
    board_y = [y_min, side_end_y, *left_arc_y, *right_arc_y, y_min]

    crease_arc_x, crease_arc_y_points = _arc_points(
        0.0,
        goal_line_y,
        crease_arc_radius,
        crease_arc_start,
        crease_arc_end,
        steps=48,
    )
    crease_x = [-crease_half_width, -crease_half_width, *crease_arc_x, crease_half_width, crease_half_width]
    crease_y = [goal_line_y, crease_arc_join_y, *crease_arc_y_points, crease_arc_join_y, goal_line_y]
    net_x = [-3.0, -3.0, -2.1, 2.1, 3.0, 3.0]
    net_y = [goal_line_y, 91.0, 92.7, 92.7, 91.0, goal_line_y]

    fig = go.Figure()

    fig.add_trace(
        go.Scatter(
            x=board_x,
            y=board_y,
            mode='lines',
            line=dict(color=board_color, width=4),
            hoverinfo='skip',
            showlegend=False,
        )
    )

    shot_df = pd.DataFrame() if shots is None else shots.copy()
    if not shot_df.empty:
        shot_df = shot_df.dropna(subset=['xCoordNorm', 'yCoordNorm']).copy()
        shot_df['outcome'] = shot_df.apply(_shot_outcome, axis=1)
        shot_df = shot_df.dropna(subset=['outcome']).copy()
        if not shot_df.empty:
            shot_df['plot_x'] = pd.to_numeric(shot_df['yCoordNorm'], errors='coerce')
            shot_df['plot_y'] = pd.to_numeric(shot_df['xCoordNorm'], errors='coerce')
            shot_df['xG'] = pd.to_numeric(shot_df['xG'], errors='coerce').fillna(0.0)
            shot_df['isRushLabel'] = shot_df['isRush'].map({True: 'Yes', False: 'No'}).fillna('No')
            shot_df['isReboundLabel'] = shot_df['isRebound'].map({True: 'Yes', False: 'No'}).fillna('No')
            xg_cap, xg_tick_vals, xg_tick_text, xg_colorscale = _xg_color_spec(shot_df['xG'])
            capped_xg = shot_df['xG'].clip(lower=0.0, upper=xg_cap)
            if xg_cap > 0:
                shot_df['xg_scaled'] = (
                    (1.0 + 99.0 * capped_xg.div(xg_cap)).apply(math.log)
                    / math.log(100.0)
                )
            else:
                shot_df['xg_scaled'] = 0.0

            non_goal_df = shot_df.loc[shot_df['outcome'] != 'Goal'].copy()
            if not non_goal_df.empty:
                fig.add_trace(
                    go.Scatter(
                        x=non_goal_df['plot_x'],
                        y=non_goal_df['plot_y'],
                        mode='markers',
                        marker=dict(
                            size=8,
                            opacity=0.42,
                            symbol='circle',
                            color=non_goal_df['xg_scaled'],
                            coloraxis='coloraxis',
                            line=dict(width=0),
                        ),
                        customdata=non_goal_df[
                            ['outcome', 'xG', 'isRushLabel', 'isReboundLabel']
                        ].to_numpy(),
                        hovertemplate=(
                            'Outcome: %{customdata[0]}<br>'
                            'xG: %{customdata[1]:.3f}<br>'
                            'Rush: %{customdata[2]}<br>'
                            'Rebound: %{customdata[3]}<br>'
                            '<extra></extra>'
                        ),
                        showlegend=False,
                    )
                )

            goal_df = shot_df.loc[shot_df['outcome'] == 'Goal'].copy()
            if not goal_df.empty:
                fig.add_trace(
                    go.Scatter(
                        x=goal_df['plot_x'],
                        y=goal_df['plot_y'],
                        mode='markers',
                        marker=dict(
                            size=13,
                            opacity=0.92,
                            symbol='star',
                            color=goal_df['xg_scaled'],
                            coloraxis='coloraxis',
                            line=dict(width=0),
                        ),
                        customdata=goal_df[
                            ['outcome', 'xG', 'isRushLabel', 'isReboundLabel']
                        ].to_numpy(),
                        hovertemplate=(
                            'Outcome: %{customdata[0]}<br>'
                            'xG: %{customdata[1]:.3f}<br>'
                            'Rush: %{customdata[2]}<br>'
                            'Rebound: %{customdata[3]}<br>'
                            '<extra></extra>'
                        ),
                        showlegend=False,
                    )
                )

    _add_line(
        fig,
        -goal_line_half_width,
        goal_line_y,
        goal_line_half_width,
        goal_line_y,
        color=red_color,
        width=1.5,
        layer='above',
    )
    _add_line(fig, x_min, blue_line_y, x_max, blue_line_y, color=blue_color, width=3, layer='above')
    _add_circle(fig, -circle_x, circle_y, circle_r, line_color=red_color, width=1.4, layer='above')
    _add_circle(fig, circle_x, circle_y, circle_r, line_color=red_color, width=1.4, layer='above')
    fig.add_trace(
        go.Scatter(
            x=net_x,
            y=net_y,
            mode='lines',
            line=dict(color='rgba(95,95,95,0.95)', width=1.1),
            fill='toself',
            fillcolor='rgba(95,95,95,0.70)',
            hoverinfo='skip',
            showlegend=False,
        )
    )
    fig.add_trace(
        go.Scatter(
            x=crease_x,
            y=crease_y,
            mode='lines',
            line=dict(color=red_color, width=1.2),
            fill='toself',
            fillcolor=crease_fill,
            hoverinfo='skip',
            showlegend=False,
        )
    )

    for sign in (-1, 1):
        dot_x = sign * circle_x
        _add_circle(fig, dot_x, circle_y, faceoff_dot_radius, line_color=red_color, width=1.0, fill=red_color, layer='above')
        _add_line(fig, dot_x - circle_r - 2.0, circle_y + 1.4, dot_x - circle_r + 0.2, circle_y + 1.4, color=red_color, width=1.0, layer='above')
        _add_line(fig, dot_x - circle_r - 2.0, circle_y - 1.4, dot_x - circle_r + 0.2, circle_y - 1.4, color=red_color, width=1.0, layer='above')
        _add_line(fig, dot_x + circle_r - 0.2, circle_y + 1.4, dot_x + circle_r + 2.0, circle_y + 1.4, color=red_color, width=1.0, layer='above')
        _add_line(fig, dot_x + circle_r - 0.2, circle_y - 1.4, dot_x + circle_r + 2.0, circle_y - 1.4, color=red_color, width=1.0, layer='above')

    for sign in (-1, 1):
        _add_circle(
            fig,
            sign * 22.0,
            neutral_spot_y,
            faceoff_dot_radius,
            line_color=red_color,
            width=1.0,
            fill=red_color,
            layer='above',
        )

    _add_line(fig, -11.0, goal_line_y, -14.0, y_max, color=red_color, width=1.2, layer='above')
    _add_line(fig, 11.0, goal_line_y, 14.0, y_max, color=red_color, width=1.2, layer='above')

    fig.update_layout(
        title=dict(text='Shot Locations For', x=0.5, xanchor='center'),
        margin=dict(l=5, r=5, t=45, b=78),
        height=PLOT_H,
        uirevision='ssa-shot-locations',
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        modebar=dict(
            bgcolor='rgba(0,0,0,0)',
        ),
        showlegend=False,
        coloraxis=dict(
            cmin=0.0,
            cmax=1.0,
            colorscale=xg_colorscale if not shot_df.empty else [
                [0.0, '#2166AC'],
                [0.25, '#67A9CF'],
                [0.5, '#D1E5F0'],
                [0.75, '#FDAE61'],
                [1.0, '#B2182B'],
            ],
            colorbar=dict(
                title=dict(text='xG', side='top'),
                orientation='h',
                x=0.5,
                xanchor='center',
                y=-0.12,
                len=0.64,
                thickness=12,
                outlinewidth=0,
                tickmode='array',
                tickvals=xg_tick_vals if not shot_df.empty else [0.0, 1.0 / 3.0, 2.0 / 3.0, 1.0],
                ticktext=xg_tick_text if not shot_df.empty else ['0.00', '0.03', '0.08', '0.20+'],
                tickfont=dict(size=10),
            ),
        ),
        xaxis=dict(
            range=[x_min - 2.0, x_max + 2.0],
            domain=[0.0, 1.0],
            constrain='domain',
            showgrid=False,
            zeroline=False,
            visible=False,
            automargin=False,
            fixedrange=True,
        ),
        yaxis=dict(
            range=[y_min - 2.0, y_max + 2.0],
            domain=[0.0, 1.0],
            constrain='domain',
            showgrid=False,
            zeroline=False,
            visible=False,
            scaleanchor='x',
            scaleratio=1,
            automargin=False,
            fixedrange=True,
        ),
    )

    if shot_df.empty:
        fig.add_annotation(
            x=0.5,
            y=0.5,
            xref='paper',
            yref='paper',
            text='No shot attempts for this selection',
            showarrow=False,
            font=dict(size=14),
        )

    return fig


SEASON_IDS = sorted(_season_ids(SEASON_START, SEASON_END), key=lambda value: int(value), reverse=True)
SEASON_LABELS = {_season_label(season_id): season_id for season_id in SEASON_IDS}
SEASON_OPTIONS = list(SEASON_LABELS.keys())

DEFAULT_SEASON_ID = '20252026'
DEFAULT_SEASON_LABEL = _season_label(DEFAULT_SEASON_ID)
DEFAULT_SEASON_INDEX = SEASON_OPTIONS.index(DEFAULT_SEASON_LABEL) if DEFAULT_SEASON_LABEL in SEASON_OPTIONS else 0

biographies = _prepare_biographies(load_biographies())
name_by_player_id = _name_lookup(biographies)
position_group_by_player_id = _position_group_lookup(biographies)

# Set filters.
c_season, c_game, c_date, c_player, c_sit, c_cat = st.columns(6, gap='small', vertical_alignment='top')

with c_season:
    season_label = st.selectbox(
        'Season',
        SEASON_OPTIONS,
        index=DEFAULT_SEASON_INDEX,
        key='ssa_season_label',
    )

season_id = SEASON_LABELS[season_label]

season_games, season_player_games, season_player_shots = _load_page_data(season_id)

available_game_types = [
    label for label, game_type_id in GAME_TYPES.items()
    if game_type_id in set(season_games['gameTypeId'].dropna().astype(int).unique().tolist())
]
if not available_game_types:
    available_game_types = ['Regular Season']

prev_game_type = st.session_state.get('ssa_game_type_label')
if prev_game_type in available_game_types:
    game_type_index = available_game_types.index(prev_game_type)
elif 'Regular Season' in available_game_types:
    game_type_index = available_game_types.index('Regular Season')
else:
    game_type_index = 0

with c_game:
    if st.session_state.get('ssa_game_type_label') not in available_game_types:
        st.session_state['ssa_game_type_label'] = available_game_types[game_type_index]
    game_type_label = st.selectbox(
        'Game Type',
        available_game_types,
        index=game_type_index,
        key='ssa_game_type_label',
    )

game_type_id = GAME_TYPES[game_type_label]

game_scope = season_games.loc[season_games['gameTypeId'] == game_type_id].copy()
game_scope = game_scope.sort_values(['gameDate', 'gameId'])

date_min = game_scope['gameDate'].min()
date_max = game_scope['gameDate'].max()
date_range_key = 'ssa_date_range_saved'
date_range_widget_key = 'ssa_date_range_widget'
date_context_key = 'ssa_date_range_context'

with c_date:
    if game_scope.empty or pd.isna(date_min) or pd.isna(date_max):
        st.text_input('Date Range', value='No games available', disabled=True)
        selected_start_date = None
        selected_end_date = None
    else:
        min_date = date_min.date()
        max_date = date_max.date()
        current_context = (season_id, game_type_id)
        if st.session_state.get(date_context_key) != current_context:
            st.session_state[date_range_key] = (min_date, max_date)
            st.session_state[date_context_key] = current_context
            if date_range_widget_key in st.session_state:
                del st.session_state[date_range_widget_key]
        else:
            st.session_state[date_range_key] = _clamp_date_range(
                st.session_state.get(date_range_key),
                min_date,
                max_date,
            )

        selected_dates = st.date_input(
            'Date Range',
            value=st.session_state.get(date_range_key, (min_date, max_date)),
            min_value=min_date,
            max_value=max_date,
            key=date_range_widget_key,
        )
        st.session_state[date_range_key] = _clamp_date_range(selected_dates, min_date, max_date)
        selected_start_date, selected_end_date = _date_tuple(selected_dates)

with c_sit:
    situation_label = st.selectbox(
        'Situation',
        list(SITUATIONS.keys()),
        index=0,
        key='ssa_situation_label',
    )

situation_code = SITUATIONS[situation_label]

available_categories = list(CATEGORIES.keys())
prev_category = st.session_state.get('ssa_category_label')
if prev_category in available_categories:
    category_index = available_categories.index(prev_category)
elif 'Actual' in available_categories:
    category_index = available_categories.index('Actual')
else:
    category_index = 0

with c_cat:
    category_label = st.selectbox(
        'Category',
        available_categories,
        index=category_index,
        key='ssa_category_label',
    )

category_code = CATEGORIES[category_label]

if selected_start_date is None or selected_end_date is None:
    selected_game_ids = set()
else:
    selected_game_ids = set(
        game_scope.loc[
            game_scope['gameDate'].between(
                pd.Timestamp(selected_start_date),
                pd.Timestamp(selected_end_date),
            ),
            'gameId',
        ].astype('int64').tolist()
    )

filtered_player_games = season_player_games.loc[
    season_player_games['gameId'].isin(selected_game_ids)
].copy()

filtered_player_totals = _aggregate_players(filtered_player_games)
eligible_players = _eligible_population(filtered_player_totals, situation_code)
if not eligible_players.empty:
    eligible_players['positionGroup'] = eligible_players['playerId'].map(position_group_by_player_id).fillna('Forwards')

eligible_ids = eligible_players['playerId'].astype(int).tolist() if not eligible_players.empty else []
eligible_id_set = set(eligible_ids)

player_options = sorted(
    eligible_ids,
    key=lambda player_id: (_safe_name(name_by_player_id, player_id).lower(), player_id),
)

default_player_id = _default_player_id(eligible_players, situation_code)
fallback_player_id = default_player_id if default_player_id in eligible_id_set else (player_options[0] if player_options else None)

saved_player_id = st.session_state.get('ssa_player_id_saved')
selected_player_id = saved_player_id if saved_player_id in eligible_id_set else fallback_player_id

with c_player:
    if player_options:
        player_widget_key = 'ssa_player_id_widget'
        if st.session_state.get(player_widget_key) not in player_options and player_widget_key in st.session_state:
            del st.session_state[player_widget_key]
        player_index = player_options.index(selected_player_id) if selected_player_id in player_options else 0
        player_id = st.selectbox(
            'Player',
            options=player_options,
            index=player_index,
            format_func=lambda player_id: _safe_name(name_by_player_id, player_id),
            key=player_widget_key,
        )
    else:
        st.selectbox('Player', options=['N/A'], index=0, disabled=True, key='ssa_player_id_empty')
        player_id = None

st.session_state['ssa_player_id_saved'] = player_id

player_row = None
if player_id is not None and not eligible_players.empty:
    match = eligible_players.loc[eligible_players['playerId'] == int(player_id)]
    if not match.empty:
        player_row = match.iloc[[0]].copy()

player_position_group = _safe_position_group(position_group_by_player_id, player_id)
comparison_players = eligible_players.copy()
if player_position_group is not None and 'positionGroup' in comparison_players.columns:
    same_group = comparison_players.loc[comparison_players['positionGroup'] == player_position_group].copy()
    if not same_group.empty:
        comparison_players = same_group
comparison_players = _filter_bottom_minutes_quantile(comparison_players, situation_code)

metric_series_map = {
    metric_name: _display_metric_series(comparison_players, metric_name, category_code, situation_code)
    for metric_name in DISPLAY_METRICS
}

player_metric_values = {}
player_metric_deltas = {}
for metric_name, values in metric_series_map.items():
    if player_row is None:
        player_metric_values[metric_name] = 0.0
        player_metric_deltas[metric_name] = 0.0
        continue

    player_value = float(
        _display_metric_series(player_row, metric_name, category_code, situation_code).iloc[0]
    )
    player_metric_values[metric_name] = player_value
    player_metric_deltas[metric_name] = _z_score(values, player_value)

# Create row with headshot + metrics.
c_hs, c_m1, c_m2, c_m3, c_m4, c_m5, c_m6 = st.columns(
    [1.125, 1, 1, 1, 1, 1, 1],
    gap='small',
    vertical_alignment='center',
)

with c_hs:
    with st.container(border=True):
        if player_id is not None:
            _img(
                f'assets/headshots/{int(player_id)}.png',
                fallback='assets/headshots/default.png',
            )
        else:
            st.empty()

for column, metric_name in zip(
    [c_m1, c_m2, c_m3, c_m4, c_m5, c_m6],
    DISPLAY_METRICS,
):
    with column:
        with st.container(border=True):
            st.metric(
                _metric_label(metric_name),
                value=_fmt_metric_value(metric_name, player_metric_values.get(metric_name, 0.0), category_code),
                delta=_fmt_sigma(player_metric_deltas.get(metric_name, 0.0)),
                delta_color='normal',
            )

# Create plots.
c1, c2, c3 = st.columns(3, gap='small', vertical_alignment='top')

with c1:
    with st.container(border=True):
        if player_row is None:
            st.info('Select a player with games in the selected date range.')
        else:
            sankey_values = {
                metric_name: float(
                    _display_metric_series(player_row, metric_name, category_code, situation_code).iloc[0]
                )
                for metric_name in ['iCorsiF', 'iFenwickF', 'iShotF', 'iGoalF']
            }
            raw_corsi = float(_raw_metric_series(player_row, 'iCorsiF', situation_code).iloc[0])

            if raw_corsi <= 0:
                st.info('No shot attempts for this selection.')
            else:
                corsi = sankey_values['iCorsiF']
                fenwick = sankey_values['iFenwickF']
                shot = sankey_values['iShotF']
                goal = sankey_values['iGoalF']

                blocked = max(corsi - fenwick, 0.0)
                missed = max(fenwick - shot, 0.0)
                saved = max(shot - goal, 0.0)

                def _fmt_node_value(value: float) -> str:
                    if category_code == 'act':
                        return f'{int(round(value))}'
                    return f'{value:.1f}'

                node_names = ['Corsi', 'Blocked', 'Fenwick', 'Missed', 'Shot', 'Saved', 'Goals']
                node_values = [corsi, blocked, fenwick, missed, shot, saved, goal]
                node_labels = [f'{name} ({_fmt_node_value(value)})' for name, value in zip(node_names, node_values)]
                node_y = _volume_outcome_node_y(blocked, missed, saved, goal)

                fig = go.Figure(
                    go.Sankey(
                        arrangement='snap',
                        valueformat='.1f' if category_code != 'act' else '.0f',
                        hoverinfo='skip',
                        node=dict(
                            label=node_labels,
                            x=[0.01, 0.99, 0.33, 0.99, 0.66, 0.99, 0.99],
                            y=node_y,
                            pad=18,
                            thickness=18,
                            color=[
                                'rgba(255,80,72,0.9)',
                                'rgba(255,80,72,0.9)',
                                'rgba(255,170,40,0.9)',
                                'rgba(255,170,40,0.9)',
                                'rgba(255,225,70,0.9)',
                                'rgba(255,225,70,0.9)',
                                'rgba(90,220,120,0.9)',
                            ],
                            line=dict(width=0.5, color='rgba(255,255,255,0.22)'),
                        ),
                        link=dict(
                            source=[0, 0, 2, 2, 4, 4],
                            target=[2, 1, 4, 3, 6, 5],
                            value=[fenwick, blocked, shot, missed, goal, saved],
                            color=[
                                'rgba(255,80,72,0.1)',
                                'rgba(255,80,72,0.1)',
                                'rgba(255,170,40,0.1)',
                                'rgba(255,170,40,0.1)',
                                'rgba(255,225,70,0.1)',
                                'rgba(255,225,70,0.1)',
                            ],
                        ),
                    ),
                )

                fig.update_layout(
                    title=dict(text='Volume & Outcome', x=0.5, xanchor='center'),
                    margin=dict(l=10, r=10, t=50, b=10),
                    height=PLOT_H,
                )

                st.plotly_chart(fig, width='stretch', config={'displayModeBar': True})

with c2:
    with st.container(border=True):
        shot_scope = season_player_shots.loc[
            season_player_shots['gameId'].isin(selected_game_ids)
        ].copy()
        shot_scope = shot_scope.loc[
            shot_scope['strengthState'].isin(_sbss_strength_values(situation_code))
        ].copy()
        if player_id is not None:
            shot_scope = shot_scope.loc[shot_scope['shooterPlayerId'] == int(player_id)].copy()
        else:
            shot_scope = shot_scope.iloc[0:0].copy()

        st.plotly_chart(
            _shot_locations_rink_figure(shot_scope),
            width='stretch',
            config={'displayModeBar': True},
        )

with c3:
    with st.container(border=True):
        if eligible_players.empty:
            st.info('No scatterplot data available for this selection.')
        else:
            available_metrics = SCATTER_METRICS.copy()

            prev_x_metric = st.session_state.get('ssa_scatter_x_metric')
            prev_y_metric = st.session_state.get('ssa_scatter_y_metric')

            if prev_x_metric in available_metrics:
                x_index = available_metrics.index(prev_x_metric)
            elif 'ixGF' in available_metrics:
                x_index = available_metrics.index('ixGF')
            else:
                x_index = 0

            if prev_y_metric in available_metrics:
                y_index = available_metrics.index(prev_y_metric)
            elif 'iGoalF' in available_metrics:
                y_index = available_metrics.index('iGoalF')
            elif len(available_metrics) > 1:
                y_index = 1
            else:
                y_index = 0

            c_scx, c_scy = st.columns(2, gap='small')
            with c_scx:
                x_metric = st.selectbox(
                    'X Axis',
                    options=available_metrics,
                    index=x_index,
                    format_func=_metric_label,
                    key='ssa_scatter_x_metric',
                )
            with c_scy:
                y_metric = st.selectbox(
                    'Y Axis',
                    options=available_metrics,
                    index=y_index,
                    format_func=_metric_label,
                    key='ssa_scatter_y_metric',
                )

            scatter_source = comparison_players.copy()
            scatter_df = scatter_source[['playerId']].copy()
            scatter_df['x_val'] = _display_metric_series(scatter_source, x_metric, category_code, situation_code)
            scatter_df['y_val'] = _display_metric_series(scatter_source, y_metric, category_code, situation_code)
            scatter_df = scatter_df.dropna(subset=['playerId', 'x_val', 'y_val']).copy()

            if scatter_df.empty:
                st.info('No valid values for this selection.')
            else:
                scatter_df['name'] = scatter_df['playerId'].apply(lambda player_id: _safe_name(name_by_player_id, player_id))
                scatter_df['is_player'] = scatter_df['playerId'].eq(player_id) if player_id is not None else False

                others = scatter_df.loc[~scatter_df['is_player']].copy()
                mine = scatter_df.loc[scatter_df['is_player']].copy()

                suffix = ' (Per 60)' if category_code == 'p60' else ''
                fmt = '.1f'
                x_label = f'{_metric_label(x_metric)}{suffix}'
                y_label = f'{_metric_label(y_metric)}{suffix}'

                fig = go.Figure()

                fig.add_trace(
                    go.Scatter(
                        x=others['x_val'],
                        y=others['y_val'],
                        mode='markers',
                        marker=dict(
                            size=7,
                            opacity=0.55,
                            color='rgba(160,160,160,0.65)',
                            symbol='circle',
                        ),
                        customdata=others[['name']].to_numpy(),
                        hovertemplate=(
                            'Player: %{customdata[0]}<br>'
                            f'{x_label}: %{{x:{fmt}}}<br>'
                            f'{y_label}: %{{y:{fmt}}}'
                            '<extra></extra>'
                        ),
                        showlegend=False,
                    )
                )

                if not mine.empty:
                    fig.add_trace(
                        go.Scatter(
                            x=mine['x_val'],
                            y=mine['y_val'],
                            mode='markers',
                            marker=dict(
                                size=14,
                                opacity=1.0,
                                symbol='star',
                                color='yellow',
                            ),
                            customdata=mine[['name']].to_numpy(),
                            hovertemplate=(
                                'Player: %{customdata[0]}<br>'
                                f'{x_label}: %{{x:{fmt}}}<br>'
                                f'{y_label}: %{{y:{fmt}}}'
                                '<extra></extra>'
                            ),
                            showlegend=False,
                        )
                    )

                fig.update_layout(
                    title=dict(text=f'Metrics vs. {player_position_group}{suffix}', x=0.5, xanchor='center'),
                    margin=dict(l=10, r=10, t=45, b=10),
                    xaxis=dict(title=f'{_metric_label(x_metric)}{suffix}'),
                    yaxis=dict(title=f'{_metric_label(y_metric)}{suffix}'),
                    height=SCATTER_PLOT_H,
                )

                fig.update_xaxes(fixedrange=True)
                fig.update_yaxes(fixedrange=True)

                st.plotly_chart(fig, width='stretch', config={'displayModeBar': True})
