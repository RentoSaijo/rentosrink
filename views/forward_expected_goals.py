# Import libraries.
import datetime as dt
import os

import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

from ranking_card import build_ranking_card_png
from utils import file_data_url, load_biographies, load_games, load_gbgs_skater_advanced, load_gbgs_skater_basic, load_teams


SEASON_START = 20112012
SEASON_END = 20252026
POSITION_GROUP = 'Forwards'
TABLE_HEIGHT = int(35 * 4.5)
CHART_HEIGHT = 500
CHART_COUNT = 6
POSITIVE_BAR_COLOR = 'rgba(90,220,120,0.9)'
NEGATIVE_BAR_COLOR = 'rgba(255,80,72,0.9)'
NEUTRAL_BAR_COLOR = 'rgba(209,229,240,0.85)'
LOGO_PATH_TEMPLATE = 'assets/logos/{team_id}.png'
LABEL_PAD = '\u00A0' * 4

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
STATISTICS = ['iGF', 'ixGF', 'iGFAx', 'oGF', 'oxGF', 'oxGFAx', 'oxGA', 'oxG%']
DEFAULT_STATISTIC = 'oxG%'
LOWER_IS_BETTER = {'oxGA'}


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
    bio = bio.loc[bio['positionCode'] != 'G'].copy()
    bio['positionGroup'] = bio['positionCode'].apply(lambda value: 'Defensemen' if value == 'D' else 'Forwards')
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


def _prepare_teams(df_in: pd.DataFrame) -> pd.DataFrame:
    teams = df_in.copy()
    teams['teamId'] = pd.to_numeric(teams.get('teamId'), errors='coerce')
    teams = teams.dropna(subset=['teamId']).copy()
    teams['teamId'] = teams['teamId'].astype('int64')
    teams['teamTriCode'] = teams.get('teamTriCode', teams.get('teamTriCodeRaw', '')).astype(str).str.strip().str.upper()
    teams = teams.loc[teams['teamTriCode'] != ''].copy()
    return teams[['teamId', 'teamTriCode']].drop_duplicates().sort_values('teamTriCode').reset_index(drop=True)


def _player_team_map(df_in: pd.DataFrame) -> pd.DataFrame:
    if df_in.empty or 'teamId' not in df_in.columns:
        return pd.DataFrame(columns=['playerId', 'teamId'])
    team_rows = df_in[['playerId', 'teamId', 'gameId']].copy()
    team_rows['playerId'] = pd.to_numeric(team_rows['playerId'], errors='coerce')
    team_rows['teamId'] = pd.to_numeric(team_rows['teamId'], errors='coerce')
    team_rows['gameId'] = pd.to_numeric(team_rows['gameId'], errors='coerce')
    team_rows = team_rows.dropna(subset=['playerId', 'teamId', 'gameId']).copy()
    if team_rows.empty:
        return pd.DataFrame(columns=['playerId', 'teamId'])
    team_rows['playerId'] = team_rows['playerId'].astype('int64')
    team_rows['teamId'] = team_rows['teamId'].astype('int64')
    team_rows['gameId'] = team_rows['gameId'].astype('int64')
    counts = (
        team_rows.groupby(['playerId', 'teamId'], as_index=False)
        .agg(games=('gameId', 'nunique'), latestGameId=('gameId', 'max'))
        .sort_values(['playerId', 'games', 'latestGameId', 'teamId'], ascending=[True, False, False, True])
    )
    return counts.drop_duplicates('playerId')[['playerId', 'teamId']].reset_index(drop=True)


@st.cache_data
def _load_page_data(season_id: str) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    games = _prepare_games(load_games())
    games = games.loc[games['seasonId'] == int(season_id)].copy()

    basic = _prepare_basic(load_gbgs_skater_basic(season_id))
    advanced = _prepare_advanced(load_gbgs_skater_advanced(season_id))
    skater_games = basic.merge(advanced, on=['playerId', 'gameId'], how='left', suffixes=('', '_adv'))

    game_meta = games[['gameId', 'gameDate', 'gameTypeId', 'seasonId']].drop_duplicates()
    skater_games = skater_games.merge(game_meta, on='gameId', how='left')
    bio = _prepare_biographies(load_biographies())

    return games, skater_games, bio


def _strength_suffixes(situation_code: str) -> list[str]:
    return GBGS_STRENGTHS.get(situation_code, [situation_code])


def _sum_metric(df_in: pd.DataFrame, base_metric: str, situation_code: str) -> pd.Series:
    cols = [f'{base_metric}_{suffix}' for suffix in _strength_suffixes(situation_code) if f'{base_metric}_{suffix}' in df_in.columns]
    if not cols:
        return pd.Series(0.0, index=df_in.index, dtype=float)
    return df_in[cols].fillna(0.0).sum(axis=1).astype(float)


def _minutes_series(df_in: pd.DataFrame, situation_code: str) -> pd.Series:
    return _sum_metric(df_in, 'mP', situation_code)


def _display_series(df_in: pd.DataFrame, base_metric: str, category_code: str, situation_code: str) -> pd.Series:
    values = _sum_metric(df_in, base_metric, situation_code)
    if category_code != 'p60':
        return values.fillna(0.0)
    minutes = _minutes_series(df_in, situation_code)
    scaled = values.div(minutes.where(minutes > 0)).mul(60.0)
    return scaled.replace([float('inf'), float('-inf')], 0.0).fillna(0.0)


def _aggregate_players(df_in: pd.DataFrame) -> pd.DataFrame:
    if df_in.empty:
        return pd.DataFrame(columns=['playerId'])
    numeric_cols = [col for col in df_in.columns if col not in {'playerId', 'gameId', 'gameDate', 'gameTypeId', 'seasonId', 'teamId', 'teamId_adv'}]
    return df_in.groupby('playerId', as_index=False)[numeric_cols].sum()


def _date_tuple(value) -> tuple[dt.date | None, dt.date | None]:
    if isinstance(value, tuple) and len(value) == 2:
        return value[0], value[1]
    if isinstance(value, list) and len(value) == 2:
        return value[0], value[1]
    if isinstance(value, dt.date):
        return value, value
    return None, None


def _clamp_date_range(value, min_date: dt.date, max_date: dt.date) -> tuple[dt.date, dt.date]:
    start, end = _date_tuple(value)
    if start is None or end is None:
        return min_date, max_date
    start = min(max(start, min_date), max_date)
    end = min(max(end, min_date), max_date)
    if start > end:
        start, end = end, start
    return start, end


def _fmt_value(value: float, category_code: str) -> float | int:
    if pd.isna(value):
        return 0
    if category_code == 'act':
        return int(round(float(value)))
    return round(float(value), 1)


def _stat_ascending(statistic: str) -> bool:
    return statistic in LOWER_IS_BETTER


def _slider_max(series: pd.Series) -> int:
    if series.empty:
        return 0
    return max(0, int(pd.to_numeric(series, errors='coerce').fillna(0).max()))


def _slider_default_half_max(series: pd.Series) -> int:
    return _slider_max(series) // 2


def _nonzero_slider_values(series: pd.Series) -> pd.Series:
    if series.empty:
        return pd.Series(dtype=float)
    values = pd.to_numeric(series, errors='coerce').fillna(0.0)
    return values.loc[values > 0]


def _slider_default_nonzero_average(series: pd.Series) -> int:
    values = _nonzero_slider_values(series)
    if values.empty:
        return 0
    return max(0, int(round(float(values.mean()))))


def _minutes_slider_default(series: pd.Series, situation_code: str) -> int:
    return _slider_default_nonzero_average(series)


def _threshold_slider(label: str, max_value: int, default_value: int, key: str) -> int:
    if max_value <= 0:
        st.slider(label, min_value=0, max_value=1, value=0, step=1, disabled=True, key=key)
        return 0
    return st.slider(
        label,
        min_value=0,
        max_value=max_value,
        value=min(default_value, max_value),
        step=1,
        key=key,
    )


def _midpoint_axis_range(
    series: pd.Series,
    midpoint: float,
    padding: float,
    lower_bound: float | None = None,
    upper_bound: float | None = None,
) -> tuple[float, float]:
    values = pd.to_numeric(series, errors='coerce').dropna()
    if values.empty:
        lower = midpoint - padding
        upper = midpoint + padding
    else:
        lower_delta = float((midpoint - values.loc[values < midpoint]).max()) if (values < midpoint).any() else 0.0
        upper_delta = float((values.loc[values > midpoint] - midpoint).max()) if (values > midpoint).any() else 0.0

        if lower_delta > 0 and upper_delta > 0:
            span = max(lower_delta, upper_delta) + padding
            lower = midpoint - span
            upper = midpoint + span
        elif upper_delta > 0:
            lower = midpoint - padding
            upper = midpoint + upper_delta + padding
        elif lower_delta > 0:
            lower = midpoint - lower_delta - padding
            upper = midpoint + padding
        else:
            lower = midpoint - padding
            upper = midpoint + padding

    if lower_bound is not None:
        lower = max(lower_bound, lower)
    if upper_bound is not None:
        upper = min(upper_bound, upper)
    return float(lower), float(upper)


def _team_logo_data_url(team_id) -> str | None:
    if pd.isna(team_id):
        return None
    logo_path = LOGO_PATH_TEMPLATE.format(team_id=int(team_id))
    if not os.path.exists(logo_path):
        return None
    return file_data_url(logo_path)


def _chart_player_label(name: str) -> str:
    return f'{str(name)}{LABEL_PAD}'


def _add_team_logos(fig: go.Figure, chart_data: pd.DataFrame) -> None:
    if 'teamId' not in chart_data.columns:
        return
    for _, row in chart_data.iterrows():
        logo_source = _team_logo_data_url(row['teamId'])
        if logo_source is None:
            continue
        fig.add_layout_image(
            dict(
                source=logo_source,
                xref='paper',
                yref='y',
                x=-0.02,
                y=row['displayLabel'],
                sizex=0.06,
                sizey=0.52,
                xanchor='center',
                yanchor='middle',
                sizing='contain',
                layer='above',
            )
        )


def _render_bar_chart(df_in: pd.DataFrame, statistic: str, title: str, x_range: tuple[float, float] | None = None) -> None:
    chart_cols = ['playerMenuName', statistic]
    if 'teamId' in df_in.columns:
        chart_cols.append('teamId')
    chart_data = df_in[chart_cols].dropna(subset=['playerMenuName', statistic]).copy().iloc[::-1].reset_index(drop=True)
    if chart_data.empty:
        st.info(f'No data available for {title.lower()}.')
        return
    chart_data['displayLabel'] = chart_data['playerMenuName'].astype(str).map(_chart_player_label)

    labels = {'playerMenuName': '', statistic: statistic}
    if statistic == 'oxG%':
        values = pd.to_numeric(chart_data[statistic], errors='coerce').fillna(50.0)
        colors = [
            POSITIVE_BAR_COLOR if value > 50.0 else NEGATIVE_BAR_COLOR if value < 50.0 else NEUTRAL_BAR_COLOR
            for value in values
        ]
        fig = go.Figure(
            go.Bar(
                x=(values - 50.0).tolist(),
                base=[50.0] * len(chart_data),
                y=chart_data['displayLabel'].tolist(),
                orientation='h',
                marker=dict(color=colors),
                customdata=chart_data[['playerMenuName', statistic]].to_numpy(),
                text=[f'{value:.1f}%' for value in values],
                textposition='outside',
                hovertemplate='%{customdata[0]}<br>%{customdata[1]:.1f}%<extra></extra>',
            )
        )
        fig.add_vline(x=50.0, line_dash='dash', line_color='#4a4a4a', line_width=2)
        fig.update_xaxes(title_text=statistic, ticksuffix='%')
    else:
        fig = px.bar(
            chart_data,
            x=statistic,
            y='displayLabel',
            orientation='h',
            labels=labels,
            title=title,
            color_discrete_sequence=['#4c78a8'],
            text=statistic,
            custom_data=['playerMenuName'],
        )
        value_format = '%{x:.1f}' if statistic != 'GA' else '%{x:.0f}'
        fig.update_traces(hovertemplate=f'%{{customdata[0]}}<br>{value_format}<extra></extra>', texttemplate=value_format, textposition='outside')
    fig.update_layout(
        height=CHART_HEIGHT,
        margin=dict(l=8, r=8, t=48, b=8),
        showlegend=False,
        title_x=0.5,
        title_text=title,
    )
    if x_range is not None:
        fig.update_xaxes(range=list(x_range))
    fig.update_xaxes(fixedrange=True)
    fig.update_yaxes(fixedrange=True, automargin=True)
    fig.update_traces(cliponaxis=False)
    _add_team_logos(fig, chart_data)
    st.plotly_chart(fig, width='stretch', config={'displayModeBar': True})


season_options = [_season_label(season_id) for season_id in reversed(_season_ids(SEASON_START, SEASON_END))]
season_lookup = {label: label.replace('-', '') for label in season_options}

c_season, c_game, c_date, c_sit, c_cat, c_stat = st.columns(6, gap='small', vertical_alignment='top')

with c_season:
    season_label = st.selectbox('Season', season_options, index=0, key='feg_season_label')
season_id = season_lookup[season_label]

season_games, season_player_games, bio = _load_page_data(season_id)
teams = _prepare_teams(load_teams())

available_game_types = [label for label, game_type_id in GAME_TYPES.items() if game_type_id in set(season_games['gameTypeId'].unique().tolist())]
if not available_game_types:
    st.info('No games available for this season.')
    st.stop()

prev_game_type = st.session_state.get('feg_game_type_label')
game_type_index = available_game_types.index(prev_game_type) if prev_game_type in available_game_types else (available_game_types.index('Regular Season') if 'Regular Season' in available_game_types else 0)

with c_game:
    if st.session_state.get('feg_game_type_label') not in available_game_types:
        st.session_state['feg_game_type_label'] = available_game_types[game_type_index]
    game_type_label = st.selectbox('Game Type', available_game_types, index=game_type_index, key='feg_game_type_label')

game_type_id = GAME_TYPES[game_type_label]
game_scope = season_games.loc[season_games['gameTypeId'] == game_type_id].copy().sort_values(['gameDate', 'gameId'])

date_min = game_scope['gameDate'].min()
date_max = game_scope['gameDate'].max()
date_range_key = 'feg_date_range_saved'
date_range_widget_key = 'feg_date_range_widget'
date_context_key = 'feg_date_range_context'

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
            st.session_state[date_range_key] = _clamp_date_range(st.session_state.get(date_range_key), min_date, max_date)

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
    situation_label = st.selectbox('Situation', list(SITUATIONS.keys()), index=0, key='feg_situation_label')
with c_cat:
    category_label = st.selectbox('Category', list(CATEGORIES.keys()), index=0, key='feg_category_label')
with c_stat:
    statistic_label = st.selectbox('Statistic', STATISTICS, index=STATISTICS.index(DEFAULT_STATISTIC), key='feg_statistic_label')

situation_code = SITUATIONS[situation_label]
category_code = CATEGORIES[category_label]

if selected_start_date is None or selected_end_date is None:
    selected_game_ids = set()
else:
    selected_game_ids = set(
        game_scope.loc[
            game_scope['gameDate'].between(pd.Timestamp(selected_start_date), pd.Timestamp(selected_end_date)),
            'gameId',
        ].astype('int64').tolist()
    )

filtered_games = season_player_games.loc[season_player_games['gameId'].isin(selected_game_ids)].copy()
player_totals = _aggregate_players(filtered_games)
player_totals = player_totals.merge(
    bio[['playerId', 'playerMenuName', 'positionGroup']],
    on='playerId',
    how='left',
)
player_totals['playerMenuName'] = player_totals['playerMenuName'].fillna(player_totals['playerId'].astype(str))
player_totals['positionGroup'] = player_totals['positionGroup'].fillna('Forwards')
player_totals = player_totals.loc[player_totals['positionGroup'] == POSITION_GROUP].copy()

available_team_ids = (
    pd.to_numeric(season_player_games['teamId'], errors='coerce')
    .dropna()
    .astype('int64')
    .unique()
    .tolist()
    if 'teamId' in season_player_games.columns else []
)
available_teams = teams.loc[teams['teamId'].isin(available_team_ids)].copy().sort_values('teamTriCode')
team_options = available_teams['teamTriCode'].tolist()
filter_team_col, filter_minutes_col, filter_download_col, filter_card_col = st.columns([1, 1, 0.9, 0.9], gap='small', vertical_alignment='bottom')
team_filter_key = 'feg_team_filter'
valid_selected_teams = [team for team in st.session_state.get(team_filter_key, []) if team in team_options]
if team_filter_key in st.session_state and st.session_state.get(team_filter_key) != valid_selected_teams:
    st.session_state[team_filter_key] = valid_selected_teams
with filter_team_col:
    selected_teams = st.multiselect('Teams', options=team_options, placeholder='Select teams; default is all.', key=team_filter_key)
selected_team_ids = set(available_teams.loc[available_teams['teamTriCode'].isin(selected_teams), 'teamId'].astype('int64').tolist())
if selected_team_ids:
    filtered_games = filtered_games.loc[pd.to_numeric(filtered_games['teamId'], errors='coerce').isin(selected_team_ids)].copy()

player_totals = _aggregate_players(filtered_games)
games_played = filtered_games.groupby('playerId')['gameId'].nunique().rename('gamesPlayed').reset_index()
player_teams = _player_team_map(filtered_games)
player_totals = player_totals.merge(games_played, on='playerId', how='left')
player_totals = player_totals.merge(player_teams, on='playerId', how='left')
player_totals['gamesPlayed'] = pd.to_numeric(player_totals['gamesPlayed'], errors='coerce').fillna(0).astype(int)
player_totals['minutes'] = _minutes_series(player_totals, situation_code)

player_totals = player_totals.merge(
    bio[['playerId', 'playerMenuName', 'positionGroup']],
    on='playerId',
    how='left',
)
player_totals['playerMenuName'] = player_totals['playerMenuName'].fillna(player_totals['playerId'].astype(str))
player_totals['positionGroup'] = player_totals['positionGroup'].fillna('Forwards')
player_totals = player_totals.loc[player_totals['positionGroup'] == POSITION_GROUP].copy()

minutes_slider_max = _slider_max(player_totals['minutes']) if 'minutes' in player_totals else 0

with filter_minutes_col:
    min_minutes_played = _threshold_slider(
        'Minimum Minutes Played',
        minutes_slider_max,
        _minutes_slider_default(player_totals['minutes'], situation_code),
        'feg_min_minutes_played',
    )

player_totals = player_totals.loc[player_totals['minutes'] >= float(min_minutes_played)].copy()

player_totals['iGF'] = _display_series(player_totals, 'iGF', category_code, situation_code)
player_totals['ixGF'] = _display_series(player_totals, 'ixGF', category_code, situation_code)
player_totals['iGFAx'] = player_totals['iGF'] - player_totals['ixGF']
player_totals['oGF'] = _display_series(player_totals, 'oGF', category_code, situation_code)
player_totals['oxGF'] = _display_series(player_totals, 'oxGF', category_code, situation_code)
player_totals['oxGFAx'] = player_totals['oGF'] - player_totals['oxGF']
player_totals['oxGA'] = _display_series(player_totals, 'oxGA', category_code, situation_code)
player_totals['oxG%'] = (
    player_totals['oxGF']
    .div((player_totals['oxGF'] + player_totals['oxGA']).where((player_totals['oxGF'] + player_totals['oxGA']) > 0))
    .mul(100.0)
    .replace([float('inf'), float('-inf')], 0.0)
    .fillna(0.0)
)

display_cols = ['playerMenuName', *STATISTICS]
ranked_players = player_totals[['playerMenuName', 'teamId', *STATISTICS]].copy()
ascending = _stat_ascending(statistic_label)
ranked_players = ranked_players.sort_values([statistic_label, 'playerMenuName'], ascending=[ascending, True]).reset_index(drop=True)

top_5 = ranked_players.head(CHART_COUNT).copy()
bottom_5 = ranked_players.sort_values([statistic_label, 'playerMenuName'], ascending=[not ascending, True]).head(CHART_COUNT)
top_chart_range = _midpoint_axis_range(top_5.get('oxG%', pd.Series(dtype=float)), midpoint=50.0, padding=5.0, lower_bound=0.0, upper_bound=100.0)
bottom_chart_range = _midpoint_axis_range(bottom_5.get('oxG%', pd.Series(dtype=float)), midpoint=50.0, padding=5.0, lower_bound=0.0, upper_bound=100.0)

table = ranked_players.drop(columns=['teamId'], errors='ignore').copy()
for col in ['iGF', 'ixGF', 'iGFAx', 'oGF', 'oxGF', 'oxGFAx', 'oxGA']:
    table[col] = table[col].map(lambda value: _fmt_value(value, category_code))
table['oxG%'] = table['oxG%'].map(lambda value: round(float(value), 1) if pd.notna(value) else 0.0)

card_png_bytes = None
card_error = None
if not ranked_players.empty:
    try:
        card_png_bytes = build_ranking_card_png(
            ranked_df=ranked_players,
            statistic=statistic_label,
            position_label='Forwards',
            season_label=season_label,
            game_type_label=game_type_label,
            situation_label=situation_label,
            selected_teams=selected_teams,
            minimum_filter_label=f'{int(min_minutes_played)} Min. Minutes Played',
            lower_is_better=ascending,
            range_start_date=selected_start_date,
            range_end_date=selected_end_date,
        )
    except Exception as exc:
        card_error = str(exc)

with filter_download_col:
    st.download_button(
        'Download Full List',
        data=table.to_csv(index=False).encode('utf-8'),
        file_name=f'forward_expected_goals_{season_id}_{game_type_id}_{situation_code}_{category_code}.csv',
        mime='text/csv',
        icon=':material/download:',
        width='stretch',
        disabled=table.empty,
        key='feg_download_full_list',
    )
with filter_card_col:
    st.download_button(
        'Download Card',
        data=card_png_bytes or b'',
        file_name=f'forward_expected_goals_card_{season_id}_{game_type_id}_{situation_code}_{category_code}_{statistic_label.lower().replace("%", "pct")}.png',
        mime='image/png',
        icon=':material/download:',
        width='stretch',
        disabled=card_png_bytes is None,
        key='feg_download_card',
    )
    if card_error:
        st.caption(f'Card export unavailable: {card_error}')

if table.empty:
    st.info('No forwards available for this selection.')
else:
    chart_left, chart_right = st.columns(2, gap='small')
    with chart_left:
        with st.container(border=True):
            _render_bar_chart(top_5, statistic_label, f'Top {CHART_COUNT} by {statistic_label}', x_range=top_chart_range if statistic_label == 'oxG%' else None)
    with chart_right:
        with st.container(border=True):
            _render_bar_chart(bottom_5, statistic_label, f'Bottom {CHART_COUNT} by {statistic_label}', x_range=bottom_chart_range if statistic_label == 'oxG%' else None)
