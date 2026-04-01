# Import libraries.
import datetime as dt
import os

import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

from ranking_card import build_ranking_card_png
from utils import file_data_url, load_games, load_gbgs_team_advanced, load_gbgs_team_basic, load_teams


SEASON_START = 20112012
SEASON_END = 20252026
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
STATISTICS = ['GF', 'xGF', 'xGFAx', 'xGA', 'xGF%', 'GA', 'GSAx']
DEFAULT_STATISTIC = 'xGF%'
LOWER_IS_BETTER = {'xGA', 'GA'}


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


def _prepare_basic(df_in: pd.DataFrame) -> pd.DataFrame:
    basic = df_in.copy()
    basic['teamId'] = pd.to_numeric(basic.get('teamId'), errors='coerce')
    basic['gameId'] = pd.to_numeric(basic.get('gameId'), errors='coerce')
    basic = basic.dropna(subset=['teamId', 'gameId']).copy()
    basic['teamId'] = basic['teamId'].astype('int64')
    basic['gameId'] = basic['gameId'].astype('int64')
    basic = _coerce_numeric_columns(basic, exclude={'teamId', 'gameId', 'gameDate'})
    basic = basic.drop(columns=['gameDate'], errors='ignore')
    return basic


def _prepare_advanced(df_in: pd.DataFrame) -> pd.DataFrame:
    advanced = df_in.copy()
    advanced['teamId'] = pd.to_numeric(advanced.get('teamId'), errors='coerce')
    advanced['gameId'] = pd.to_numeric(advanced.get('gameId'), errors='coerce')
    advanced = advanced.dropna(subset=['teamId', 'gameId']).copy()
    advanced['teamId'] = advanced['teamId'].astype('int64')
    advanced['gameId'] = advanced['gameId'].astype('int64')
    advanced = _coerce_numeric_columns(advanced, exclude={'teamId', 'gameId'})
    return advanced


def _prepare_teams(df_in: pd.DataFrame) -> pd.DataFrame:
    teams = df_in.copy()
    teams['teamId'] = pd.to_numeric(teams.get('teamId'), errors='coerce')
    teams = teams.dropna(subset=['teamId']).copy()
    teams['teamId'] = teams['teamId'].astype('int64')
    teams['teamTriCode'] = teams.get('teamTriCode', teams.get('teamTriCodeRaw', '')).astype(str).str.strip().str.upper()
    teams['teamFullName'] = teams.get('teamFullName', '').astype(str).str.strip()
    teams = teams.loc[teams['teamTriCode'] != ''].copy()
    return teams[['teamId', 'teamTriCode', 'teamFullName']].drop_duplicates().sort_values('teamTriCode').reset_index(drop=True)


TWO_WORD_TEAM_SUFFIXES = {
    'Blue Jackets',
    'Golden Knights',
    'Maple Leafs',
    'Red Wings',
}


def _compact_team_name(full_name: str) -> str:
    parts = [part for part in str(full_name).strip().split() if part]
    if len(parts) <= 1:
        return str(full_name).strip()

    suffix_size = 1
    if len(parts) >= 3 and ' '.join(parts[-2:]) in TWO_WORD_TEAM_SUFFIXES:
        suffix_size = 2

    return ' '.join(parts[-suffix_size:])


@st.cache_data
def _load_page_data(season_id: str) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    games = _prepare_games(load_games())
    games = games.loc[games['seasonId'] == int(season_id)].copy()

    basic = _prepare_basic(load_gbgs_team_basic(season_id))
    advanced = _prepare_advanced(load_gbgs_team_advanced(season_id))
    team_games = basic.merge(advanced, on=['teamId', 'gameId'], how='left', suffixes=('', '_adv'))

    game_meta = games[['gameId', 'gameDate', 'gameTypeId', 'seasonId']].drop_duplicates()
    team_games = team_games.merge(game_meta, on='gameId', how='left')
    teams = _prepare_teams(load_teams())

    return games, team_games, teams


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


def _aggregate_teams(df_in: pd.DataFrame) -> pd.DataFrame:
    if df_in.empty:
        return pd.DataFrame(columns=['teamId'])
    numeric_cols = [col for col in df_in.columns if col not in {'teamId', 'gameId', 'gameDate', 'gameTypeId', 'seasonId'}]
    return df_in.groupby('teamId', as_index=False)[numeric_cols].sum()


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


def _chart_team_label(name: str) -> str:
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
    label_col = 'chartName' if 'chartName' in df_in.columns else 'playerMenuName'
    chart_cols = [label_col, statistic, 'teamId']
    chart_data = df_in[chart_cols].dropna(subset=[label_col, statistic]).copy().iloc[::-1].reset_index(drop=True)
    if chart_data.empty:
        st.info(f'No data available for {title.lower()}.')
        return
    chart_data['displayLabel'] = chart_data[label_col].astype(str).map(_chart_team_label)

    labels = {label_col: '', statistic: statistic}
    if statistic == 'xGF%':
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
                customdata=chart_data[[label_col, statistic]].to_numpy(),
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
            custom_data=[label_col],
        )
        fig.update_traces(hovertemplate='%{customdata[0]}<br>%{x:.1f}<extra></extra>', texttemplate='%{x:.1f}', textposition='outside')
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
    season_label = st.selectbox('Season', season_options, index=0, key='teg_season_label')
season_id = season_lookup[season_label]

season_games, season_team_games, teams = _load_page_data(season_id)

available_game_types = [label for label, game_type_id in GAME_TYPES.items() if game_type_id in set(season_games['gameTypeId'].unique().tolist())]
if not available_game_types:
    st.info('No games available for this season.')
    st.stop()

prev_game_type = st.session_state.get('teg_game_type_label')
game_type_index = available_game_types.index(prev_game_type) if prev_game_type in available_game_types else (available_game_types.index('Regular Season') if 'Regular Season' in available_game_types else 0)

with c_game:
    if st.session_state.get('teg_game_type_label') not in available_game_types:
        st.session_state['teg_game_type_label'] = available_game_types[game_type_index]
    game_type_label = st.selectbox('Game Type', available_game_types, index=game_type_index, key='teg_game_type_label')

game_type_id = GAME_TYPES[game_type_label]
game_scope = season_games.loc[season_games['gameTypeId'] == game_type_id].copy().sort_values(['gameDate', 'gameId'])

date_min = game_scope['gameDate'].min()
date_max = game_scope['gameDate'].max()
date_range_widget_key = f'teg_date_range_widget_{season_id}_{game_type_id}'

with c_date:
    if game_scope.empty or pd.isna(date_min) or pd.isna(date_max):
        st.text_input('Date Range', value='No games available', disabled=True)
        selected_start_date = None
        selected_end_date = None
    else:
        min_date = date_min.date()
        max_date = date_max.date()
        selected_dates = st.date_input(
            'Date Range',
            value=(min_date, max_date),
            min_value=min_date,
            max_value=max_date,
            key=date_range_widget_key,
        )
        selected_start_date, selected_end_date = _clamp_date_range(selected_dates, min_date, max_date)

with c_sit:
    situation_label = st.selectbox('Situation', list(SITUATIONS.keys()), index=0, key='teg_situation_label')
with c_cat:
    category_label = st.selectbox('Category', list(CATEGORIES.keys()), index=0, key='teg_category_label')
with c_stat:
    statistic_label = st.selectbox('Statistic', STATISTICS, index=STATISTICS.index(DEFAULT_STATISTIC), key='teg_statistic_label')

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

filtered_games = season_team_games.loc[season_team_games['gameId'].isin(selected_game_ids)].copy()
available_team_ids = (
    pd.to_numeric(filtered_games['teamId'], errors='coerce')
    .dropna()
    .astype('int64')
    .unique()
    .tolist()
    if 'teamId' in filtered_games.columns else []
)
available_teams = teams.loc[teams['teamId'].isin(available_team_ids)].copy().sort_values('teamTriCode')
team_options = available_teams['teamTriCode'].tolist()
filter_team_col, filter_download_col, filter_card_col = st.columns([1, 0.9, 0.9], gap='small', vertical_alignment='bottom')
team_filter_key = 'teg_team_filter'
valid_selected_teams = [team for team in st.session_state.get(team_filter_key, []) if team in team_options]
if team_filter_key in st.session_state and st.session_state.get(team_filter_key) != valid_selected_teams:
    st.session_state[team_filter_key] = valid_selected_teams
with filter_team_col:
    selected_teams = st.multiselect('Teams', options=team_options, placeholder='Select teams; default is all.', key=team_filter_key)
selected_team_ids = set(available_teams.loc[available_teams['teamTriCode'].isin(selected_teams), 'teamId'].astype('int64').tolist())
if selected_team_ids:
    filtered_games = filtered_games.loc[pd.to_numeric(filtered_games['teamId'], errors='coerce').isin(selected_team_ids)].copy()

team_totals = _aggregate_teams(filtered_games)
team_totals['minutes'] = _minutes_series(team_totals, situation_code)
team_totals = team_totals.loc[team_totals['minutes'] > 0].copy()
team_totals = team_totals.merge(teams, on='teamId', how='left', suffixes=('', '_team'))

if 'teamTriCode' not in team_totals.columns:
    for col in ('teamTriCode_team', 'teamTriCode_x', 'teamTriCode_y'):
        if col in team_totals.columns:
            team_totals['teamTriCode'] = team_totals[col]
            break
if 'teamFullName' not in team_totals.columns:
    for col in ('teamFullName_team', 'teamFullName_x', 'teamFullName_y'):
        if col in team_totals.columns:
            team_totals['teamFullName'] = team_totals[col]
            break

if 'teamTriCode' not in team_totals.columns:
    team_totals['teamTriCode'] = pd.Series(pd.NA, index=team_totals.index, dtype='object')
if 'teamFullName' not in team_totals.columns:
    team_totals['teamFullName'] = pd.Series(pd.NA, index=team_totals.index, dtype='object')

team_totals['teamTriCode'] = team_totals['teamTriCode'].fillna(team_totals['teamId'].astype(str))
team_totals['teamFullName'] = team_totals['teamFullName'].fillna(team_totals['teamTriCode'])

team_totals['GF'] = _display_series(team_totals, 'GF', category_code, situation_code)
team_totals['xGF'] = _display_series(team_totals, 'xGF', category_code, situation_code)
team_totals['xGFAx'] = team_totals['GF'] - team_totals['xGF']
team_totals['GA'] = _display_series(team_totals, 'GA', category_code, situation_code)
team_totals['xGA'] = _display_series(team_totals, 'xGA', category_code, situation_code)
team_totals['GSAx'] = team_totals['xGA'] - team_totals['GA']
team_totals['xGF%'] = (
    team_totals['xGF']
    .div((team_totals['xGF'] + team_totals['xGA']).where((team_totals['xGF'] + team_totals['xGA']) > 0))
    .mul(100.0)
    .replace([float('inf'), float('-inf')], 0.0)
    .fillna(0.0)
)

ranked_teams = team_totals[['teamId', 'teamTriCode', 'teamFullName', *STATISTICS]].copy()
ranked_teams['playerFullName'] = ranked_teams['teamFullName']
ranked_teams['chartName'] = ranked_teams['teamFullName']
ranked_teams['playerMenuName'] = ranked_teams['teamFullName'].map(_compact_team_name)
ranked_teams['cardName'] = ranked_teams['teamFullName'].map(_compact_team_name)
ranked_teams = ranked_teams[['teamId', 'teamTriCode', 'playerFullName', 'playerMenuName', 'chartName', 'cardName', *STATISTICS]]
ascending = _stat_ascending(statistic_label)
ranked_teams = ranked_teams.sort_values([statistic_label, 'chartName'], ascending=[ascending, True]).reset_index(drop=True)

top_5 = ranked_teams.head(CHART_COUNT).copy()
bottom_5 = ranked_teams.sort_values([statistic_label, 'playerMenuName'], ascending=[not ascending, True]).head(CHART_COUNT)
top_chart_range = _midpoint_axis_range(top_5.get('xGF%', pd.Series(dtype=float)), midpoint=50.0, padding=5.0, lower_bound=0.0, upper_bound=100.0)
bottom_chart_range = _midpoint_axis_range(bottom_5.get('xGF%', pd.Series(dtype=float)), midpoint=50.0, padding=5.0, lower_bound=0.0, upper_bound=100.0)

table = ranked_teams.rename(columns={'chartName': 'teamFullName'}).drop(columns=['teamId', 'playerFullName', 'playerMenuName', 'cardName'], errors='ignore').copy()
for col in ['GF', 'xGF', 'xGFAx', 'xGA', 'GA', 'GSAx']:
    table[col] = table[col].map(lambda value: _fmt_value(value, category_code))
table['xGF%'] = table['xGF%'].map(lambda value: round(float(value), 1) if pd.notna(value) else 0.0)

card_png_bytes = None
card_error = None
if not ranked_teams.empty:
    try:
        card_png_bytes = build_ranking_card_png(
            ranked_df=ranked_teams,
            statistic=statistic_label,
            position_label='Teams',
            season_label=season_label,
            game_type_label=game_type_label,
            situation_label=situation_label,
            selected_teams=selected_teams,
            minimum_filter_label=None,
            lower_is_better=ascending,
            range_start_date=selected_start_date,
            range_end_date=selected_end_date,
            title_statistic_label=(f'{statistic_label} per 60' if category_code == 'p60' else statistic_label),
        )
    except Exception as exc:
        card_error = str(exc)

with filter_download_col:
    st.download_button(
        'Download Full List',
        data=table.to_csv(index=False).encode('utf-8'),
        file_name=f'team_expected_goals_{season_id}_{game_type_id}_{situation_code}_{category_code}.csv',
        mime='text/csv',
        icon=':material/download:',
        width='stretch',
        disabled=table.empty,
        key='teg_download_full_list',
    )
with filter_card_col:
    st.download_button(
        'Download Card',
        data=card_png_bytes or b'',
        file_name=f'team_expected_goals_card_{season_id}_{game_type_id}_{situation_code}_{category_code}_{statistic_label.lower().replace("%", "pct")}.png',
        mime='image/png',
        icon=':material/download:',
        width='stretch',
        disabled=card_png_bytes is None,
        key='teg_download_card',
    )
    if card_error:
        st.caption(f'Card export unavailable: {card_error}')

if table.empty:
    st.info('No teams available for this selection.')
else:
    chart_left, chart_right = st.columns(2, gap='small')
    with chart_left:
        with st.container(border=True):
            _render_bar_chart(top_5, statistic_label, f'Top {CHART_COUNT} by {statistic_label}', x_range=top_chart_range if statistic_label == 'xGF%' else None)
    with chart_right:
        with st.container(border=True):
            _render_bar_chart(bottom_5, statistic_label, f'Bottom {CHART_COUNT} by {statistic_label}', x_range=bottom_chart_range if statistic_label == 'xGF%' else None)
