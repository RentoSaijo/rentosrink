# Import libraries.
import datetime as dt

import pandas as pd
import streamlit as st

from utils import load_biographies, load_games, load_gbgs_skater_advanced, load_gbgs_skater_basic


SEASON_START = 20112012
SEASON_END = 20252026
POSITION_GROUP = 'Forwards'
TABLE_HEIGHT = 35 * 16

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
    numeric_cols = [col for col in df_in.columns if col not in {'playerId', 'gameId', 'gameDate', 'gameTypeId', 'seasonId'}]
    return df_in.groupby('playerId', as_index=False)[numeric_cols].sum()


def _filter_bottom_minutes_quantile(df_in: pd.DataFrame, quantile: float = 0.25) -> pd.DataFrame:
    if df_in.empty:
        return df_in.copy()
    minutes = pd.to_numeric(df_in.get('minutes'), errors='coerce').fillna(0.0)
    cutoff = float(minutes.quantile(quantile))
    filtered = df_in.loc[minutes > cutoff].copy()
    if filtered.empty:
        filtered = df_in.loc[minutes >= cutoff].copy()
    return filtered if not filtered.empty else df_in.copy()


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


season_options = [_season_label(season_id) for season_id in reversed(_season_ids(SEASON_START, SEASON_END))]
season_lookup = {label: label.replace('-', '') for label in season_options}

c_season, c_game, c_date, c_sit, c_cat = st.columns(5, gap='small', vertical_alignment='top')

with c_season:
    season_label = st.selectbox('Season', season_options, index=0, key='feg_season_label')
season_id = season_lookup[season_label]

season_games, season_player_games, bio = _load_page_data(season_id)

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
player_totals['minutes'] = _minutes_series(player_totals, situation_code)
player_totals = player_totals.loc[player_totals['minutes'] > 0].copy()

player_totals = player_totals.merge(
    bio[['playerId', 'playerMenuName', 'positionGroup']],
    on='playerId',
    how='left',
)
player_totals['playerMenuName'] = player_totals['playerMenuName'].fillna(player_totals['playerId'].astype(str))
player_totals['positionGroup'] = player_totals['positionGroup'].fillna('Forwards')
player_totals = player_totals.loc[player_totals['positionGroup'] == POSITION_GROUP].copy()
player_totals = _filter_bottom_minutes_quantile(player_totals)

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

display_cols = ['playerMenuName', 'iGF', 'ixGF', 'iGFAx', 'oGF', 'oxGF', 'oxGFAx', 'oxGA', 'oxG%']
table = player_totals[display_cols].copy()
table = table.sort_values(['oxG%', 'playerMenuName'], ascending=[False, True]).reset_index(drop=True)
for col in ['iGF', 'ixGF', 'iGFAx', 'oGF', 'oxGF', 'oxGFAx', 'oxGA']:
    table[col] = table[col].map(lambda value: _fmt_value(value, category_code))
table['oxG%'] = table['oxG%'].map(lambda value: round(float(value), 1) if pd.notna(value) else 0.0)

if table.empty:
    st.info('No forwards available for this selection.')
else:
    st.dataframe(table, width='stretch', hide_index=True, height=TABLE_HEIGHT)
