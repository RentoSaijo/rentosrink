# Load libraries.
import base64
import html
import mimetypes
import os
from pathlib import Path

import pandas as pd
import streamlit as st


def _file_cache_token(path):
    stat = os.stat(path)
    return (stat.st_mtime_ns, stat.st_size)


@st.cache_data
def _read_csv(path, token):
    return pd.read_csv(path)


@st.cache_data(show_spinner=False)
def _local_file_data_url(path, token):
    mime_type = mimetypes.guess_type(path)[0] or 'application/octet-stream'
    data = Path(path).read_bytes()
    encoded = base64.b64encode(data).decode('ascii')
    return f'data:{mime_type};base64,{encoded}'


def file_data_url(path):
    return _local_file_data_url(path, _file_cache_token(path))


def render_local_image(path, fallback=None, alt=''):
    selected_path = None

    if path and os.path.exists(path):
        selected_path = path
    elif fallback and os.path.exists(fallback):
        selected_path = fallback

    if selected_path is None:
        st.empty()
        return

    data_url = file_data_url(selected_path)
    safe_alt = html.escape(alt, quote=True)
    st.markdown(
        f'<img src="{data_url}" alt="{safe_alt}" style="display: block; width: 100%; height: auto;">',
        unsafe_allow_html=True,
    )

# Define helpers.
def load_biographies():
    path = 'data/biographies.csv'
    return _read_csv(path, _file_cache_token(path))

def load_games():
    path = 'data/games.csv'
    return _read_csv(path, _file_cache_token(path))

def load_teams():
    path = 'data/teams.csv'
    return _read_csv(path, _file_cache_token(path))

def load_contract_projection(season = 20262027):
    path = f'data/contract_projection_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_contract_possibility(season = 20262027):
    path = f'data/contract_possibility_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_skater_contracts():
    path = 'data/skater_contracts.csv'
    return _read_csv(path, _file_cache_token(path))

def load_skater_free_agents(path = 'data/skater_free_agents.csv'):
    return _read_csv(path, _file_cache_token(path))

def load_skater_shot_analysis(season = 20242025):
    path = f'data/skater_shot_analysis_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_goalie_shot_analysis(season = 20242025):
    path = f'data/goalie_shot_analysis_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_gbgs_skater_basic(season = 20242025):
    path = f'data/gbgs/basic/skaters_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_gbgs_skater_advanced(season = 20242025):
    path = f'data/gbgs/advanced/skaters_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_gbgs_goalie_basic(season = 20242025):
    path = f'data/gbgs/basic/goalies_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_gbgs_goalie_advanced(season = 20242025):
    path = f'data/gbgs/advanced/goalies_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_gbgs_team_basic(season = 20242025):
    path = f'data/gbgs/basic/teams_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_gbgs_team_advanced(season = 20242025):
    path = f'data/gbgs/advanced/teams_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_sbss_skaters(season = 20242025):
    path = f'data/sbss/skaters_{season}.csv'
    return _read_csv(path, _file_cache_token(path))

def load_sbss_goalies(season = 20242025):
    path = f'data/sbss/goalies_{season}.csv'
    return _read_csv(path, _file_cache_token(path))
