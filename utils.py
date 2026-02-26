# Load libraries.
import os
import streamlit as st
import pandas as pd


def _file_cache_token(path):
    stat = os.stat(path)
    return (stat.st_mtime_ns, stat.st_size)


@st.cache_data
def _read_csv(path, token):
    return pd.read_csv(path)

# Define helpers.
def load_biographies():
    path = 'data/biographies.csv'
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
