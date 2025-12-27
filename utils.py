import streamlit as st
import pandas as pd

@st.cache_data
def load_biographies():
    return pd.read_csv('data/biographies.csv')

@st.cache_data
def load_skater_shot_analysis(season = 20242025):
    return pd.read_csv(f'data/skater_shot_analysis_{season}.csv')

@st.cache_data
def load_goalie_shot_analysis(season = 20242025):
    return pd.read_csv(f'data/goalie_shot_analysis_{season}.csv')
