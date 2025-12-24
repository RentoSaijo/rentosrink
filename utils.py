import streamlit as st
import pandas as pd

@st.cache_data
def load_skaters_xg():
    return pd.read_csv('data/skaters_xG.csv')

@st.cache_data
def load_goalies_xg():
    return pd.read_csv('data/goalies_xG.csv')
