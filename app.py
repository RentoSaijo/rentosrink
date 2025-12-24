# Load libraries.
import streamlit as st
import pandas as pd

# Logo
st.logo(image = 'assets/Letter.png', size = 'large')

# Title
st.title('Rento\'s Rink')

# Load data.
@st.cache_data
def load_skaters_xG():
    return pd.read_csv('data/skaters_xG.csv')
skaters_xG = load_skaters_xG()

# Create scatter plot.
st.scatter_chart(
    skaters_xG,
    x = "ixGF_20242025_2",
    y = "iGF_20242025_2",
)
