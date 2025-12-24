# Load library.
import streamlit as st
from utils import load_goalies_xg

# Load data.
goalies_xG = load_goalies_xg()

# Temporary.
st.title('Coming soon!')

# Create scatter plot.
st.scatter_chart(
    goalies_xG,
    x = "ixGA_20242025_2",
    y = "iGA_20242025_2",
)
