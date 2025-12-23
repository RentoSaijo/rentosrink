import streamlit as st
from streamlit.components.v1 import html
    html('''
       <script>
        window.top.document.querySelectorAll(`[href*="streamlit.io"]`).forEach(e => e.setAttribute("style", "display: none;"));
      </script>
    ''')

st.title("Rento's Rink")