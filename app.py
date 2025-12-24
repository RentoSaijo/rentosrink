# Load libraries.
import streamlit as st

# Logo
st.logo(image = 'assets/Letter.png', size = 'large')

# Set defaults.
st.set_page_config(layout = 'wide')

# Set up pages.
index_page    = st.Page(
    page  = 'views/index.py',
    title = 'Rento\'s Rink',
    icon  = '🏠',
    default = True
)
skaters_xg_page = st.Page(
    page  = 'views/skaters_xg.py',
    title = 'Skaters xG',
    icon  = '🏒'
)
goalies_xg_page = st.Page(
    page  = 'views/goalies_xg.py',
    title = 'Goalies xG',
    icon  = '🏒'
)

# Set up navigation.
pg = st.navigation(
    {
        'About': [index_page],
        'Models': [skaters_xg_page, goalies_xg_page]
    }
)
pg.run()
