# Load libraries.
import streamlit as st

# Logo
st.logo(image = 'assets/Letter.png', size = 'large')

# Set default.
st.set_page_config(layout = 'wide')

# Set up pages.
home_page = st.Page(
    page  = 'views/index.py',
    title = 'Rento\'s Rink',
    icon  = ':material/home:',
    default = True
)
skater_shot_analysis_page = st.Page(
    page  = 'views/skater_shot_analysis.py',
    title = 'Skater Shot Analysis',
    icon  = ':material/readiness_score:'
)
goalie_shot_analysis_page = st.Page(
    page  = 'views/goalie_shot_analysis.py',
    title = 'Goalie Shot Analysis',
    icon  = ':material/readiness_score:'
)

# Set up navigation.
pg = st.navigation(
    {
        'About': [home_page],
        'Models': [skater_shot_analysis_page, goalie_shot_analysis_page]
    }
)
pg.run()
