# Load libraries.
import streamlit as st

from utils import file_data_url


# Set default.
st.set_page_config(layout = 'wide')

# Logo
st.logo(image = file_data_url('assets/Letter.png'), size = 'large')
st.markdown(
    '''
    <style>
    div.block-container {
        padding-top: 3rem;
        padding-bottom: 3rem;
    }
    </style>
    ''',
    unsafe_allow_html=True,
)

# Set up pages.
home_page = st.Page(
    page  = 'views/index.py',
    title = 'Rento\'s Rink',
    icon  = ':material/home:',
    default = True
)
xg_article_page = st.Page(
    page  = 'views/example_article.py',
    title = 'xG Model Article',
    icon  = ':material/article:',
    url_path = 'xg_article'
)
expected_goals_page = st.Page(
    page  = 'views/expected_goal.py',
    title = 'Expected Goal',
    icon  = ':material/model_training:'
)
contract_projection_page = st.Page(
    page  = 'views/contract_projection.py',
    title = 'Contract Projection',
    icon  = ':material/model_training:',
    url_path = 'contract_projection'
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
skater_free_agents_page = st.Page(
    page  = 'views/skater_free_agents.py',
    title = 'Skater Free Agents',
    icon  = ':material/attach_money:',
    url_path = 'skater_free_agents'
)
forward_expected_goals_page = st.Page(
    page  = 'views/forward_expected_goals.py',
    title = 'Forward Expected Goals',
    icon  = ':material/leaderboard:'
)
defense_expected_goals_page = st.Page(
    page  = 'views/defense_expected_goals.py',
    title = 'Defense Expected Goals',
    icon  = ':material/leaderboard:'
)
goalie_expected_goals_page = st.Page(
    page  = 'views/goalie_expected_goals.py',
    title = 'Goalie Expected Goals',
    icon  = ':material/leaderboard:'
)


# Register all routes, then render custom sidebar links.
pg = st.navigation(
    {
        'About': [home_page],
        'Visualizations': [
            skater_shot_analysis_page,
            goalie_shot_analysis_page,
            skater_free_agents_page,
        ],
        'Rankings': [
            forward_expected_goals_page,
            defense_expected_goals_page,
            goalie_expected_goals_page,
        ],
        'Models': [expected_goals_page, contract_projection_page],
        'Articles': [xg_article_page],
    },
    position='hidden',
)

with st.sidebar:
    st.caption('About')
    st.page_link(home_page, label='Rento\'s Rink', icon=':material/home:')

    st.caption('Visualizations')
    st.page_link(skater_shot_analysis_page, label='Skater Shot Analysis', icon=':material/readiness_score:')
    st.page_link(goalie_shot_analysis_page, label='Goalie Shot Analysis', icon=':material/readiness_score:')
    st.page_link(skater_free_agents_page, label='Skater Free Agents', icon=':material/attach_money:')

    st.caption('Rankings')
    st.page_link(forward_expected_goals_page, label='Forward Expected Goals', icon=':material/leaderboard:')
    st.page_link(defense_expected_goals_page, label='Defense Expected Goals', icon=':material/leaderboard:')
    st.page_link(goalie_expected_goals_page, label='Goalie Expected Goals', icon=':material/leaderboard:')

    st.caption('Models')
    st.page_link(expected_goals_page, label='Expected Goal', icon=':material/model_training:')
    st.page_link(contract_projection_page, label='Contract Projection', icon=':material/model_training:')

    st.caption('Articles')
    st.page_link(xg_article_page, label='Example Article', icon=':material/article:')

pg.run()
