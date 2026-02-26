# Load libraries.
import streamlit as st

# Logo
st.logo(image = 'assets/Letter.png', size = 'large')

# Set default.
st.set_page_config(layout = 'wide')
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
example_article_page = st.Page(
    page  = 'views/example_article.py',
    title = 'Example Article',
    icon  = ':material/article:'
)
expected_goals_page = st.Page(
    page  = 'views/expected_goal.py',
    title = 'Expected Goal',
    icon  = ':material/model_training:'
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
skater_free_agents_legacy_page = st.Page(
    page  = 'views/skater_free_agents.py',
    title = 'Skater Free Agents',
    icon  = ':material/attach_money:',
    url_path = 'contract_projection'
)


# Register all routes (including legacy alias), then render custom sidebar links.
pg = st.navigation(
    {
        'About': [home_page],
        'Visualizations': [
            skater_shot_analysis_page,
            goalie_shot_analysis_page,
            skater_free_agents_page,
        ],
        'Models': [expected_goals_page],
        'Articles': [example_article_page],
        'Legacy': [skater_free_agents_legacy_page],
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

    st.caption('Models')
    st.page_link(expected_goals_page, label='Expected Goal', icon=':material/model_training:')

    st.caption('Articles')
    st.page_link(example_article_page, label='Example Article', icon=':material/article:')

pg.run()
