# Load libraries.
from pathlib import Path
import pandas as pd
import plotly.graph_objects as go
import streamlit as st

def season_label(season_id):
    season_id = str(season_id)
    return f'{season_id[:4]}-{season_id[6:]}'

@st.cache_data(show_spinner=False)
def load_architecture_data():
    data_dir = Path('articles/xG/data')
    by_season_path = data_dir / 'partition_by_season.csv'
    overall_path = data_dir / 'partition_overall.csv'
    code_path = data_dir / 'non_standard_codes.csv'
    overlap_path = data_dir / 'code_overlap.csv'

    if not (by_season_path.exists() and overall_path.exists() and code_path.exists()):
        return None, None, None, None

    by_season = pd.read_csv(by_season_path)
    overall = pd.read_csv(overall_path)
    non_standard_codes = pd.read_csv(code_path)
    code_overlap = pd.read_csv(overlap_path) if overlap_path.exists() else pd.DataFrame()
    return by_season, overall, non_standard_codes, code_overlap

# Keep this article page in a centered, non-wide reading layout.
st.markdown(
    '''
    <style>
    div.block-container {
        max-width: 860px;
    }
    </style>
    ''',
    unsafe_allow_html=True,
)

st.title('Expected Goal (xG) Model')
st.caption('Published: February 28, 2026')
st.markdown('## I. Introduction')

st.markdown(
    '''
Expected goals, or **xG**, is a probability estimate for a single shot event: given the information available at release, what is the chance this shot becomes a goal? Once each shot has a probability, those estimates can be summed to describe chance quality across players, teams, games, and seasons. That framing matters because hockey is noisy. Goals are relatively rare, and short stretches are shaped by finishing runs, goaltending streaks, and randomness. Shot counts alone miss that context. A harmless perimeter attempt and a dangerous slot chance both count as one shot, but they are not equally threatening. xG measures that difference directly. Used properly, xG does not compete with goals; it complements them. Goals tell us what happened, while xG helps explain what was likely to happen based on the chances that were created and allowed.

Historically, the expected-goals framework grew first in soccer, where analysts formalized shot-quality modeling from event data. Hockey adopted the same probabilistic logic and adapted it to a faster, more state-dependent game. As data quality improved, methods moved from simple interpretable baselines to more flexible nonlinear approaches, but the objective stayed the same: produce probabilities that are informative, stable, and well-calibrated. Calibration is critical because a model can rank chances well yet still be mis-scaled in absolute probability terms, making totals and comparisons less reliable. Good xG work is therefore not just model choice; it is disciplined data handling, explicit assumptions, and rigorous testing. We follow that philosophy from foundations through implementation, evaluation, interpretation, and limitations so the model is understood as a full system, not a black-box number.
'''
)

st.markdown('## II. Architecture')
st.markdown(
    'Here we focus on the build phase of the xG pipeline: define the training data, split the problem into the right game-state partitions, decide how feature information enters through versioning, and then choose the model class that can support repeated retraining without sacrificing predictive quality.'
)

st.markdown('### A. Data')
st.markdown('Training uses NHL play-by-play data pulled through `nhlscraper`, specifically `gc_pbps()`, with regular season and playoff shot events (`goal`, `shot-on-goal`, `missed-shot`) as the supervised sample. The model-training scripts fit on 2022-23 through 2024-25, while we repeatedly compare three anchor seasons that serve different validation roles: 2021-22 is an unseen past check (backward out-of-sample), 2023-24 is a seen in-window reference, and 2025-26 is an unseen future check (forward out-of-sample). Keeping those roles fixed throughout our evaluation makes each comparison interpretable as either in-era fit quality or out-of-era robustness.')
season_role = pd.DataFrame(
    [
        {'Season': '2021-22', 'Role in Comparison': 'Unseen Past (backward out-of-sample)'},
        {'Season': '2023-24', 'Role in Comparison': 'Seen (inside training window)'},
        {'Season': '2025-26', 'Role in Comparison': 'Unseen Future (forward out-of-sample)'},
    ]
)
st.dataframe(season_role, hide_index=True, width='stretch')
data_inventory = pd.DataFrame(
    [
        {
            'Data Component': 'Event Identifiers',
            'Examples': 'gameId, eventId, eventOwnerTeamId, shootingPlayerId, goalieInNetId',
        },
        {
            'Data Component': 'Shot Description',
            'Examples': 'distance, angle, shotType',
        },
        {
            'Data Component': 'Pre-Shot Dynamics',
            'Examples': 'dDdT, dAdT, isRush, isRebound',
        },
        {
            'Data Component': 'Game State and Context',
            'Examples': 'period, time elapsed, score state, shot-attempt state, home/playoff flags',
        },
        {
            'Data Component': 'Roster / Biometric Context',
            'Examples': 'shooter and goalie height, weight, age, handedness/position metadata',
        },
        {
            'Data Component': 'Partition Flags',
            'Examples': 'situationCode, isEmptyNetAgainst, isEmptyNetFor, skaterCountFor/Against',
        },
    ]
)
st.dataframe(data_inventory, hide_index=True, width='stretch')

st.markdown(
    '''
Some notes:
- `dDdT` and `dAdT` come from `nhlscraper::calculate_speed()` and represent event-to-event rates: change in shot distance over elapsed time and change in shot angle over elapsed time since the prior valid event. These variables add temporal shot-context information that many simpler xG setups miss, because they encode how quickly the chance geometry is changing right before release rather than only the final snapshot location.
- For `isRush` and `isRebound`, this project follows the public WAR on Ice and NaturalStatTrick convention family, implemented explicitly through `nhlscraper` rules. `isRush` is flagged when a shot attempt occurs within 4 seconds of a prior neutral-zone or defensive-zone event with no stoppage in between, and `isRebound` is flagged when a shot attempt occurs within 3 seconds of a prior blocked, missed, or saved attempt by the same attacking team with no stoppage. These operational definitions reflect transition pressure and second-chance dynamics.
'''
)

st.markdown('### B. Partioning')
st.markdown("Partition rules are straightforward: Standard 5v5 uses `situationCode == '1551'`; Special Teams uses non-5v5, non-shootout, non-empty-net shots; Empty Net uses `isEmptyNetAgainst == TRUE`; and Shootout / Penalty Shot uses `situationCode` in `{1010, 0101}`.")

by_season_df, overall_df, non_standard_df, code_overlap_df = load_architecture_data()
if by_season_df is None:
    st.warning(
        'Architecture evidence files are missing. Run '
        '`Rscript articles/xG/scripts/build_architecture_data.R` to generate them.'
    )
else:
    rename_map = {'Empty-Net Context': 'Empty Net'}
    by_season_df['bucket'] = by_season_df['bucket'].replace(rename_map)
    overall_df['bucket'] = overall_df['bucket'].replace(rename_map)
    non_standard_df['bucket'] = non_standard_df['bucket'].replace(rename_map)
    if code_overlap_df is not None and not code_overlap_df.empty:
        code_overlap_df['bucket'] = code_overlap_df['bucket'].replace(rename_map)

    bucket_order = [
        'Standard 5v5',
        'Special Teams',
        'Empty Net',
        'Shootout / Penalty Shot',
    ]
    bucket_colors = {
        'Standard 5v5': '#2E86C1',
        'Special Teams': '#E67E22',
        'Empty Net': '#C0392B',
        'Shootout / Penalty Shot': '#16A085',
    }

    by_season_df['season'] = by_season_df['season'].astype(str)
    by_season_df['seasonLabel'] = by_season_df['season'].apply(season_label)
    by_season_df['bucket'] = pd.Categorical(by_season_df['bucket'], categories=bucket_order, ordered=True)
    by_season_df = by_season_df.sort_values(['season', 'bucket']).copy()
    by_season_df['shotSharePct'] = by_season_df['shotShare'] * 100.0
    by_season_df['goalRatePct'] = by_season_df['goalRate'] * 100.0

    overall_df['bucket'] = pd.Categorical(overall_df['bucket'], categories=bucket_order, ordered=True)
    overall_df = overall_df.sort_values('bucket').copy()
    overall_df['shotSharePct'] = overall_df['shotShare'] * 100.0

    st.markdown(
        '''
<div style="display:flex; flex-wrap:wrap; gap:8px 16px; margin:0.2rem 0 0.8rem 0; font-size:0.95rem;">
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#2E86C1; display:inline-block;"></span>Standard 5v5</span>
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#E67E22; display:inline-block;"></span>Special Teams</span>
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#C0392B; display:inline-block;"></span>Empty Net</span>
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#16A085; display:inline-block;"></span>Shootout / Penalty Shot</span>
</div>
''',
        unsafe_allow_html=True,
    )

    chart_col1, chart_col2 = st.columns(2, gap='small', vertical_alignment='top')

    with chart_col1:
        fig_mix = go.Figure()
        for bucket in bucket_order:
            d = by_season_df.loc[by_season_df['bucket'] == bucket].copy()
            if d.empty:
                continue
            fig_mix.add_trace(
                go.Bar(
                    x=d['seasonLabel'],
                    y=d['shotSharePct'],
                    name=bucket,
                    marker_color=bucket_colors[bucket],
                    customdata=d[['shots', 'goalRatePct']].to_numpy(),
                    hovertemplate=(
                        'Season: %{x}<br>'
                        f'Partition: {bucket}<br>'
                        'Shot Share: %{y:.1f}%<br>'
                        'Shots: %{customdata[0]:,.0f}<br>'
                        'Goal Rate: %{customdata[1]:.1f}%'
                        '<extra></extra>'
                    ),
                )
            )

        fig_mix.update_layout(
            title=dict(text='Shot Mix by Circumstance', x=0.5, xanchor='center'),
            barmode='stack',
            margin=dict(l=10, r=10, t=45, b=55),
            xaxis=dict(title='Season'),
            yaxis=dict(title='Shot Share (%)', range=[0, 100]),
            showlegend=False,
            height=430,
        )
        fig_mix.update_xaxes(fixedrange=True)
        fig_mix.update_yaxes(fixedrange=True)
        st.plotly_chart(fig_mix, width='stretch', config={'displayModeBar': True})

    with chart_col2:
        fig_rate = go.Figure()
        for bucket in bucket_order:
            d = by_season_df.loc[by_season_df['bucket'] == bucket].copy()
            if d.empty:
                continue
            fig_rate.add_trace(
                go.Scatter(
                    x=d['seasonLabel'],
                    y=d['goalRatePct'],
                    mode='lines+markers',
                    name=bucket,
                    line=dict(width=3, color=bucket_colors[bucket]),
                    marker=dict(size=8),
                    customdata=d[['shots']].to_numpy(),
                    hovertemplate=(
                        'Season: %{x}<br>'
                        f'Partition: {bucket}<br>'
                        'Goal Rate: %{y:.1f}%<br>'
                        'Shots: %{customdata[0]:,.0f}'
                        '<extra></extra>'
                    ),
                )
            )

        fig_rate.update_layout(
            title=dict(text='Conversion Rate by Circumstance', x=0.5, xanchor='center'),
            margin=dict(l=10, r=10, t=60, b=55),
            xaxis=dict(title='Season'),
            yaxis=dict(title='Goal Rate (%)'),
            showlegend=False,
            height=430,
        )
        fig_rate.update_xaxes(fixedrange=True)
        fig_rate.update_yaxes(fixedrange=True)
        st.plotly_chart(fig_rate, width='stretch', config={'displayModeBar': True})

    overall_map = {
        str(row['bucket']): row
        for _, row in overall_df.iterrows()
    }
    if all(k in overall_map for k in bucket_order):
        summary_text = (
            f"Across the pooled seasons, Standard 5v5 drives most volume ({overall_map['Standard 5v5']['shotSharePct']:.1f}% of shots), while Special Teams carries higher baseline conversion ({overall_map['Special Teams']['goalRate'] * 100:.1f}% vs. {overall_map['Standard 5v5']['goalRate'] * 100:.1f}% at 5v5), and Empty Net plus Shootout / Penalty Shot remain low-volume but structurally different states ({overall_map['Empty Net']['shotSharePct']:.1f}% and {overall_map['Shootout / Penalty Shot']['shotSharePct']:.1f}%); this is exactly why dedicated partitions matter."
        )
        if code_overlap_df is not None and not code_overlap_df.empty:
            summary_text += ' Some `situationCode` values also appear in both empty-net and non-empty contexts, so partition logic must prioritize explicit empty-net flags before situation-code grouping.'
        st.markdown(summary_text)

    if code_overlap_df is not None and not code_overlap_df.empty:
        overlap_top = code_overlap_df.sort_values('shots', ascending=False).head(8).copy()
        overlap_top = overlap_top.rename(
            columns={
                'situationCode': 'Situation Code',
                'bucket': 'Partition',
                'shots': 'Shots',
            }
        )
        st.dataframe(
            overlap_top[['Situation Code', 'Partition', 'Shots']],
            hide_index=True,
            width='stretch',
        )

    non_standard_df['bucket'] = pd.Categorical(non_standard_df['bucket'], categories=bucket_order, ordered=True)
    top_codes = (
        non_standard_df.sort_values(['bucket', 'shots'], ascending=[True, False])
        .groupby('bucket', observed=True)
        .head(4)
        .copy()
    )
    top_codes = top_codes.rename(
        columns={
            'bucket': 'Partition',
            'situationCode': 'Situation Code',
            'shots': 'Shots',
            'shotShareWithinBucket': 'Share Within Partition',
            'goalRate': 'Goal Rate',
        }
    )
    top_codes['Share Within Partition'] = (top_codes['Share Within Partition'] * 100).map(lambda x: f'{x:.1f}%')
    top_codes['Goal Rate'] = (top_codes['Goal Rate'] * 100).map(lambda x: f'{x:.1f}%')

    st.markdown('The non-5v5 world is not one regime, and the highest-volume non-standard situation codes show that this side of the data is internally heterogeneous rather than a single background bucket.')
    st.dataframe(
        top_codes[['Partition', 'Situation Code', 'Shots', 'Share Within Partition', 'Goal Rate']],
        hide_index=True,
        width='stretch',
    )

st.markdown('### C. Versioning')
st.markdown(
    'Partitioning answers where a shot came from in game-state terms. Versioning answers how much information should be allowed into the estimator. The design moves from highly controllable signals to broader, more descriptive context:'
)

version_table = pd.DataFrame(
    [
        {
            'Version': 'V1',
            'Information Layer': 'Shot geometry and shot context',
            'Philosophy': 'Most controllable by the shooter and strongest baseline for repeatable shot quality.',
        },
        {
            'Version': 'V2',
            'Information Layer': 'Immediate pre-shot dynamics',
            'Philosophy': 'Adds high-signal context right before release without jumping to full game state.',
        },
        {
            'Version': 'V3',
            'Information Layer': 'Game state and scoreboard context',
            'Philosophy': 'Captures tactical environment and state pressure that shape chance selection.',
        },
        {
            'Version': 'V4',
            'Information Layer': 'Player biometrics and profile context',
            'Philosophy': 'Adds richer descriptive signal that can help prediction but is less directly controllable.',
        },
    ]
)
st.dataframe(version_table, hide_index=True, width='stretch')

st.markdown(
    'This versioning structure keeps the model family honest: if richer versions improve predictive metrics, that gain is earned and measurable; if gains fade in out-of-era data, simpler versions remain available as robust alternatives. In practice, this is the bridge between model building and model testing, because each version becomes a testable hypothesis about what information is truly useful.'
)

st.markdown('### D. Model Choice')
st.markdown(
    'The final model family is LightGBM because it gave the best practical balance between training speed and predictive strength at project scale. Logistic regression remains an interpretable baseline, while random forest and XGBoost are strong nonlinear candidates, but LightGBM is the most efficient fit for repeated multi-partition, multi-version training and refresh cycles.'
)

candidate_table = pd.DataFrame(
    [
        {
            'Candidate': 'Logistic Regression',
            'Predictive Flexibility': 'Low to Moderate',
            'Training Time at Scale': 'Fast',
            'Project Fit': 'Excellent baseline; limited nonlinear interaction capture.',
        },
        {
            'Candidate': 'Random Forest',
            'Predictive Flexibility': 'Moderate to High',
            'Training Time at Scale': 'Moderate to Slow',
            'Project Fit': 'Useful benchmark; less efficient for repeated large refreshes.',
        },
        {
            'Candidate': 'XGBoost',
            'Predictive Flexibility': 'High',
            'Training Time at Scale': 'Moderate',
            'Project Fit': 'Strong contender; typically heavier tuning/runtime tradeoff here.',
        },
        {
            'Candidate': 'LightGBM (Chosen)',
            'Predictive Flexibility': 'High',
            'Training Time at Scale': 'Fast to Moderate',
            'Project Fit': 'Best training-time/predictive-power balance for this architecture.',
        },
    ]
)
st.dataframe(candidate_table, hide_index=True, width='stretch')

@st.cache_data(show_spinner=False)
def load_feature_importance():
    path = Path('articles/xG/data/feature_importance_all.csv')
    if not path.exists():
        return None
    return pd.read_csv(path)


def feature_family(feature_name):
    f = str(feature_name).lower()
    if f in {'distance', 'angle'} or f.startswith('shottype'):
        return 'Shot Geometry and Type'
    if f in {'dddt', 'dadt', 'isrush', 'isrebound'}:
        return 'Pre-Shot Dynamics'
    if (
        f in {'ishome', 'isplayoff', 'period'}
        or 'secondselapsed' in f
        or f in {
            'goalsfor', 'goalsagainst', 'sogfor', 'sogagainst',
            'fenwickfor', 'fenwickagainst', 'corsifor', 'corsiagainst'
        }
    ):
        return 'Game State and Score'
    if f.startswith('shooter') or f.startswith('goalie'):
        return 'Player Profile'
    if f.startswith('isemptynet') or f.startswith('skatercount'):
        return 'Net and Skater State'
    return 'Other'


st.markdown('## III. Validation')
st.markdown('### A. Metrics')
st.markdown('After the architecture and training choices are fixed, validation asks whether the estimated probabilities are both discriminative and trustworthy in scale. We intentionally focus on two metrics only. Log-loss measures probability quality at the event level and punishes confident mistakes more heavily than uncertain mistakes, so lower values indicate better probabilistic sharpness and discrimination. Calibration is reported as observed goals divided by expected goals, where 1.000 is ideal; values above 1.000 indicate underprediction in aggregate, and values below 1.000 indicate overprediction. The values below use LightGBM only and follow the three season roles used throughout our evaluation: 2021-22 as unseen past, 2023-24 as seen reference, and 2025-26 as unseen future.')

st.markdown('### B. Results by Scope')

validation_rows = [
    {'Scope': '5v5', 'Season': '2021-22', 'Season Role': 'Unseen Past', 'Version': 'V1', 'Log Loss': 0.20704, 'Calibration': 0.994},
    {'Scope': '5v5', 'Season': '2021-22', 'Season Role': 'Unseen Past', 'Version': 'V2', 'Log Loss': 0.20921, 'Calibration': 0.989},
    {'Scope': '5v5', 'Season': '2021-22', 'Season Role': 'Unseen Past', 'Version': 'V3', 'Log Loss': 0.20793, 'Calibration': 0.991},
    {'Scope': '5v5', 'Season': '2021-22', 'Season Role': 'Unseen Past', 'Version': 'V4', 'Log Loss': 0.20776, 'Calibration': 0.987},
    {'Scope': '5v5', 'Season': '2023-24', 'Season Role': 'Seen', 'Version': 'V1', 'Log Loss': 0.19467, 'Calibration': 1.019},
    {'Scope': '5v5', 'Season': '2023-24', 'Season Role': 'Seen', 'Version': 'V2', 'Log Loss': 0.18740, 'Calibration': 1.027},
    {'Scope': '5v5', 'Season': '2023-24', 'Season Role': 'Seen', 'Version': 'V3', 'Log Loss': 0.19211, 'Calibration': 1.026},
    {'Scope': '5v5', 'Season': '2023-24', 'Season Role': 'Seen', 'Version': 'V4', 'Log Loss': 0.19108, 'Calibration': 1.027},
    {'Scope': '5v5', 'Season': '2025-26', 'Season Role': 'Unseen Future', 'Version': 'V1', 'Log Loss': 0.20335, 'Calibration': 0.985},
    {'Scope': '5v5', 'Season': '2025-26', 'Season Role': 'Unseen Future', 'Version': 'V2', 'Log Loss': 0.20129, 'Calibration': 0.987},
    {'Scope': '5v5', 'Season': '2025-26', 'Season Role': 'Unseen Future', 'Version': 'V3', 'Log Loss': 0.20106, 'Calibration': 0.986},
    {'Scope': '5v5', 'Season': '2025-26', 'Season Role': 'Unseen Future', 'Version': 'V4', 'Log Loss': 0.20087, 'Calibration': 0.985},
    {'Scope': 'All Situations', 'Season': '2021-22', 'Season Role': 'Unseen Past', 'Version': 'V1', 'Log Loss': 0.22898, 'Calibration': 1.001},
    {'Scope': 'All Situations', 'Season': '2021-22', 'Season Role': 'Unseen Past', 'Version': 'V2', 'Log Loss': 0.23101, 'Calibration': 0.997},
    {'Scope': 'All Situations', 'Season': '2021-22', 'Season Role': 'Unseen Past', 'Version': 'V3', 'Log Loss': 0.22992, 'Calibration': 0.999},
    {'Scope': 'All Situations', 'Season': '2021-22', 'Season Role': 'Unseen Past', 'Version': 'V4', 'Log Loss': 0.22999, 'Calibration': 0.996},
    {'Scope': 'All Situations', 'Season': '2023-24', 'Season Role': 'Seen', 'Version': 'V1', 'Log Loss': 0.21815, 'Calibration': 1.008},
    {'Scope': 'All Situations', 'Season': '2023-24', 'Season Role': 'Seen', 'Version': 'V2', 'Log Loss': 0.21184, 'Calibration': 1.013},
    {'Scope': 'All Situations', 'Season': '2023-24', 'Season Role': 'Seen', 'Version': 'V3', 'Log Loss': 0.21503, 'Calibration': 1.013},
    {'Scope': 'All Situations', 'Season': '2023-24', 'Season Role': 'Seen', 'Version': 'V4', 'Log Loss': 0.21265, 'Calibration': 1.014},
    {'Scope': 'All Situations', 'Season': '2025-26', 'Season Role': 'Unseen Future', 'Version': 'V1', 'Log Loss': 0.22842, 'Calibration': 0.957},
    {'Scope': 'All Situations', 'Season': '2025-26', 'Season Role': 'Unseen Future', 'Version': 'V2', 'Log Loss': 0.22631, 'Calibration': 0.959},
    {'Scope': 'All Situations', 'Season': '2025-26', 'Season Role': 'Unseen Future', 'Version': 'V3', 'Log Loss': 0.22609, 'Calibration': 0.957},
    {'Scope': 'All Situations', 'Season': '2025-26', 'Season Role': 'Unseen Future', 'Version': 'V4', 'Log Loss': 0.22617, 'Calibration': 0.959},
]
validation_df = pd.DataFrame(validation_rows)
season_order = ['2021-22', '2023-24', '2025-26']
season_colors = {'2021-22': '#2E86C1', '2023-24': '#E67E22', '2025-26': '#16A085'}
version_order = ['V1', 'V2', 'V3', 'V4']

tabs = st.tabs(['5v5', 'All Situations'])
for scope_label, tab in zip(['5v5', 'All Situations'], tabs):
    with tab:
        d_scope = validation_df.loc[validation_df['Scope'] == scope_label].copy()
        d_scope['Season'] = pd.Categorical(d_scope['Season'], categories=season_order, ordered=True)
        d_scope['Version'] = pd.Categorical(d_scope['Version'], categories=version_order, ordered=True)
        d_scope = d_scope.sort_values(['Season', 'Version'])

        st.markdown(
            '''
<div style="display:flex; flex-wrap:wrap; gap:8px 16px; margin:0.2rem 0 0.8rem 0; font-size:0.95rem;">
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#2E86C1; display:inline-block;"></span>2021-22 (Unseen Past)</span>
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#E67E22; display:inline-block;"></span>2023-24 (Seen)</span>
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#16A085; display:inline-block;"></span>2025-26 (Unseen Future)</span>
</div>
''',
            unsafe_allow_html=True,
        )

        col_log, col_cal = st.columns(2, gap='small', vertical_alignment='top')

        with col_log:
            fig_log = go.Figure()
            for season in season_order:
                d_season = d_scope.loc[d_scope['Season'] == season].copy()
                if d_season.empty:
                    continue
                fig_log.add_trace(
                    go.Scatter(
                        x=d_season['Version'],
                        y=d_season['Log Loss'],
                        mode='lines+markers',
                        line=dict(width=3, color=season_colors[season]),
                        marker=dict(size=8),
                        customdata=d_season[['Season Role']].to_numpy(),
                        hovertemplate=(
                            'Season: ' + season + '<br>'
                            'Role: %{customdata[0]}<br>'
                            'Version: %{x}<br>'
                            'Log Loss: %{y:.5f}'
                            '<extra></extra>'
                        ),
                        showlegend=False,
                    )
                )

            fig_log.update_layout(
                title=dict(text='Log Loss by Version', x=0.5, xanchor='center'),
                margin=dict(l=10, r=10, t=45, b=40),
                xaxis=dict(title='Version'),
                yaxis=dict(title='Log Loss'),
                height=380,
                showlegend=False,
            )
            fig_log.update_xaxes(fixedrange=True)
            fig_log.update_yaxes(fixedrange=True)
            st.plotly_chart(fig_log, width='stretch', config={'displayModeBar': True})

        with col_cal:
            fig_cal = go.Figure()
            for season in season_order:
                d_season = d_scope.loc[d_scope['Season'] == season].copy()
                if d_season.empty:
                    continue
                fig_cal.add_trace(
                    go.Scatter(
                        x=d_season['Version'],
                        y=d_season['Calibration'],
                        mode='lines+markers',
                        line=dict(width=3, color=season_colors[season]),
                        marker=dict(size=8),
                        customdata=d_season[['Season Role']].to_numpy(),
                        hovertemplate=(
                            'Season: ' + season + '<br>'
                            'Role: %{customdata[0]}<br>'
                            'Version: %{x}<br>'
                            'Calibration: %{y:.3f}'
                            '<extra></extra>'
                        ),
                        showlegend=False,
                    )
                )

            fig_cal.add_shape(
                type='line',
                xref='paper',
                yref='y',
                x0=0,
                y0=1.0,
                x1=1,
                y1=1.0,
                line=dict(color='rgba(180,180,180,0.7)', width=2, dash='dot'),
            )
            fig_cal.update_layout(
                title=dict(text='Calibration by Version', x=0.5, xanchor='center'),
                margin=dict(l=10, r=10, t=45, b=40),
                xaxis=dict(title='Version'),
                yaxis=dict(title='Calibration (Observed / Expected)'),
                height=380,
                showlegend=False,
            )
            fig_cal.update_xaxes(fixedrange=True)
            fig_cal.update_yaxes(fixedrange=True)
            st.plotly_chart(fig_cal, width='stretch', config={'displayModeBar': True})

        d_scope['Calibration Error'] = (d_scope['Calibration'] - 1.0).abs()
        d_best_log = d_scope.loc[d_scope.groupby('Season', observed=False)['Log Loss'].idxmin()].copy().sort_values('Season')
        d_best_cal = d_scope.loc[d_scope.groupby('Season', observed=False)['Calibration Error'].idxmin()].copy().sort_values('Season')

        summary_log = (
            d_scope.groupby('Version', as_index=False, observed=False)
            .agg(
                MeanLogLoss=('Log Loss', 'mean'),
                LogLossRange=('Log Loss', lambda x: x.max() - x.min()),
                MeanCalibration=('Calibration', 'mean'),
                MeanCalibrationError=('Calibration Error', 'mean'),
            )
            .sort_values('MeanLogLoss')
            .reset_index(drop=True)
        )
        st.dataframe(summary_log, hide_index=True, width='stretch')

        season_means = (
            d_scope.groupby('Season', as_index=False, observed=False)
            .agg(
                MeanLogLoss=('Log Loss', 'mean'),
                MeanCalibration=('Calibration', 'mean'),
            )
            .sort_values('Season')
        )

        gain_rows = []
        for season in season_order:
            d_season = d_scope.loc[d_scope['Season'] == season].copy()
            if d_season.empty:
                continue
            v1_log = float(d_season.loc[d_season['Version'] == 'V1', 'Log Loss'].iloc[0])
            best_log = float(d_season['Log Loss'].min())
            gain_rows.append((season, v1_log - best_log))

        gain_sentence = ', '.join([f'{season}: {gain:.5f}' for season, gain in gain_rows]) if gain_rows else 'N/A'
        best_log_sentence = ', '.join(
            d_best_log['Season'].astype(str) + ' -> ' + d_best_log['Version'].astype(str) + ' (' + d_best_log['Log Loss'].map(lambda x: f'{x:.5f}') + ')'
        )
        best_cal_sentence = ', '.join(
            d_best_cal['Season'].astype(str) + ' -> ' + d_best_cal['Version'].astype(str) + ' (error ' + d_best_cal['Calibration Error'].map(lambda x: f'{x:.3f}') + ')'
        )
        season_drift_sentence = ', '.join(
            season_means['Season'].astype(str) + ': ' + season_means['MeanCalibration'].map(lambda x: f'{x:.3f}')
        )

        st.markdown(
            f"For {scope_label}, the best log-loss version by season is {best_log_sentence}. The smallest calibration-error version by season is {best_cal_sentence}. Relative to V1, log-loss gains from selecting the best version are {gain_sentence}. Mean calibration by season is {season_drift_sentence}, which shows how calibration direction shifts by era rather than remaining fixed."
        )

st.markdown('Across both scopes, the validation results show three stable patterns. First, the best version is not identical across all season roles, so version choice should be treated as a robustness tradeoff rather than a single permanent winner. Second, log-loss gains are largest in the seen window and narrower in unseen windows, which is expected when richer feature sets capture season-specific structure. Third, calibration stays close to 1.000 but still moves directionally by era, so calibration should be monitored continuously even when discrimination metrics remain strong.')

st.markdown('## IV. Interpretation')
st.markdown('Once validation confirms that probability quality is strong enough, interpretation asks a different question: what information the model is actually relying on to achieve that performance. LightGBM feature importance is based on gain share, which measures how much each feature contributes to reducing the training objective across tree splits. Higher gain does not mean causal effect or directional effect, but it does show relative influence in the fitted model. Practically, this tool lets you compare importance profiles by partition, switch versions to see how signal concentration changes as features are added, and aggregate features into families to identify whether the model is leaning mostly on shot geometry, pre-shot dynamics, game state, player profile, or net/skater context. The view below defaults to Version 4 because it is the richest feature set and gives the clearest picture of cross-partition behavior.')

imp_all = load_feature_importance()
if imp_all is None:
    st.warning(
        'Feature-importance data is missing. Run '
        '`Rscript articles/xG/scripts/export_feature_importance.R` to generate it.'
    )
else:
    part_map = {
        'standard': 'Standard 5v5',
        'special': 'Special Teams',
        'empty': 'Empty Net',
        'shootout': 'Shootout / Penalty Shot',
    }
    imp_all['partition_label'] = imp_all['partition'].map(part_map).fillna(imp_all['partition'])
    imp_all['version'] = pd.to_numeric(imp_all['version'], errors='coerce')

    version_options = sorted([int(v) for v in imp_all['version'].dropna().unique().tolist()])
    default_idx = version_options.index(4) if 4 in version_options else 0
    selected_version = st.selectbox('Version for Interpretation', version_options, index=default_idx, key='xg_interpretation_version')

    imp_v = imp_all.loc[imp_all['version'] == int(selected_version)].copy()
    if imp_v.empty:
        st.info('No feature-importance rows found for this version.')
    else:
        partition_order = ['Standard 5v5', 'Special Teams', 'Empty Net', 'Shootout / Penalty Shot']
        tabs_imp = st.tabs(partition_order)

        for p_label, tab in zip(partition_order, tabs_imp):
            with tab:
                d_part = imp_v.loc[imp_v['partition_label'] == p_label].copy()
                if d_part.empty:
                    st.info('No feature-importance rows for this partition/version.')
                    continue

                d_part = d_part.sort_values('gain', ascending=False).head(12).copy()
                d_part = d_part.iloc[::-1].copy()
                d_part['gainPct'] = d_part['gain'] * 100.0

                fig_imp = go.Figure(
                    go.Bar(
                        x=d_part['gainPct'],
                        y=d_part['feature'],
                        orientation='h',
                        marker=dict(color='#2E86C1'),
                        customdata=d_part[['rank']].to_numpy(),
                        hovertemplate=(
                            'Feature: %{y}<br>'
                            'Gain Share: %{x:.2f}%<br>'
                            'Rank: %{customdata[0]:.0f}'
                            '<extra></extra>'
                        ),
                        showlegend=False,
                    )
                )
                fig_imp.update_layout(
                    title=dict(text=f'Top Feature Importance ({p_label}, V{selected_version})', x=0.5, xanchor='center'),
                    margin=dict(l=10, r=10, t=45, b=35),
                    xaxis=dict(title='Gain Share (%)'),
                    yaxis=dict(title='Feature'),
                    height=420,
                    showlegend=False,
                )
                fig_imp.update_xaxes(fixedrange=True)
                fig_imp.update_yaxes(fixedrange=True)
                st.plotly_chart(fig_imp, width='stretch', config={'displayModeBar': True})

                top3 = d_part.sort_values('gain', ascending=False).head(3)['feature'].tolist()
                top5_share = d_part.sort_values('gain', ascending=False).head(5)['gainPct'].sum()
                st.markdown(
                    f"For {p_label}, the top three features are {', '.join(top3)}, and the top five features account for about {top5_share:.1f}% of gain within this top-feature slice."
                )

        imp_v['family'] = imp_v['feature'].apply(feature_family)
        family_share = (
            imp_v.groupby(['partition_label', 'family'], as_index=False)['gain']
            .sum()
        )
        family_share['gainPct'] = family_share['gain'] * 100.0

        families = [
            'Shot Geometry and Type',
            'Pre-Shot Dynamics',
            'Game State and Score',
            'Player Profile',
            'Net and Skater State',
            'Other',
        ]
        partition_order = ['Standard 5v5', 'Special Teams', 'Empty Net', 'Shootout / Penalty Shot']
        heat = (
            family_share.pivot(index='partition_label', columns='family', values='gainPct')
            .reindex(index=partition_order, columns=families)
            .fillna(0.0)
        )

        fig_family = go.Figure(
            go.Heatmap(
                z=heat.values,
                x=heat.columns.tolist(),
                y=heat.index.tolist(),
                colorscale='Blues',
                zmin=0,
                zmax=max(5.0, float(heat.values.max())),
                hovertemplate='Partition: %{y}<br>Feature Family: %{x}<br>Gain Share: %{z:.2f}%<extra></extra>',
                colorbar=dict(title='Gain Share (%)'),
            )
        )
        fig_family.update_layout(
            title=dict(text=f'Feature Family Share by Partition (V{selected_version})', x=0.5, xanchor='center'),
            margin=dict(l=10, r=10, t=45, b=80),
            xaxis=dict(title='Feature Family'),
            yaxis=dict(title='Partition'),
            height=420,
        )
        fig_family.update_xaxes(fixedrange=True)
        fig_family.update_yaxes(fixedrange=True)
        st.plotly_chart(fig_family, width='stretch', config={'displayModeBar': True})

        top_family = (
            family_share.sort_values(['partition_label', 'gainPct'], ascending=[True, False])
            .groupby('partition_label', as_index=False)
            .first()
        )
        family_sentence = ', '.join(
            top_family['partition_label'] + ' -> ' + top_family['family'] + ' (' + top_family['gainPct'].map(lambda x: f'{x:.1f}%') + ')'
        )
        st.markdown(
            f"The dominant feature family by partition for Version {selected_version} is {family_sentence}. This helps separate stable core drivers from partition-specific context effects and gives a practical interpretation layer atop the validation metrics."
        )

st.markdown('## V. Conclusion')
st.markdown('We walked through the full xG workflow as a connected system: start from structured event data, partition the problem by game circumstance, version feature sets by information philosophy, train with a model class that balances speed and predictive power, validate using log-loss and calibration across seen and unseen eras, and then interpret the fitted model through feature-importance structure. The central takeaway is that reliable xG comes from disciplined architecture and evaluation, not from a single algorithm choice in isolation.')
st.markdown('There are important limitations. The event stream does not provide full passing-chain or puck/player tracking detail, so important pre-shot context is still partially latent. There is also no direct third spatial coordinate (puck or stick height), which matters for tips, elevated releases, and save mechanics. And while the model can include descriptive profile variables, it is not yet explicitly modeling persistent individual finishing/shot-stopping skill or team-level shooting environment skill as dedicated components. The most feasible next step with available data is to add shift-fatigue signals and explicit individual/team skill features into the feature set, then re-run the same partitioned and versioned validation framework to measure whether those additions improve out-of-era log-loss and calibration without sacrificing interpretability.')
st.markdown('Summarized analyses of these model outputs are available in the following pages:')
st.page_link('views/skater_shot_analysis.py', label='Skater Shot Analysis', icon=':material/readiness_score:')
st.page_link('views/goalie_shot_analysis.py', label='Goalie Shot Analysis', icon=':material/readiness_score:')
