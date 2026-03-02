# Load libraries.
from pathlib import Path
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import streamlit as st


def load_architecture_data():
    data_dir = Path('articles/contracts/data')
    snapshot_path = data_dir / 'data_snapshot.csv'
    family_path = data_dir / 'feature_family_counts.csv'
    term_path = data_dir / 'term_distribution_by_resign.csv'
    volume_path = data_dir / 'scenario_volume.csv'
    term_compare_path = data_dir / 'model_compare_term_by_split.csv'
    aavp_compare_path = data_dir / 'model_compare_aavp_by_split.csv'
    importance_path = data_dir / 'model_importance_selected.csv'

    required = [snapshot_path, family_path, term_path, volume_path, term_compare_path, aavp_compare_path, importance_path]
    if not all(p.exists() for p in required):
        return None

    return {
        'snapshot': pd.read_csv(snapshot_path),
        'feature_families': pd.read_csv(family_path),
        'term_distribution': pd.read_csv(term_path),
        'scenario_volume': pd.read_csv(volume_path),
        'term_compare_by_split': pd.read_csv(term_compare_path),
        'aavp_compare_by_split': pd.read_csv(aavp_compare_path),
        'importance_selected': pd.read_csv(importance_path),
    }


def render_term_legend(term_colors):
    chips = ''.join(
        f'<span style="display:inline-flex; align-items:center; gap:6px; margin-right:12px;">'
        f'<span style="width:10px; height:10px; display:inline-block; background:{color};"></span>'
        f'Term {term}</span>'
        for term, color in term_colors.items()
    )
    st.markdown(
        f'<div style="display:flex; flex-wrap:wrap; margin:0.1rem 0 0.7rem 0; font-size:0.92rem;">{chips}</div>',
        unsafe_allow_html=True,
    )


def classify_contract_feature(feature_name):
    f = str(feature_name)
    f_lower = f.lower()

    if (
        f_lower in {'ageatsigning', 'contractnumber', 'prevterm', 'prevaavp', 'positioncode', 'isresign'}
        or f_lower.startswith('positioncode_')
        or f_lower.startswith('isresign_')
    ):
        return 'Player and Contract History'
    if f_lower.startswith('mp_2_'):
        return 'Usage Levels'
    if f_lower.startswith('dmp_2_'):
        return 'Usage Trends'

    offensive_roots = {
        'assists', 'x', 'y', 'icorsif', 'ifenwickf', 'isogf', 'igf', 'ixgf',
        'ocorsif', 'ofenwickf', 'osogf', 'ogf', 'oxgf',
    }
    defensive_roots = {'blocks', 'giveaways', 'hits', 'takeaways', 'ocorsia', 'ofenwicka', 'osoga', 'oga', 'oxga'}

    is_trend = f_lower.startswith('d')
    root_token = f_lower
    if '_2_' in root_token:
        root_token = root_token.split('_2_')[0]
    if is_trend:
        root_token = root_token[1:]
    if root_token in offensive_roots:
        return 'Offensive Trends' if is_trend else 'Offensive Rates'
    if root_token in defensive_roots:
        return 'Defensive Trends' if is_trend else 'Defensive Rates'
    return 'Other'


def describe_contract_feature(feature_name):
    f = str(feature_name)
    f_lower = f.lower()
    situation_map = {'ev': 'even-strength', 'pp': 'power-play', 'sh': 'shorthanded'}

    exact = {
        'prevaavp': 'previous contract cap-share level',
        'prevterm': 'previous contract length',
        'ageatsigning': 'age at signing',
        'contractnumber': 'career contract count',
        'isresign': 'whether the player is re-signing with the same team',
    }
    if f_lower in exact:
        return exact[f_lower]
    if f_lower.startswith('positioncode'):
        return 'position profile'
    if f_lower.startswith('isresign_'):
        return 'whether the player is re-signing with the same team'

    if f_lower.startswith('mp_2_'):
        sit = f_lower.split('_')[2]
        sit_label = situation_map.get(sit, 'special-teams')
        return f'recent {sit_label} usage volume'
    if f_lower.startswith('dmp_2_'):
        sit = f_lower.split('_')[2]
        sit_label = situation_map.get(sit, 'special-teams')
        return f'change in {sit_label} usage between lookback seasons'

    is_trend = f_lower.startswith('d')
    token = f_lower[1:] if is_trend else f_lower
    if '_2_' not in token:
        family = classify_contract_feature(f)
        family_desc = {
            'Offensive Rates': 'recent offensive rate profile',
            'Offensive Trends': 'recent offensive trend profile',
            'Defensive Rates': 'recent defensive rate profile',
            'Defensive Trends': 'recent defensive trend profile',
            'Usage Levels': 'recent usage profile',
            'Usage Trends': 'recent usage trend profile',
            'Player and Contract History': 'player and contract history',
            'Other': 'term-structure signal',
        }
        return family_desc.get(family, 'contract signal')

    root, right = token.split('_2_', 1)
    sit = right.split('_')[0]
    sit_label = situation_map.get(sit, 'special-teams')
    root_desc = {
        'assists': 'assist production',
        'blocks': 'block activity',
        'giveaways': 'giveaway rate',
        'hits': 'physical engagement',
        'takeaways': 'takeaway rate',
        'x': 'shot-location profile',
        'y': 'shot-location profile',
        'icorsif': 'individual shot-attempt generation',
        'ifenwickf': 'individual unblocked-shot generation',
        'isogf': 'individual shots on goal generation',
        'igf': 'individual goals-for finishing',
        'ixgf': 'individual expected-goals generation',
        'ocorsif': 'on-ice shot-attempt creation',
        'ofenwickf': 'on-ice unblocked-shot creation',
        'osogf': 'on-ice shots-on-goal creation',
        'ogf': 'on-ice goals-for scoring',
        'oxgf': 'on-ice expected-goals creation',
        'ocorsia': 'on-ice shot-attempt suppression',
        'ofenwicka': 'on-ice unblocked-shot suppression',
        'osoga': 'on-ice shots-on-goal suppression',
        'oga': 'on-ice goals-against suppression',
        'oxga': 'on-ice expected-goals-against suppression',
    }
    metric_desc = root_desc.get(root, 'performance signal')
    if is_trend:
        return f'change in {metric_desc} at {sit_label}'
    return f'recent {metric_desc} at {sit_label}'


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

st.title('Contract Projection Model')
st.caption('Published: March 1, 2026')

st.markdown('## I. Introduction')
st.markdown('An NHL player contract is defined by two numbers that carry most of the strategic weight: term determines how long the commitment runs, and AAV determines how much cap space is consumed each season. For cap accounting, AAV is the annualized value of the deal, so term and salary structure together determine the cap hit path that constrains roster decisions year over year. Teams are not just pricing talent; they are buying a time path of risk and flexibility. A short term with high AAV protects optionality but pressures the near-term cap, while longer terms can reduce annual pressure in some structures but increase exposure to aging, role change, and performance variance.')
st.markdown('That tradeoff is amplified by a hard salary cap and by CBA contract rules. In the rule set represented by this training history, teams can sign up to eight years when re-signing their own player and up to seven years for external signings, which is why our scenario grid separates re-sign and non-re-sign branches. The league has ratified a new CBA that reduces those maxima to seven and six years beginning with the 2026-27 CBA window, so this model framing should be read as a snapshot tied to the governing rule regime in the underlying sample and updated as those constraints roll in.')
st.markdown('We currently focus on skaters, which is the active production scope of the pipeline today. Goalie contract modeling is planned but not yet integrated into this page. Our objective here is to explain how the model is built, why uncertainty is represented through scenario structure instead of one point estimate, and how to interpret the outputs in practical decision terms.')

st.markdown('## II. Architecture')

data = load_architecture_data()
if data is None:
    st.warning('Architecture data files are missing. Run `Rscript articles/contracts/build_architecture_data.R` and refresh this page.')
    st.stop()

snapshot = data['snapshot']
feature_families = data['feature_families']
term_distribution = data['term_distribution']
scenario_volume = data['scenario_volume']
term_compare = data['term_compare_by_split']
aavp_compare = data['aavp_compare_by_split']
importance_selected = data['importance_selected']

snapshot_map = dict(zip(snapshot['metric'], snapshot['value']))
train_contracts = snapshot_map.get('Training Contracts', '')
train_skaters = snapshot_map.get('Training Skaters', '')
train_range = snapshot_map.get('Training Season Range', '')
train_predictors = snapshot_map.get('Training Predictors', '')
validate_contracts = snapshot_map.get('Validation Contracts', '')
validate_season = snapshot_map.get('Validation Season', '')
test_rows = snapshot_map.get('Testing Scenario Rows', '')
test_skaters = snapshot_map.get('Testing Skaters', '')
test_rows_per_skater = snapshot_map.get('Testing Rows per Skater', '')

st.markdown('### A. Data')
st.markdown('We build contract-level targets from Spotrac contract records through `nhlscraper::contracts()`, then join skater season statistics and skater shot-analysis outputs from our xG pipeline. The key design decision is leakage control: for a contract starting in season `T`, we use lookback seasons `T-2` and `T-3`, not `T-1`. This avoids pulling in information that may not have existed at signing time when deals are agreed before the upcoming season is played.')
st.markdown(f'That leakage-safe window still preserves directional signal because we keep both level and trend features across offensive, defensive, usage, and player and contract history families. We split usage by hockey situations that map to on-ice role demands: even-strength, power play, and shorthanded. In the current snapshot, we train on {train_contracts} contracts from {train_skaters} skaters across {train_range} with {train_predictors} engineered predictors, hold out {validate_contracts} contracts from {validate_season} for model comparison, and score {test_rows} forward scenario rows for {test_skaters} skaters.')

ff = feature_families.copy()
ff['featureFamily'] = ff['featureFamily'].replace(
    {
        'Usage Levels': 'Usage Levels and Trends',
        'Usage Trends': 'Usage Levels and Trends',
        'Player and Contract Context': 'Player and Contract History',
    }
)
ff = ff.groupby('featureFamily', as_index=False)['count'].sum()
ff = ff.sort_values('count', ascending=True).copy()
fig_families = go.Figure(
    go.Bar(
        x=ff['count'],
        y=ff['featureFamily'],
        orientation='h',
        marker_color='#2E86C1',
        hovertemplate='Feature Family: %{y}<br>Count: %{x}<extra></extra>',
        showlegend=False,
    )
)
fig_families.update_layout(
    title=dict(text='Predictor Mix by Feature Family', x=0.5, xanchor='center'),
    margin=dict(l=10, r=10, t=55, b=35),
    xaxis=dict(title='Number of Predictors'),
    yaxis=dict(title=None),
    height=360,
)
fig_families.update_xaxes(fixedrange=True)
fig_families.update_yaxes(fixedrange=True)
st.plotly_chart(fig_families, width='stretch', config={'displayModeBar': True})

feature_examples = pd.DataFrame(
    [
        {
            'Feature Family': 'Offensive Rates',
            'Description': 'Recent two-season scoring and chance-generation pace, including on-ice offense.',
        },
        {
            'Feature Family': 'Offensive Trends',
            'Description': 'Change in offensive pace from prior season to recent season, controlling for minutes.',
        },
        {
            'Feature Family': 'Defensive Rates',
            'Description': 'Suppression and against-profile indicators while the skater is on ice.',
        },
        {
            'Feature Family': 'Defensive Trends',
            'Description': 'Direction of defensive impact and shot-against exposure heading into the contract window.',
        },
        {
            'Feature Family': 'Usage Levels and Trends',
            'Description': 'Role size by situation and whether that role is expanding or contracting.',
        },
        {
            'Feature Family': 'Player and Contract History',
            'Description': 'Lifecycle, prior deal baseline, and market context entering the new negotiation.',
        },
    ]
)
st.dataframe(feature_examples, hide_index=True, width='stretch')
st.markdown('Feature aggregation is built to preserve both volume context and direction of change. For rate-based variables, we compute a minutes-weighted per-60 average across the two leakage-safe seasons (`T-2` and `T-3`) using `60 * (stat_T-2 + stat_T-3) / (minutes_T-2 + minutes_T-3)` when valid minutes exist, and default to zero otherwise. Trend variables are then computed as `per60_T-2 - per60_T-3`, so positive values indicate upward movement entering the contract window. Usage averages are computed from valid-season minutes only, with usage deltas as `minutes_T-2 - minutes_T-3`.')

st.markdown('### B. Term-AAV% Split')
st.markdown('The modeling target is split into two linked tasks: term is estimated as a probability distribution over candidate lengths, and compensation is estimated as cap-relative salary (`AAV%`). This split is central to how uncertainty is handled. A single combined point estimate can look precise while hiding structural error, but contract economics are highly sensitive to the term branch; if the branch is wrong, the implied salary interpretation is usually wrong as well.')
st.markdown('By separating the tasks, the output becomes a scenario surface instead of one line item. Decision-makers can evaluate which term branches are plausible, how salary expectations change across those branches, and where cap risk concentrates. This is closer to real negotiation logic, where teams balance downside and flexibility across multiple feasible structures rather than optimizing a singular deterministic one.')

term_distribution['term'] = term_distribution['term'].astype(int)
term_distribution['sharePct'] = term_distribution['sharePct'].astype(float)
term_order = sorted(term_distribution['term'].unique().tolist())
scenario_order = ['Re-sign', 'Not Re-sign']
term_colors = {
    1: '#1F77B4',
    2: '#2A9D8F',
    3: '#4CAF50',
    4: '#F4A261',
    5: '#E76F51',
    6: '#D62828',
    7: '#8D5A97',
    8: '#5E60CE',
}

render_term_legend({k: term_colors[k] for k in term_order if k in term_colors})

fig_term_mix = make_subplots(
    rows=1,
    cols=2,
    specs=[[{'type': 'domain'}, {'type': 'domain'}]],
    subplot_titles=('Re-sign', 'Not Re-sign'),
)
for idx, scenario in enumerate(scenario_order, start=1):
    d = term_distribution.loc[term_distribution['isResign'] == scenario].copy()
    if d.empty:
        continue
    d = d.sort_values('term')
    d['termLabel'] = d['term'].map(lambda x: f'Term {x}')
    d['labelText'] = d['sharePct'].map(lambda x: f'{x:.0f}%' if x >= 4.0 else '')
    colors = [term_colors.get(int(t), '#6C757D') for t in d['term']]
    fig_term_mix.add_trace(
        go.Pie(
            labels=d['termLabel'],
            values=d['contracts'],
            text=d['labelText'],
            hole=0.58,
            sort=False,
            direction='clockwise',
            marker=dict(colors=colors),
            textinfo='text',
            textfont=dict(color='white', size=13),
            hovertemplate=(
                f'Scenario: {scenario}<br>'
                '%{label}<br>'
                'Share Within Scenario: %{percent}<br>'
                'Historical Contracts: %{value:,.0f}<extra></extra>'
            ),
            showlegend=False,
        ),
        row=1,
        col=idx,
    )

fig_term_mix.update_layout(
    title=dict(text='Historical Term Mix by Scenario Type', x=0.5, xanchor='center'),
    margin=dict(l=10, r=10, t=70, b=25),
    height=400,
)
st.plotly_chart(fig_term_mix, width='stretch', config={'displayModeBar': True})

st.markdown('### C. Re-sign vs. Not Re-sign')
st.markdown('Re-sign status changes the feasible contract space because CBA maximum term is tied to signing context. In this pipeline, scenario generation enforces a different term ceiling by branch: re-sign cases evaluate terms 1 through 8, while non-re-sign cases evaluate terms 1 through 7. The non-re-sign branch represents open-market contexts, including unrestricted free-agent additions and external negotiations where incumbent rights do not apply. That is a hard structural guardrail, not a soft preference, and it prevents the model from proposing open-market structures that are not valid under the rule set encoded in the training sample.')
st.markdown('From a hockey operations perspective, this distinction reflects a real negotiation asymmetry: an incumbent team can offer one additional year of security relative to outside bidders, and that extra year can materially change both AAV negotiation range and player preference. Our branching therefore encodes a CBA-driven option value, not just a statistical split.')
st.markdown('This matters for uncertainty interpretation because branch constraints directly shape the candidate set that term probabilities and salary estimates are applied to. The model is not only learning historical frequencies; it is being asked to allocate uncertainty across a rule-consistent scenario grid, which keeps outputs aligned with real negotiation constraints.')

scenario_volume_display = scenario_volume.copy()
scenario_volume_display['rowsPerPlayer'] = scenario_volume_display['rowsPerPlayer'].map(lambda x: f'{x:.0f}')
scenario_volume_display = scenario_volume_display.rename(
    columns={
        'scenario': 'Scenario Type',
        'rows': 'Scenario Rows',
        'players': 'Skaters',
        'uniqueTerms': 'Available Terms',
        'rowsPerPlayer': 'Rows per Skater',
    }
)
st.dataframe(
    scenario_volume_display[['Scenario Type', 'Skaters', 'Available Terms', 'Scenario Rows', 'Rows per Skater']],
    hide_index=True,
    width='stretch',
)

st.markdown('### D. Versioning')
st.markdown('We version the full training pipeline so model-family choices remain testable rather than fixed by assumption. Each version uses the same feature set and preprocessing recipe but swaps the learning algorithm for both tasks, which makes comparisons interpretable and straightforward.')

version_table = pd.DataFrame(
    [
        {
            'Version': 'V1',
            'Term Model': 'Random Forest Classifier',
            'AAV% Model': 'Random Forest Regressor',
            'Purpose': 'Tree-bagging baseline with strong variance reduction and stable tabular behavior.',
        },
        {
            'Version': 'V2',
            'Term Model': 'XGBoost Classifier',
            'AAV% Model': 'XGBoost Regressor',
            'Purpose': 'Gradient-boosted tree benchmark with strong nonlinear fit capacity.',
        },
        {
            'Version': 'V3',
            'Term Model': 'LightGBM Classifier',
            'AAV% Model': 'LightGBM Regressor',
            'Purpose': 'Alternative boosted-tree implementation optimized for histogram-based splits.',
        },
    ]
)
st.dataframe(version_table, hide_index=True, width='stretch')

st.markdown('## III. Validation')

st.markdown('### A. Metrics')
st.markdown(f'We evaluate model families on two roles: `Seen` contracts (the in-sample training set used during fitting) and `Unseen Future` contracts ({validate_contracts} held out from {validate_season}) that were never used for model training. Comparing both roles matters because it separates fit capacity from forward robustness. For term classification we use multiclass log loss, and for AAV% regression we use MSE, with lower values indicating better performance for both metrics.')

validation_scope = pd.DataFrame(
    [
        {
            'Evaluation Role': 'Seen',
            'Contracts': train_contracts,
            'Term Metric': 'Multiclass Log Loss',
            'AAV% Metric': 'MSE',
        },
        {
            'Evaluation Role': 'Unseen Future',
            'Contracts': validate_contracts,
            'Term Metric': 'Multiclass Log Loss',
            'AAV% Metric': 'MSE',
        },
    ]
)
st.dataframe(validation_scope, hide_index=True, width='stretch')

st.markdown('### B. Results by Model Family')

split_colors = {
    'Seen': '#2E86C1',
    'Unseen Future': '#E67E22',
}

st.markdown(
    '''
<div style="display:flex; flex-wrap:wrap; gap:8px 16px; margin:0.2rem 0 0.8rem 0; font-size:0.95rem;">
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#2E86C1; display:inline-block;"></span>Seen</span>
  <span style="display:inline-flex; align-items:center; gap:6px;"><span style="width:11px; height:11px; background:#E67E22; display:inline-block;"></span>Unseen Future</span>
</div>
''',
    unsafe_allow_html=True,
)

chart_col1, chart_col2 = st.columns(2, gap='small', vertical_alignment='top')

with chart_col1:
    d_term = term_compare.copy()
    fig_term_metric = go.Figure()
    for split in ['Seen', 'Unseen Future']:
        d_split = d_term.loc[d_term['split'] == split].copy()
        if d_split.empty:
            continue
        fig_term_metric.add_trace(
            go.Bar(
                x=d_split['candidate'],
                y=d_split['logLoss'],
                marker_color=split_colors.get(split, '#6C757D'),
                customdata=d_split[['accuracy', 'rank']].to_numpy(),
                name=split,
                hovertemplate=(
                    'Role: ' + split + '<br>'
                    'Candidate: %{x}<br>'
                    'Multiclass Log Loss: %{y:.3f}<br>'
                    'Accuracy: %{customdata[0]:.3f}<br>'
                    'Rank Within Role: %{customdata[1]:.0f}<extra></extra>'
                ),
                showlegend=False,
            )
        )
    fig_term_metric.update_layout(
        title=dict(text='Term Model Comparison', x=0.5, xanchor='center'),
        barmode='group',
        margin=dict(l=10, r=10, t=55, b=55),
        xaxis=dict(title='Candidate'),
        yaxis=dict(title='Multiclass Log Loss'),
        height=390,
    )
    fig_term_metric.update_xaxes(fixedrange=True)
    fig_term_metric.update_yaxes(fixedrange=True)
    st.plotly_chart(fig_term_metric, width='stretch', config={'displayModeBar': True})

with chart_col2:
    d_aavp = aavp_compare.copy()
    fig_aavp_metric = go.Figure()
    for split in ['Seen', 'Unseen Future']:
        d_split = d_aavp.loc[d_aavp['split'] == split].copy()
        if d_split.empty:
            continue
        fig_aavp_metric.add_trace(
            go.Bar(
                x=d_split['candidate'],
                y=d_split['mse'],
                marker_color=split_colors.get(split, '#6C757D'),
                customdata=d_split[['rmse', 'mae', 'rank']].to_numpy(),
                name=split,
                hovertemplate=(
                    'Role: ' + split + '<br>'
                    'Candidate: %{x}<br>'
                    'MSE: %{y:.6f}<br>'
                    'RMSE: %{customdata[0]:.4f}<br>'
                    'MAE: %{customdata[1]:.4f}<br>'
                    'Rank Within Role: %{customdata[2]:.0f}<extra></extra>'
                ),
                showlegend=False,
            )
        )
    fig_aavp_metric.update_layout(
        title=dict(text='AAV% Model Comparison', x=0.5, xanchor='center'),
        barmode='group',
        margin=dict(l=10, r=10, t=55, b=55),
        xaxis=dict(title='Candidate'),
        yaxis=dict(title='MSE'),
        height=390,
    )
    fig_aavp_metric.update_xaxes(fixedrange=True)
    fig_aavp_metric.update_yaxes(fixedrange=True)
    st.plotly_chart(fig_aavp_metric, width='stretch', config={'displayModeBar': True})

best_term_seen = term_compare.loc[term_compare['split'] == 'Seen'].sort_values('rank').iloc[0]
best_term_unseen = term_compare.loc[term_compare['split'] == 'Unseen Future'].sort_values('rank').iloc[0]
best_aavp_seen = aavp_compare.loc[aavp_compare['split'] == 'Seen'].sort_values('rank').iloc[0]
best_aavp_unseen = aavp_compare.loc[aavp_compare['split'] == 'Unseen Future'].sort_values('rank').iloc[0]
st.markdown(
    f'On seen data, XGBoost is strongest for both tasks (term log loss {best_term_seen["logLoss"]:.3f}, AAV% MSE {best_aavp_seen["mse"]:.6f}), which is consistent with high-capacity boosted trees fitting rich tabular structure. On unseen-future contracts, the winning pair changes for term while staying stable for salary: LightGBM leads term (log loss {best_term_unseen["logLoss"]:.3f}) and XGBoost remains best for AAV% (MSE {best_aavp_unseen["mse"]:.6f}). This split behavior is exactly why we treat forward validation as the primary selector and do not pick models only from in-sample fit.')

st.markdown('## IV. Interpretation')
st.markdown('After selecting model families through validation, interpretation asks what information each selected model is actually using. We use gain-based feature importance from the fitted production pair (LightGBM term classifier and XGBoost AAV% regressor). Gain indicates how much each feature contributes to objective reduction across splits; it is a measure of model reliance, not causal effect. Reading both tasks together is useful because term and salary can depend on overlapping but not identical signals.')

if importance_selected.empty:
    st.warning('Feature-importance data is missing. Run `Rscript articles/contracts/build_architecture_data.R` and refresh this page.')
else:
    task_tabs = st.tabs(['Term Model', 'AAV% Model'])
    for task_label, tab in zip(['Term', 'AAV%'], task_tabs):
        with tab:
            d_task = importance_selected.loc[importance_selected['task'] == task_label].copy()
            if d_task.empty:
                st.info('No importance rows found for this task.')
                continue

            d_task = d_task.sort_values('gain', ascending=False).copy()
            top_features = d_task.head(20).iloc[::-1].copy()
            top_features['gainSharePct'] = top_features['gainShare'] * 100.0

            family_share = d_task.copy()
            family_share['Feature Family'] = family_share['feature'].apply(classify_contract_feature)
            if task_label == 'AAV%':
                family_share['Feature Family'] = family_share['Feature Family'].replace({'Other': 'Term'})
            family_share = (
                family_share.groupby('Feature Family', as_index=False)['gainShare']
                .sum()
                .sort_values('gainShare', ascending=False)
            )
            family_share['gainSharePct'] = family_share['gainShare'] * 100.0

            col_imp1, col_imp2 = st.columns(2, gap='small', vertical_alignment='top')

            with col_imp1:
                fig_imp = go.Figure(
                    go.Bar(
                        x=top_features['gainSharePct'],
                        y=top_features['feature'],
                        orientation='h',
                        marker_color='#2E86C1',
                        customdata=top_features[['rank']].to_numpy(),
                        hovertemplate=(
                            'Feature: %{y}<br>'
                            'Gain Share: %{x:.2f}%<br>'
                            'Global Rank: %{customdata[0]:.0f}<extra></extra>'
                        ),
                        showlegend=False,
                    )
                )
                fig_imp.update_layout(
                    title=dict(text=f'Top Feature Importance ({task_label})', x=0.5, xanchor='center'),
                    margin=dict(l=10, r=10, t=50, b=35),
                    xaxis=dict(title='Gain Share (%)'),
                    yaxis=dict(title='Feature'),
                    height=460,
                )
                fig_imp.update_xaxes(fixedrange=True)
                fig_imp.update_yaxes(fixedrange=True)
                st.plotly_chart(fig_imp, width='stretch', config={'displayModeBar': True})

            with col_imp2:
                fig_family = go.Figure(
                    go.Bar(
                        x=family_share['Feature Family'],
                        y=family_share['gainSharePct'],
                        marker_color='#16A085',
                        hovertemplate='Feature Family: %{x}<br>Gain Share: %{y:.2f}%<extra></extra>',
                        showlegend=False,
                    )
                )
                fig_family.update_layout(
                    title=dict(text=f'Importance Concentration by Family ({task_label})', x=0.5, xanchor='center'),
                    margin=dict(l=10, r=10, t=50, b=80),
                    xaxis=dict(title='Feature Family'),
                    yaxis=dict(title='Gain Share (%)'),
                    height=460,
                )
                fig_family.update_xaxes(fixedrange=True)
                fig_family.update_yaxes(fixedrange=True)
                st.plotly_chart(fig_family, width='stretch', config={'displayModeBar': True})

            top3_descriptions = []
            for raw_feature in d_task.head(3)['feature'].tolist():
                desc = describe_contract_feature(raw_feature)
                if desc not in top3_descriptions:
                    top3_descriptions.append(desc)
            top_family = family_share.iloc[0]
            st.markdown(
                f"For the {task_label} model, the strongest signals come from {', '.join(top3_descriptions)}. At the family level, the largest share comes from {top_family['Feature Family']} ({top_family['gainSharePct']:.1f}%), which helps explain where the model is concentrating its signal."
            )

st.markdown('## V. Conclusion')
st.markdown('We built this contract-projection system as a connected workflow rather than a single model run: leakage-safe historical windows define the input frame, scenario branching separates term uncertainty from salary uncertainty, versioned training keeps model-family choices testable, and seen versus unseen-future validation is used to choose the production pair. A practical takeaway is that contract projection is most useful when presented as structured uncertainty over valid options, not as a deterministic number.')
st.markdown('There are still important limitations we should improve in future versions. With the current setup, projected AAV% tends to rise as the fixed term increases, but real negotiations often include a tradeoff where players accept lower annual cap share in exchange for longer security. Our pooled training objective is likely learning the broad historical pattern that stronger players both earn longer terms and higher AAV%, while missing that conditional term-versus-annual-value tradeoff within comparable player tiers. A practical next experiment is to retrain the salary component on a term-conditioned subset (for example, excluding 1-year and 2-year deals) and compare whether the term-AAV% relationship becomes more realistic under longer-horizon negotiation contexts.')
st.markdown('The current feature set also does not yet encode team-relative context, such as how strong a player is relative to internal alternatives on the same roster, nor does it explicitly include team cap-state pressure at signing. We also do not yet model injury and availability history as dedicated risk signals, even though those factors can materially change both term appetite and salary outcomes. Finally, as mentioned in the introduction, this page currently covers skaters only; goalie contracts are not yet modeled in this framework and remain a planned expansion.')
st.markdown('Summarized outputs from this model are available in the following page:')
st.page_link('views/skater_free_agents.py', label='Skater Free Agents', icon=':material/readiness_score:')
