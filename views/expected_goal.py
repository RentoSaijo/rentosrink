from pathlib import Path
import re

import pandas as pd
import plotly.graph_objects as go
import streamlit as st


DATA_DIR = Path("articles/xG/data")
CURRENT_DATA_SCRIPT = "Rscript articles/xG/build_current_model_data.R"
DATASET_ORDER = [
    "Standard 5v5",
    "Non-Standard Even Strength",
    "Power Play",
    "Shorthanded",
    "Empty Net",
    "Penalty Shot",
]
ENGINE_COLORS = {
    "XGBoost": "#2E86C1",
    "LightGBM": "#1E8449",
    "Hybrid V3": "#CA6F1E",
}
OVERALL_LABEL_COLORS = {
    "All XGBoost": ENGINE_COLORS["XGBoost"],
    "All LightGBM": ENGINE_COLORS["LightGBM"],
    "Hybrid V3": ENGINE_COLORS["Hybrid V3"],
}
DATASET_COLORS = {
    "Standard 5v5": "#2E86C1",
    "Non-Standard Even Strength": "#5D6D7E",
    "Power Play": "#D68910",
    "Shorthanded": "#A93226",
    "Empty Net": "#148F77",
    "Penalty Shot": "#6C3483",
}
CHART_LABELS = {
    "Standard 5v5": "Standard<br>5v5",
    "Non-Standard Even Strength": "Other Even<br>Strength",
    "Power Play": "Power<br>Play",
    "Shorthanded": "Short-<br>Handed",
    "Empty Net": "Empty<br>Net",
    "Penalty Shot": "Penalty<br>Shot",
}
TAB_LABELS = {
    "Standard 5v5": "5v5",
    "Non-Standard Even Strength": "Other EV",
    "Power Play": "PP",
    "Shorthanded": "SH",
    "Empty Net": "EN",
    "Penalty Shot": "PS",
}
DEFAULT_COLOR_ORDER = ["#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A", "#19D3F3"]
ARCHIVED_V4_UNSEEN_FUTURE = pd.DataFrame(
    [
        {
            "Scope": "Standard 5v5",
            "Archived Model": "Previous Suite V4",
            "Archived Log Loss": 0.20087,
            "Archived Brier": 0.05362,
            "Archived ROC AUC": 0.778439,
            "Archived Calibration Ratio": 0.985,
        },
        {
            "Scope": "All Situations",
            "Archived Model": "Previous Suite V4",
            "Archived Log Loss": 0.22617,
            "Archived Brier": 0.06187,
            "Archived ROC AUC": 0.772341,
            "Archived Calibration Ratio": 0.959,
        },
    ]
)


def legend_row(items):
    html = [
        '<div style="display:flex; flex-wrap:wrap; gap:8px 16px; margin:0.2rem 0 0.8rem 0; font-size:0.95rem;">'
    ]
    for label, color in items:
        html.append(
            f'<span style="display:inline-flex; align-items:center; gap:6px;">'
            f'<span style="width:11px; height:11px; background:{color}; display:inline-block;"></span>{label}'
            f"</span>"
        )
    html.append("</div>")
    st.markdown("".join(html), unsafe_allow_html=True)


def format_decimal(value, digits=5):
    return f"{value:.{digits}f}"


def clean_feature_label(feature_name):
    mapping = {
        "distance": "Distance",
        "angle": "Angle",
        "xCoordNorm": "X Coordinate (Normalized)",
        "yCoordNorm": "Y Coordinate (Normalized)",
        "dXCoordNorm": "Delta X Coordinate",
        "dYCoordNorm": "Delta Y Coordinate",
        "dDistance": "Delta Distance",
        "dAngle": "Delta Angle",
        "dSecondsElapsedInSequence": "Delta Seconds in Sequence",
        "dXCoordNormPerSecond": "Delta X Coordinate per Second",
        "dYCoordNormPerSecond": "Delta Y Coordinate per Second",
        "dDistancePerSecond": "Delta Distance per Second",
        "dAnglePerSecond": "Delta Angle per Second",
        "secondsElapsedInSequence": "Seconds in Sequence",
        "minSecondsElapsedInShiftFor": "Min On-Ice Shift Time (For)",
        "maxSecondsElapsedInShiftFor": "Max On-Ice Shift Time (For)",
        "avgSecondsElapsedInShiftFor": "Average On-Ice Shift Time (For)",
        "minSecondsElapsedInShiftAgainst": "Min On-Ice Shift Time (Against)",
        "maxSecondsElapsedInShiftAgainst": "Max On-Ice Shift Time (Against)",
        "avgSecondsElapsedInShiftAgainst": "Average On-Ice Shift Time (Against)",
        "shooterSecondsElapsedInShift": "Shooter Shift Time",
        "shooterSecondsElapsedSinceLastShift": "Shooter Rest Since Last Shift",
        "minSecondsElapsedSinceLastShiftFor": "Min Rest Since Last Shift (For)",
        "maxSecondsElapsedSinceLastShiftFor": "Max Rest Since Last Shift (For)",
        "avgSecondsElapsedSinceLastShiftFor": "Average Rest Since Last Shift (For)",
        "minSecondsElapsedSinceLastShiftAgainst": "Min Rest Since Last Shift (Against)",
        "maxSecondsElapsedSinceLastShiftAgainst": "Max Rest Since Last Shift (Against)",
        "avgSecondsElapsedSinceLastShiftAgainst": "Average Rest Since Last Shift (Against)",
        "isRebound_yes": "Rebound Flag",
        "isRush_yes": "Rush Flag",
        "shotType_tip.in": "Shot Type: Tip-In",
        "shotType_snap": "Shot Type: Snap",
        "shotType_wrist": "Shot Type: Wrist",
        "shotType_slap": "Shot Type: Slap",
        "shotType_backhand": "Shot Type: Backhand",
        "shotType_deflected": "Shot Type: Deflected",
        "zoneCode_O": "Zone Code: Offensive",
        "zoneCode_N": "Zone Code: Neutral",
        "zoneCode_D": "Zone Code: Defensive",
        "goalsFor": "Goals For",
        "goalsAgainst": "Goals Against",
        "goalDifferential": "Goal Differential",
        "shotsFor": "Shots For",
        "shotsAgainst": "Shots Against",
        "shotDifferential": "Shot Differential",
        "fenwickFor": "Fenwick For",
        "fenwickAgainst": "Fenwick Against",
        "fenwickDifferential": "Fenwick Differential",
        "corsiFor": "Corsi For",
        "corsiAgainst": "Corsi Against",
        "corsiDifferential": "Corsi Differential",
        "goalieWeight": "Goalie Weight",
        "goalieAge": "Goalie Age",
        "shooterWeight": "Shooter Weight",
        "shooterAge": "Shooter Age",
        "shooterHandCode_R": "Shooter Hand: Right",
        "shooterHandCode_L": "Shooter Hand: Left",
    }
    if feature_name in mapping:
        return mapping[feature_name]

    label = str(feature_name).replace(".", "-")
    label = re.sub(r"([a-z0-9])([A-Z])", r"\1 \2", label)
    label = label.replace("_", " ")
    label = label.replace("Norm", "Normalized")
    label = label.title()
    label = label.replace("X Coord", "X Coordinate")
    label = label.replace("Y Coord", "Y Coordinate")
    label = label.replace("Id", "ID")
    return label


def feature_family(feature_name):
    f = str(feature_name).lower()
    if (
        f in {"distance", "angle", "xcoordnorm", "ycoordnorm", "isbehindnet"}
        or f.startswith("shottype")
        or f.startswith("zonecode")
    ):
        return "Shot Geometry"
    if (
        f.startswith("d")
        or f in {"secondselapsedinsequence", "typedesckeyprev", "isrush_yes", "isrebound_yes", "crossedroyalroad"}
    ):
        return "Pre-Shot Movement"
    if "shift" in f or "sincelastshift" in f:
        return "Shift and Rest"
    if (
        f in {"ishome_yes", "isplayoff_yes", "isovertime_yes", "periodnumber"}
        or "strengthstate" in f
        or "skatercount" in f
        or "mandifferential" in f
        or "goals" in f
        or "shots" in f
        or "fenwick" in f
        or "corsi" in f
        or "isemptynetfor" in f
    ):
        return "Game State"
    if f.startswith("shooter") or f.startswith("goalie"):
        return "Player Profile"
    return "Other"


@st.cache_data(show_spinner=False)
def load_current_model_data():
    required_paths = {
        "training": DATA_DIR / "current_training_summary.csv",
        "overall": DATA_DIR / "current_unseen_future_overall.csv",
        "partition": DATA_DIR / "current_unseen_future_by_partition.csv",
        "importance": DATA_DIR / "current_feature_importance.csv",
    }

    missing = [str(path) for path in required_paths.values() if not path.exists()]
    if missing:
        return None, missing

    training = pd.read_csv(required_paths["training"])
    overall = pd.read_csv(required_paths["overall"])
    partition = pd.read_csv(required_paths["partition"])
    importance = pd.read_csv(required_paths["importance"])
    return {
        "training": training,
        "overall": overall,
        "partition": partition,
        "importance": importance,
    }, []


st.markdown(
    """
    <style>
    div.block-container {
        max-width: 860px;
    }
    </style>
    """,
    unsafe_allow_html=True,
)

st.title("Expected Goal (xG) Model")
st.caption("Published: March 15, 2026")

st.markdown("## I. Introduction")
st.markdown(
    """
Expected goals, or **xG**, is a probability estimate for a single shot event: given the information available at release, what is the chance this shot becomes a goal? Once each shot has a probability, those estimates can be summed to describe chance quality across players, teams, games, and seasons. That framing matters because hockey is noisy. Goals are relatively rare, and short stretches are shaped by finishing runs, goaltending streaks, and randomness. Shot counts alone miss that context. A harmless perimeter attempt and a dangerous slot chance both count as one shot, but they are not equally threatening. xG measures that difference directly. Used properly, xG does not compete with goals; it complements them. Goals tell us what happened, while xG helps explain what was likely to happen based on the chances that were created and allowed.

Historically, the expected-goals framework grew first in soccer, where analysts formalized shot-quality modeling from event data. Hockey adopted the same probabilistic logic and adapted it to a faster, more state-dependent game. As data quality improved, methods moved from simple interpretable baselines to more flexible nonlinear approaches, but the objective stayed the same: produce probabilities that are informative, stable, and well-calibrated. Calibration is critical because a model can rank chances well yet still be mis-scaled in absolute probability terms, making totals and comparisons less reliable. Good xG work is therefore not just model choice; it is disciplined data handling, explicit assumptions, and rigorous testing. We follow that philosophy from foundations through implementation, evaluation, interpretation, and limitations so the model is understood as a full system, not a black-box number.
"""
)

model_data, _ = load_current_model_data()
if model_data is None:
    st.warning(
        "Current xG article data is missing. Run "
        f"`{CURRENT_DATA_SCRIPT}` to regenerate the supporting CSVs."
    )
    st.stop()

training_df = model_data["training"].copy()
overall_df = model_data["overall"].copy()
partition_df = model_data["partition"].copy()
importance_df = model_data["importance"].copy()

training_df["datasetLabel"] = pd.Categorical(training_df["datasetLabel"], categories=DATASET_ORDER, ordered=True)
training_df = training_df.sort_values("datasetLabel").reset_index(drop=True)
training_df["rowShare"] = training_df["rows"] / training_df["rows"].sum()
training_df["goalRatePct"] = training_df["goal_rate"] * 100
training_df["chartLabel"] = training_df["datasetLabel"].astype(str).map(CHART_LABELS)

overall_df["label"] = pd.Categorical(
    overall_df["label"],
    categories=["All XGBoost", "All LightGBM", "Hybrid V3"],
    ordered=True,
)
overall_df = overall_df.sort_values("label").reset_index(drop=True)

partition_df["datasetLabel"] = pd.Categorical(partition_df["datasetLabel"], categories=DATASET_ORDER, ordered=True)
partition_df["label"] = pd.Categorical(
    partition_df["label"],
    categories=["XGBoost", "LightGBM", "Hybrid V3"],
    ordered=True,
)
partition_df = partition_df.sort_values(["datasetLabel", "label"]).reset_index(drop=True)
partition_df["chartLabel"] = partition_df["datasetLabel"].astype(str).map(CHART_LABELS)

importance_df["datasetLabel"] = pd.Categorical(importance_df["datasetLabel"], categories=DATASET_ORDER, ordered=True)
importance_df = importance_df.sort_values(["datasetLabel", "rank"]).reset_index(drop=True)
importance_df["featureLabel"] = importance_df["feature"].apply(clean_feature_label)
importance_df["family"] = importance_df["feature"].apply(feature_family)

chosen_models = training_df[["dataset", "datasetLabel", "model", "engineLabel"]].copy()
chosen_models = chosen_models.rename(columns={"engineLabel": "Engine"})
chosen_models["datasetLabel"] = chosen_models["datasetLabel"].astype(str)

engine_partition_df = partition_df.loc[partition_df["label"].astype(str).isin(["XGBoost", "LightGBM"])].copy()
engine_partition_df["datasetLabel"] = engine_partition_df["datasetLabel"].astype(str)

engine_choice_rows = []
for dataset in ["sd", "ev", "pp", "sh", "en", "ps"]:
    d = engine_partition_df.loc[engine_partition_df["dataset"] == dataset].sort_values("log_loss").reset_index(drop=True)
    best = d.iloc[0]
    other = d.iloc[1]
    chosen = chosen_models.loc[chosen_models["dataset"] == dataset].iloc[0]
    engine_choice_rows.append(
        {
            "dataset": dataset,
            "datasetLabel": best["datasetLabel"],
            "chartLabel": CHART_LABELS[best["datasetLabel"]],
            "model": chosen["model"],
            "engine": chosen["Engine"],
            "chosenLogLoss": best["log_loss"],
            "otherEngine": other["label"],
            "otherLogLoss": other["log_loss"],
            "edge": other["log_loss"] - best["log_loss"],
        }
    )

engine_choice_df = pd.DataFrame(engine_choice_rows)

st.markdown("## II. What Changed")

st.markdown("### A. Overview")
st.markdown(
    "The archived February 28, 2026 article described a V1-V4 feature-layer experiment. That framing was useful while the model family was still being explored, but it is no longer the clearest description of the system we actually deploy. This new article is organized around the live model family and the downstream shot-by-shot scoring workflow, so it now matches how xG is actually used instead of how an earlier research pass happened to be narrated."
)
st.markdown(
    "Several substantive things changed with that shift in framing. First, versioning is no longer presented as a generic V1-through-V4 feature ladder. It now follows operational generations and season coverage, because that is how the system is truly maintained: legacy ridge generations for older historical backfills, and a new hybrid generation built from a combination of XGBoost and LightGBM models for the modern window. Second, shift and rest timing is no longer hypothetical future work. Shooter shift time, shooter rest time, and on-ice min/max/average shift and rest summaries are in the live feature set and show up in the deployed models' importance profiles. Third, the evaluation emphasis is narrower and more honest."
)

st.markdown("### B. Shot Preparation")
st.markdown(
    "The modern prep path starts with `nhlscraper::gc_pbps()`, then adds shift charts, event-to-event deltas, and shooter and goalie biometrics before any partition-specific modeling begins. That matters because the new family is not just a different learner on the same old input table. It now carries explicit shift-time workload context into the feature set, including shooter shift time, shooter rest since last shift, and on-ice skater shift and rest summaries for both teams. Those variables were the clearest missing piece in the archived page, and they are now part of the foundation."
)
st.markdown(
    "The other notable preparation change is now additive rather than subtractive. `missed-shot` rows with `reason == \"short\"` are kept throughout the pipeline instead of being dropped. They still collapse into the existing `other` missed-reason bucket when previous-event context is encoded, so the change is event inclusion rather than a new standalone reason level. The practical consequence is consistency: the same inclusion now applies in training, testing, comparison, and published shot-by-shot scoring."
)

st.markdown("## III. Architecture")

st.markdown("### A. Six-Way Partitioning")
partition_table = pd.DataFrame(
    [
        {
            "Partition": "Standard 5v5",
            "Rule": "5 skaters against 5 skaters, both goalies in net, not a penalty shot.",
            "Deployed Model": "sd1 (XGBoost)",
        },
        {
            "Partition": "Non-Standard Even Strength",
            "Rule": "Same skater count, non-empty-net, non-penalty-shot, but not standard 5v5.",
            "Deployed Model": "ev2 (LightGBM)",
        },
        {
            "Partition": "Power Play",
            "Rule": "Shooter's team has a skater advantage and the defending goalie is still in net.",
            "Deployed Model": "pp1 (XGBoost)",
        },
        {
            "Partition": "Shorthanded",
            "Rule": "Shooter's team has fewer skaters than the defending team and the net is not empty.",
            "Deployed Model": "sh2 (LightGBM)",
        },
        {
            "Partition": "Empty Net",
            "Rule": "The shooting team is attacking an empty net, regardless of other manpower details.",
            "Deployed Model": "en2 (LightGBM)",
        },
        {
            "Partition": "Penalty Shot",
            "Rule": "`situationCode` is `0101` or `1010`; the `ps` model is still trained on both penalty-shot and shootout-style rows.",
            "Deployed Model": "ps1 (XGBoost)",
        },
    ]
)
st.dataframe(partition_table, hide_index=True, width="stretch")

total_training_rows = int(training_df["rows"].sum())
sd_share = float(training_df.loc[training_df["dataset"] == "sd", "rowShare"].iloc[0])
pp_share = float(training_df.loc[training_df["dataset"] == "pp", "rowShare"].iloc[0])
st.markdown(
    f"The new tree-based family is trained on 2023-24 and 2024-25 and spans {total_training_rows:,} supervised shot events once the six game-state partitions are split apart. Standard 5v5 still dominates volume ({sd_share * 100:.1f}% of rows), power-play shots provide the next-largest slice ({pp_share * 100:.1f}%), and the remaining partitions are much smaller but materially different scoring environments. That asymmetry is precisely why the project keeps separate estimators instead of forcing one model to span all contexts."
)
fig_rows = go.Figure(
    go.Bar(
        x=training_df["chartLabel"],
        y=training_df["rows"],
        marker_color=DEFAULT_COLOR_ORDER[: len(training_df)],
        customdata=training_df[["goalRatePct", "rowShare"]].to_numpy(),
        hovertemplate=(
            "Partition: %{x}<br>"
            "Rows: %{y:,.0f}<br>"
            "Goal Rate: %{customdata[0]:.2f}%<br>"
            "Row Share: %{customdata[1]:.1%}"
            "<extra></extra>"
        ),
        showlegend=False,
    )
)
fig_rows.update_layout(
    title=dict(text="Training Rows by Partition", x=0.5, xanchor="center"),
    margin=dict(l=10, r=10, t=45, b=90),
    xaxis=dict(title="Partition"),
    yaxis=dict(title="Rows"),
    height=390,
    showlegend=False,
)
fig_rows.update_xaxes(fixedrange=True)
fig_rows.update_yaxes(fixedrange=True)
st.plotly_chart(fig_rows, width="stretch", config={"displayModeBar": True})

st.markdown("### B. XGBoost vs. LightGBM")
st.markdown(
    "The new generation uses boosted-tree models because the modern training window is large enough, and rich enough in interaction structure, that linear ridge models would leave meaningful signal untapped. Distance and angle still anchor the problem, but the newer family also has to reconcile movement deltas, pre-shot event context, manpower state, scoreboard state, biometrics, and shift-time workload features. Tree boosting is a practical fit for that mix because it can model nonlinearities and interactions without forcing the project to hand-code an unwieldy number of cross terms."
)
st.markdown(
    "XGBoost and LightGBM were both retained because neither engine wins everywhere. The new training workflow tunes each partition separately and does not blindly accept the most complex candidate; when simpler configurations sit within the one-standard-error band of the raw best result, the workflow prefers the simpler option. From there, the deployment choice is made partition by partition. XGBoost holds the edge for Standard 5v5, Power Play, and Penalty Shot. LightGBM wins for Non-Standard Even Strength, Shorthanded, and Empty Net. The result is a mixed production bundle because the forward evidence says a mixed bundle is better than a single-engine rule."
)
st.markdown(
    "That flexibility is also why redundant variables are not automatically a problem here. In a tree-based model, correlated predictors can still be useful because different splits may exploit similar information at different thresholds or in different interaction paths. Redundancy can diffuse feature-importance rankings and it does not guarantee better generalization, but it is not inherently harmful in the way it would often be for a tightly specified linear model."
)

st.markdown("### C. Predictors")
predictor_table = pd.DataFrame(
    [
        {
            "Feature Family": "Shot Geometry",
            "Examples": "distance, angle, normalized x/y coordinates, shot type, zone code",
            "Role": "Defines the baseline danger profile of the chance at release.",
        },
        {
            "Feature Family": "Pre-Shot Movement",
            "Examples": "delta distance, delta angle, delta coordinates, per-second movement rates, previous-event context",
            "Role": "Captures how quickly the chance geometry changed before the shot.",
        },
        {
            "Feature Family": "Shift and Rest Timing",
            "Examples": "shooter shift time, shooter rest time, min/max/average shift and rest summaries for both sides",
            "Role": "Adds workload and fatigue context that the archived article only proposed conceptually.",
        },
        {
            "Feature Family": "Game State",
            "Examples": "score state, shots, Fenwick, Corsi, skater counts, strength state, playoff/overtime flags",
            "Role": "Represents tactical environment and the pressure surrounding the attempt.",
        },
        {
            "Feature Family": "Player Profile",
            "Examples": "shooter and goalie height, weight, handedness, age, position",
            "Role": "Adds descriptive player context without claiming causal skill decomposition.",
        },
    ]
)
st.dataframe(predictor_table, hide_index=True, width="stretch")

st.markdown(
    "The `d*` columns come from `nhlscraper::add_deltas()`. They are not generic moving averages; they are event-to-event deltas and rates computed relative to the prior valid event within the same game and sequence, including `dSecondsElapsedInSequence`, `dDistance`, `dAngle`, `dXCoordNorm`, `dYCoordNorm`, and their per-second counterparts. That means the model now sees not just where a shot was taken, but how quickly the puck environment changed immediately before release."
)
st.markdown(
    "For `isRush` and `isRebound`, the underlying definitions are the same as in the archived article; what changed here is the verification. The wording below now matches the `nhlscraper` implementation directly. In the package's shot-context computation, `isRush` becomes true when a shot attempt occurs within four seconds of the most recent neutral-zone or defensive-zone event in the same game without a stoppage, faceoff, or period boundary reset. `isRebound` becomes true when a shot attempt occurs within three seconds of the same team's most recent `shot-on-goal`, `missed-shot`, or `blocked-shot` source event, again with the context reset by stoppages. Penalty-shot and shootout-style rows are explicitly forced to `FALSE` for both flags."
)
st.markdown(
    "The `ps` partition is intentionally simpler than the other five. It keeps the core spatial, scoreboard, and shooter/goalie profile inputs, but it does not carry the full shift-time and manpower-state feature stack because penalty-shot and shootout-style events are structurally different from in-flow game play. That smaller predictor set is a modeling choice, not a data limitation."
)

st.markdown("### D. Versioning")
version_table = pd.DataFrame(
    [
        {
            "Generation": "Legacy v1 Ridge",
            "Seasons Scored": "2012-13 through 2017-18 directly; 2010-11 and 2011-12 via crossfit only",
            "Training Window": "2010-11 and 2011-12",
            "Model Family": "Ridge logistic regression",
            "Purpose": "Earliest historical backfill layer.",
        },
        {
            "Generation": "Legacy v2 Ridge",
            "Seasons Scored": "2018-19 through 2022-23 directly",
            "Training Window": "2016-17 and 2017-18",
            "Model Family": "Ridge logistic regression",
            "Purpose": "Mid-era historical backfill layer.",
        },
        {
            "Generation": "New v3 Hybrid",
            "Seasons Scored": "2025-26 directly; 2023-24 and 2024-25 via crossfit refits",
            "Training Window": "2023-24 and 2024-25",
            "Model Family": "Per-partition XGBoost / LightGBM blend",
            "Purpose": "New production deployment.",
        },
    ]
)
st.dataframe(version_table, hide_index=True, width="stretch")

st.markdown(
    "This is a more honest use of version numbers than the archived V1-V4 feature ladder. The deployment question is not whether a generic feature layer called V3 or V4 exists in the abstract; it is which model generation can score a given season without pretending away data drift or training leakage. Within the new generation, the project still compares a v1 XGBoost candidate and a v2 LightGBM candidate for every partition, but the production answer is a v3 hybrid that takes whichever engine actually wins for that partition."
)

st.markdown("## IV. Validation")

st.markdown("### A. V3 Selection")
st.markdown(
    "Now, let us observe the prediction results and select the best models for V3. The tabs below show the same six partitions through four evaluation lenses. Log loss remains the main selection criterion, while Brier score, ROC AUC, and calibration ratio help show whether the same engine choice still looks sensible on the other probability-quality summaries."
)

legend_row([("XGBoost", ENGINE_COLORS["XGBoost"]), ("LightGBM", ENGINE_COLORS["LightGBM"])])
selection_tabs = st.tabs(["Log Loss", "Brier", "ROC AUC", "Calibration Ratio"])
selection_metric_specs = [
    ("Log Loss", "log_loss", "Log Loss"),
    ("Brier", "brier", "Brier"),
    ("ROC AUC", "roc_auc", "ROC AUC"),
    ("Calibration Ratio", "calibration_ratio", "Calibration Ratio"),
]

for tab, (metric_label, metric_col, y_axis_title) in zip(selection_tabs, selection_metric_specs):
    with tab:
        fig_partition = go.Figure()
        for label in ["XGBoost", "LightGBM"]:
            d = engine_partition_df.loc[engine_partition_df["label"].astype(str) == label].copy()
            fig_partition.add_trace(
                go.Bar(
                    x=d["chartLabel"],
                    y=d[metric_col],
                    name=label,
                    marker_color=ENGINE_COLORS[label],
                    customdata=d[["log_loss", "brier", "roc_auc", "calibration_ratio"]].to_numpy(),
                    hovertemplate=(
                        "Partition: %{x}<br>"
                        f"Engine: {label}<br>"
                        "Log Loss: %{customdata[0]:.6f}<br>"
                        "Brier: %{customdata[1]:.6f}<br>"
                        "ROC AUC: %{customdata[2]:.6f}<br>"
                        "Calibration Ratio: %{customdata[3]:.6f}"
                        "<extra></extra>"
                    ),
                    showlegend=False,
                )
            )

        fig_partition.update_layout(
            title=dict(text=f"2025-26 {metric_label} by Partition and Engine", x=0.5, xanchor="center"),
            barmode="group",
            margin=dict(l=10, r=10, t=45, b=95),
            xaxis=dict(title="Partition"),
            yaxis=dict(title=y_axis_title),
            height=430,
            showlegend=False,
        )
        fig_partition.update_xaxes(fixedrange=True)
        fig_partition.update_yaxes(fixedrange=True)
        st.plotly_chart(fig_partition, width="stretch", config={"displayModeBar": True})

choice_df = engine_choice_df.copy()
choice_df["2025-26 Log Loss"] = choice_df["chosenLogLoss"].map(lambda x: format_decimal(x, 6))
choice_df["Edge vs Other Engine"] = choice_df["edge"].map(lambda x: format_decimal(x, 6))
choice_df = choice_df.rename(columns={"datasetLabel": "Partition", "model": "Deployed Model", "engine": "Engine"})
st.dataframe(
    choice_df[["Partition", "Deployed Model", "Engine", "2025-26 Log Loss", "Edge vs Other Engine"]],
    hide_index=True,
    width="stretch",
)

choice_edges = []
for _, row in engine_choice_df.iterrows():
    choice_edges.append((row["datasetLabel"], row["edge"]))
choice_edges = sorted(choice_edges, key=lambda x: x[1], reverse=True)
edge_sentence = ", ".join([f"{label}: {edge:.6f}" for label, edge in choice_edges[:3]])
st.markdown(
    f"Using log loss as the selection criterion, the largest 2025-26 engine separations appear in {edge_sentence}. Standard 5v5 is the opposite case: it supplies most of the volume, but the XGBoost versus LightGBM gap there is tiny. The Brier and ROC AUC tabs mostly reinforce the same split-by-partition picture, while the calibration-ratio tab should be read by closeness to 1.000 rather than by raw bar height. Taken together, the evidence still argues for a per-partition deployment map instead of a uniform engine mandate."
)

st.markdown("### B. Overall V1-3 Comparison")

overall_display = overall_df.copy()
overall_display["Deployment"] = overall_display["label"].astype(str)
overall_display["Log Loss"] = overall_display["log_loss"].map(lambda x: format_decimal(x, 6))
overall_display["Brier"] = overall_display["brier"].map(lambda x: format_decimal(x, 6))
overall_display["ROC AUC"] = overall_display["roc_auc"].map(lambda x: format_decimal(x, 6))
overall_display["Calibration Ratio"] = overall_display["calibration_ratio"].map(lambda x: format_decimal(x, 6))
st.dataframe(
    overall_display[["Deployment", "Log Loss", "Brier", "ROC AUC", "Calibration Ratio"]],
    hide_index=True,
    width="stretch",
)

hybrid_row = overall_df.loc[overall_df["label"].astype(str) == "Hybrid V3"].iloc[0]
single_engine = overall_df.loc[overall_df["label"].astype(str).isin(["All XGBoost", "All LightGBM"])].copy()
best_single = single_engine.sort_values("log_loss").iloc[0]
best_brier = single_engine["brier"].min()
best_roc = single_engine["roc_auc"].max()

st.markdown(
    f"Relative to the best single-engine baseline ({best_single['label']}), Hybrid V3 improves 2025-26 log loss by {best_single['log_loss'] - hybrid_row['log_loss']:.6f}, improves Brier score by {best_brier - hybrid_row['brier']:.6f}, and raises ROC AUC by {hybrid_row['roc_auc'] - best_roc:.6f}. The one metric it does not win outright is aggregate calibration distance from 1.000, where the all-XGBoost bundle is marginally closer, so the hybrid should be described as a better event-level scorer rather than as a universal winner on every summary statistic."
)

st.markdown("### C. Comparison with the Archived V4 Suite")
archived_compare = ARCHIVED_V4_UNSEEN_FUTURE.copy()
current_sd = partition_df.loc[
    (partition_df["dataset"] == "sd") & (partition_df["label"].astype(str) == "Hybrid V3")
].iloc[0]
archived_compare["New Deployment"] = ["sd1", "Hybrid V3"]
archived_compare["New Log Loss"] = [current_sd["log_loss"], hybrid_row["log_loss"]]
archived_compare["New Brier"] = [current_sd["brier"], hybrid_row["brier"]]
archived_compare["New ROC AUC"] = [current_sd["roc_auc"], hybrid_row["roc_auc"]]
archived_compare["New Calibration Ratio"] = [current_sd["calibration_ratio"], hybrid_row["calibration_ratio"]]
metric_tabs = st.tabs(["Log Loss", "Brier", "ROC AUC", "Calibration Ratio"])
archived_metric_specs = [
    ("Log Loss", "Archived Log Loss", "New Log Loss", "Value"),
    ("Brier", "Archived Brier", "New Brier", "Value"),
    ("ROC AUC", "Archived ROC AUC", "New ROC AUC", "Value"),
    ("Calibration Ratio", "Archived Calibration Ratio", "New Calibration Ratio", "Ratio"),
]

for tab, (metric_label, archived_col, current_col, y_axis_title) in zip(metric_tabs, archived_metric_specs):
    with tab:
        fig_archived = go.Figure()
        fig_archived.add_trace(
            go.Bar(
                x=archived_compare["Scope"],
                y=archived_compare[archived_col],
                name="Archived V4",
                marker_color="#7F8C8D",
                hovertemplate=(
                    "Scope: %{x}<br>"
                    f"Series: Archived V4<br>{metric_label}: "
                    "%{y:.6f}<extra></extra>"
                ),
                showlegend=False,
            )
        )
        fig_archived.add_trace(
            go.Bar(
                x=archived_compare["Scope"],
                y=archived_compare[current_col],
                name="New Deployment",
                marker_color=ENGINE_COLORS["Hybrid V3"],
                hovertemplate=(
                    "Scope: %{x}<br>"
                    "Series: New Deployment<br>"
                    f"{metric_label}: "
                    "%{y:.6f}<extra></extra>"
                ),
                showlegend=False,
            )
        )
        fig_archived.update_layout(
            title=dict(text=f"Archived V4 vs New Hybrid V3: {metric_label}", x=0.5, xanchor="center"),
            barmode="group",
            margin=dict(l=10, r=10, t=45, b=40),
            xaxis=dict(title="Scope"),
            yaxis=dict(title=y_axis_title),
            height=360,
            showlegend=False,
        )
        fig_archived.update_xaxes(fixedrange=True)
        fig_archived.update_yaxes(fixedrange=True)
        st.plotly_chart(fig_archived, width="stretch", config={"displayModeBar": True})
st.markdown(
    "The archived unseen-future V4 comparison now spans all four deployment metrics that matter here: log loss, Brier score, ROC AUC, and calibration ratio. On both Standard 5v5 and All Situations, the new deployment improves log loss and Brier score relative to the old V4 suites while also posting higher ROC AUC. Calibration ratio moves the other way: the archived V4 suites sat closer to 1.000 than the new deployment in these two preserved comparisons."
)

st.markdown("## V. Interpretation")

st.markdown("### A. Top Predictors")
st.markdown(
    "The saved result objects export the top-20 gain features for each deployed partition. Those are enough to show the broad structure of the new family. Distance remains the dominant signal almost everywhere, which is exactly what a sane xG model should do, but the newer shift-time features now appear high enough in the rankings to matter rather than sitting in a speculative future-work paragraph."
)

tabs = st.tabs([TAB_LABELS[label] for label in DATASET_ORDER])
for dataset_label, tab in zip(DATASET_ORDER, tabs):
    with tab:
        d = importance_df.loc[importance_df["datasetLabel"].astype(str) == dataset_label].copy()
        if d.empty:
            st.info("No feature-importance rows available for this partition.")
            continue

        d = d.sort_values("rank").head(8).copy()
        d = d.iloc[::-1].copy()
        engine_label = d["engineLabel"].iloc[0]
        model_key = d["model"].iloc[0]
        d["gainPct"] = d["gain"] * 100

        fig_imp = go.Figure(
            go.Bar(
                x=d["gainPct"],
                y=d["featureLabel"],
                orientation="h",
                marker=dict(color=ENGINE_COLORS["XGBoost"] if engine_label == "XGBoost" else ENGINE_COLORS["LightGBM"]),
                customdata=d[["rank"]].to_numpy(),
                hovertemplate=(
                    "Feature: %{y}<br>"
                    "Gain: %{x:.2f}%<br>"
                    "Rank: %{customdata[0]:.0f}"
                    "<extra></extra>"
                ),
                showlegend=False,
            )
        )
        fig_imp.update_layout(
            title=dict(text=f"Top Gain Features ({dataset_label}, {model_key})", x=0.5, xanchor="center"),
            margin=dict(l=10, r=10, t=45, b=35),
            xaxis=dict(title="Gain Share (%)"),
            yaxis=dict(title="Feature"),
            height=390,
            showlegend=False,
        )
        fig_imp.update_xaxes(fixedrange=True)
        fig_imp.update_yaxes(fixedrange=True)
        st.plotly_chart(fig_imp, width="stretch", config={"displayModeBar": True})

        top_features = d.sort_values("rank").head(3)["featureLabel"].tolist()
        shift_features = d.loc[d["family"] == "Shift and Rest", "featureLabel"].tolist()
        if shift_features:
            st.markdown(
                f"The leading signals here are {', '.join(top_features)}. The highest-ranked shift/rest proxy in the exported slice is {shift_features[0]}, which is one of the clearest practical differences from the archived model article."
            )
        else:
            st.markdown(
                f"The leading signals here are {', '.join(top_features)}. This is the partition where the new deployment still leans most heavily on spatial and scoreboard structure rather than on the newer shift/rest proxies."
            )

st.markdown("### B. Top Predictor Families")
st.markdown(
    "The family view below normalizes within each model's exported top-20 gain slice rather than across the full tree ensemble, so it should be read as a composition summary of the saved importance tables. Even with that limitation, it captures the main structural story: geometry remains the backbone, pre-shot movement is substantial in the in-flow partitions, and shift/rest timing now owns a meaningful share of the modern model's explanatory surface."
)

family_df = importance_df.groupby(["datasetLabel", "family"], as_index=False, observed=False)["gain"].sum()
family_df["datasetLabel"] = family_df["datasetLabel"].astype(str)
family_totals = family_df.groupby("datasetLabel", as_index=False)["gain"].sum().rename(columns={"gain": "datasetGain"})
family_df = family_df.merge(family_totals, on="datasetLabel", how="left")
family_df["gainPctWithinSlice"] = family_df["gain"] / family_df["datasetGain"] * 100

family_order = ["Shot Geometry", "Pre-Shot Movement", "Shift and Rest", "Game State", "Player Profile", "Other"]
heat = (
    family_df.pivot(index="datasetLabel", columns="family", values="gainPctWithinSlice")
    .reindex(index=DATASET_ORDER, columns=family_order)
    .fillna(0.0)
)

fig_family = go.Figure(
    go.Heatmap(
        z=heat.values,
        x=heat.columns.tolist(),
        y=heat.index.tolist(),
        colorscale="Blues",
        zmin=0,
        zmax=max(25.0, float(heat.values.max())),
        hovertemplate="Partition: %{y}<br>Family: %{x}<br>Share Within Exported Slice: %{z:.1f}%<extra></extra>",
        colorbar=dict(title="Slice Share (%)"),
    )
)
fig_family.update_layout(
    title=dict(text="Feature-Family Share Within Exported Top-20 Gain Slices", x=0.5, xanchor="center"),
    margin=dict(l=10, r=10, t=45, b=80),
    xaxis=dict(title="Feature Family"),
    yaxis=dict(title="Partition"),
    height=420,
)
fig_family.update_xaxes(fixedrange=True)
fig_family.update_yaxes(fixedrange=True)
st.plotly_chart(fig_family, width="stretch", config={"displayModeBar": True})

dominant_family = (
    family_df.sort_values(["datasetLabel", "gainPctWithinSlice"], ascending=[True, False])
    .groupby("datasetLabel", as_index=False)
    .first()
)
family_sentence = ", ".join(
    dominant_family["datasetLabel"] + " -> " + dominant_family["family"] + " (" + dominant_family["gainPctWithinSlice"].map(lambda x: f"{x:.1f}%") + ")"
)
st.markdown(
    f"The dominant family within the exported top-20 slice is {family_sentence}. The more revealing point is not that geometry still leads, but that shift and rest timing is now an actual contributor in the modern deployment rather than a placeholder for future work."
)

st.markdown("## VI. Ethical Backfilling")

st.markdown("### A. How the Models Are Used in Shot-by-Shot Data (SBSS)")
st.markdown(
    "The shot-by-shot scoring pipeline rebuilds the same season-level attempt table, carries the same partition logic, and uses the season-appropriate xG family to score the Fenwick rows before the skater and goalie outputs are written. Blocked shots remain in shot-by-shot scoring because the downstream shot-attempt views care about full Corsi accounting, but those rows are assigned `xG = 0` because the xG models themselves are trained and scored only on Fenwick events."
)
st.markdown(
    "The SBSS output rules mirror the model prep carefully. Regular-season shootout attempts are removed before final output even though the `ps` partition is still how penalty-shot and shootout-style events are modeled. Penalty shots remain and their written `strengthState` is normalized to `even-strength`. Empty-net attempts stay in skater SBSS, but only reach goalie SBSS when a defending goalie identifier is actually present. Those details matter because they prevent the published files from drifting away from the model assumptions that produced the xG values in the first place."
)

st.markdown("### B. Season Coverage and Leakage Policy")
backfill_table = pd.DataFrame(
    [
        {
            "Season(s)": "2010-11 and 2011-12",
            "Scoring Rule": "Legacy v1 ridge crossfit only",
            "Leakage Safeguard": "Five game-based holdout parts; temporary models refit on the other season plus the other four parts.",
        },
        {
            "Season(s)": "2012-13 through 2017-18",
            "Scoring Rule": "Stored legacy v1 ridge workflows",
            "Leakage Safeguard": "Direct scoring is safe because those seasons are outside the v1 training pair.",
        },
        {
            "Season(s)": "2018-19 through 2022-23",
            "Scoring Rule": "Stored legacy v2 ridge workflows",
            "Leakage Safeguard": "Direct scoring is safe because those seasons are outside the v2 training pair.",
        },
        {
            "Season(s)": "2023-24 and 2024-25",
            "Scoring Rule": "New v3 crossfit refits",
            "Leakage Safeguard": "Five game-based holdout parts with fresh refits on the paired season plus the other four parts of the target season.",
        },
        {
            "Season(s)": "2025-26",
            "Scoring Rule": "Stored new v3 deployment bundle",
            "Leakage Safeguard": "Direct forward scoring with the preferred saved models.",
        },
    ]
)
st.dataframe(backfill_table, hide_index=True, width="stretch")

st.markdown(
    "The key ethical rule is simple: a saved model is not used to score a season if that season's own target rows were part of the model fit. For the crossfit eras, the holdouts are built at the game level with `ntile(..., 5)` over sorted distinct `gameId` values, so entire games stay together rather than leaking related shots across folds. SBSS also refits only the partitions that are actually needed in the held-out part, which keeps the backfill practical without weakening the leakage guardrail."
)
st.markdown(
    "There is one explicit limitation in the new 2023-24 and 2024-25 crossfit path, and it should be stated plainly. The refits reuse the previously selected best hyperparameters from the new training artifacts, and those hyperparameters were originally chosen on the full pooled 2023-24 / 2024-25 training window. The held-out shots are therefore excluded from the final fit for their fold, but they did indirectly influence the chosen tuning values. That is weaker than fully nested cross-validation, but stronger than simply scoring those seasons with the fully trained saved models. The code treats this as a known compromise, not as something to hide."
)
st.markdown(
    "Older seasons add one more historical safeguard. If the legacy skater-count inputs needed for clean classification are missing, the pipeline first rules out penalty-shot and empty-net cases and then forces the remaining row into `sd`. That keeps the partitioning exhaustive in sparse historical data instead of discarding rows or pretending that missing manpower context is harmless."
)

st.markdown("## VII. Conclusion")
st.markdown(
    "The new xG system is best understood as a season-aware deployment family rather than as a single static model. It uses a modern shared prep path, six game-state partitions, new tree models trained on 2023-24 and 2024-25, a hybrid per-partition deployment bundle for 2025-26, and explicit leakage safeguards when historical or in-window backfilling would otherwise be misleading. That framing is more complicated than the archived V1-V4 article, but it is also more truthful about how the project actually uses xG. There are still meaningful limitations. The model still works from event data rather than full tracking, so passing structure, traffic, release deception, and puck height remain only partially observed. The crossfit path for 2023-24 and 2024-25 still inherits some hyperparameter-selection leakage because it reuses the previously selected tuning values. And the smallest partitions, especially empty-net and penalty-shot / shootout-style events, will always be more sample-constrained than 5v5. Even so, this version is materially stronger than the archived article suggested: the shift-time predictors are real, the short-miss cleanup is now reversed consistently, the versioning now reflects deployment reality, and the unseen-future comparison shows why the new system is a hybrid instead of a single-engine monolith."
)

st.markdown("The downstream summaries built from these model outputs remain available here:")
st.page_link("views/skater_shot_analysis.py", label="Skater Shot Analysis", icon=":material/readiness_score:")
st.page_link("views/goalie_shot_analysis.py", label="Goalie Shot Analysis", icon=":material/readiness_score:")
