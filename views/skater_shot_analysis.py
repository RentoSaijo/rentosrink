# Import libraries.
import os
import pandas as pd
import streamlit as st
import plotly.graph_objects as go
from utils import load_biographies, load_skater_shot_analysis

# ------------------------ CONFIG ------------------------ #
# Make this easy to change over time.
SEASON_START = 20112012
SEASON_END   = 20252026   # default season should be 20252026

GAME_TYPES = {"Regular Season": 2, "Playoffs": 3}  # renamed label
CATEGORIES = {"Actual": "act", "Per 82": "p82", "Per 60": "p60"}
SITUATIONS = {"5 on 5": "std", "All": "all"}

def _season_ids(start: int, end: int) -> list[str]:
    ids = []
    y0 = int(str(start)[:4])
    y1 = int(str(end)[:4])
    for y in range(y0, y1 + 1):
        ids.append(f"{y}{y+1}")
    return ids

def _season_label(season_id: str) -> str:
    season_id = str(season_id)
    return f"{season_id[:4]}-{season_id[4:]}"

SEASON_IDS = _season_ids(SEASON_START, SEASON_END)

# Most recent -> oldest in dropdown
SEASON_IDS = sorted(SEASON_IDS, key=lambda s: int(s), reverse=True)

SEASON_LABELS  = {_season_label(s): s for s in SEASON_IDS}
SEASON_OPTIONS = list(SEASON_LABELS.keys())

DEFAULT_SEASON_ID = "20252026"
default_season_label = _season_label(DEFAULT_SEASON_ID)
default_season_index = SEASON_OPTIONS.index(default_season_label) if default_season_label in SEASON_OPTIONS else 0

# ------------------------ DATA LOAD ------------------------ #
bio = load_biographies()

# ------------------------ FILTER ROW (NO BORDERS) ------------------------ #
# Order (left -> right): Season, Game Type, Player, Situation, Category
c_season, c_game, c_player, c_sit, c_cat = st.columns(
    [1, 1, 1, 1, 1.1],
    gap="small",
    vertical_alignment="top",
)

# 1) Season
with c_season:
    season_label = st.selectbox(
        "Season",
        SEASON_OPTIONS,
        index=default_season_index,
        key="ssa_season_label",
    )
season_id = SEASON_LABELS[season_label]  # YYYYYYYY

# Load skater shot analysis for selected season
ssa = load_skater_shot_analysis(season_id)

# ------------------------ GAME TYPE OPTIONS (FILTERED BY DATA) ------------------------ #
ssa_gt = ssa.copy()
ssa_gt["playerId"] = pd.to_numeric(ssa_gt.get("playerId"), errors="coerce")

available_game_types = []
for label, gt_id in GAME_TYPES.items():
    gp_col = f"gP_{gt_id}"
    if gp_col not in ssa_gt.columns:
        continue
    ssa_gt[gp_col] = pd.to_numeric(ssa_gt.get(gp_col), errors="coerce")
    # include only if at least one player has games played > 0
    ok = ssa_gt.dropna(subset=["playerId", gp_col])
    if not ok.empty and (ok[gp_col] > 0).any():
        available_game_types.append(label)

# Fallback (shouldn't happen, but keeps UI safe)
if not available_game_types:
    available_game_types = ["Regular Season"]

# Keep current selection if still valid; otherwise default to Regular Season if available
prev_gt_label = st.session_state.get("ssa_game_type_label", None)

if prev_gt_label in available_game_types:
    gt_index = available_game_types.index(prev_gt_label)
elif "Regular Season" in available_game_types:
    gt_index = available_game_types.index("Regular Season")
else:
    gt_index = 0

with c_game:
    game_type_label = st.selectbox(
        "Game Type",
        available_game_types,
        index=gt_index,
        key="ssa_game_type_label",
    )
game_type_id = GAME_TYPES[game_type_label]  # 2 or 3

# 4) Situation (regular selectbox)
with c_sit:
    situation_label = st.selectbox(
        "Situation",
        list(SITUATIONS.keys()),
        index=0,  # default 5 on 5
        key="ssa_situation_label",
    )
situation_code = SITUATIONS[situation_label]  # "std" / "all"

# 5) Category (dropdown)
with c_cat:
    category_label = st.selectbox(
        "Category",
        list(CATEGORIES.keys()),
        index=0,  # default Actual
        key="ssa_category_label",
    )
category_code = CATEGORIES[category_label]  # "act" / "p82" / "p60"

# 3) Player (depends on season + game_type; persist by playerId, not name)
gp_col = f"gP_{game_type_id}"

ssa_ok = ssa.copy()
ssa_ok["playerId"] = pd.to_numeric(ssa_ok.get("playerId"), errors="coerce")
ssa_ok[gp_col] = pd.to_numeric(ssa_ok.get(gp_col), errors="coerce")

ssa_ok = ssa_ok.dropna(subset=["playerId", gp_col])
ssa_ok = ssa_ok.loc[ssa_ok[gp_col] > 0].copy()
ssa_ok["playerId"] = ssa_ok["playerId"].astype(int)

eligible_ids = sorted(ssa_ok["playerId"].unique().tolist())

# Join bios for display names (only eligible)
bio_ok = bio.copy()
bio_ok["playerId"] = pd.to_numeric(bio_ok.get("playerId"), errors="coerce")
bio_ok = bio_ok.dropna(subset=["playerId"]).copy()
bio_ok["playerId"] = bio_ok["playerId"].astype(int)

bio_ok = bio_ok.loc[bio_ok["playerId"].isin(set(eligible_ids))].copy()

# Map id -> name (strip just in case)
bio_ok["menuName"] = bio_ok["menuName"].astype(str).str.strip()
id_to_name = dict(zip(bio_ok["playerId"], bio_ok["menuName"]))

def _name_for(pid: int) -> str:
    return id_to_name.get(int(pid), f"Player {pid}")

# ---------------- PLAYER (persist cleanly; no widget-key mutation) ----------------

# Default player for this season/game_type/situation = max iGF
igf_col = f"iGF_{game_type_id}_{situation_code}"
default_player_id = None
if igf_col in ssa_ok.columns:
    ssa_ok[igf_col] = pd.to_numeric(ssa_ok.get(igf_col), errors="coerce")
    ssa_rank = ssa_ok.dropna(subset=[igf_col]).copy()
    if not ssa_rank.empty:
        default_player_id = int(ssa_rank.sort_values(igf_col, ascending=False).iloc[0]["playerId"])

fallback_id = (
    default_player_id
    if (default_player_id is not None and default_player_id in eligible_ids)
    else (eligible_ids[0] if eligible_ids else None)
)

# Persisted selection stored separately (NOT the widget key)
if "ssa_player_id_saved" not in st.session_state:
    st.session_state["ssa_player_id_saved"] = None

saved_id = st.session_state["ssa_player_id_saved"]

# Decide what should be selected this run:
# - keep saved if still eligible
# - else use fallback
selected_id = saved_id if (saved_id in eligible_ids) else fallback_id

player_index = (
    eligible_ids.index(selected_id)
    if (eligible_ids and selected_id in eligible_ids)
    else None
)

with c_player:
    player_id = st.selectbox(
        "Player",
        options=eligible_ids,
        format_func=_name_for,
        index=player_index,
        key="ssa_player_id",  # widget owns this; we will NOT write to it elsewhere
        placeholder=("N/A" if not eligible_ids else None),
    )

# Update saved selection AFTER widget value is known (safe, different key)
st.session_state["ssa_player_id_saved"] = player_id

player_name = _name_for(player_id) if player_id is not None else None

# ------------------------ 2ND ROW: HEADSHOT + METRICS ------------------------ #
def _img(path, fallback=None):
    if path and os.path.exists(path):
        st.image(path, width="stretch")
    elif fallback and os.path.exists(fallback):
        st.image(fallback, width="stretch")

def _fmt_num(x, decimals=1):
    if x is None or pd.isna(x):
        return "N/A"
    return f"{float(x):.{decimals}f}"

def _fmt_int_or_1dp(x):
    if x is None or pd.isna(x):
        return "N/A"
    x = float(x)
    # If it's basically an integer, show as int; else 1dp
    return f"{int(round(x))}" if abs(x - round(x)) < 1e-9 else f"{x:.1f}"

def _fmt_sigma(z):
    if z is None or pd.isna(z):
        return "N/A"
    return f"{float(z):+.1f}σ"

def _transform_metric(df_in: pd.DataFrame, metric_col: str, category_code: str, game_type_id: int) -> pd.Series:
    """
    category_code:
      - "act": raw
      - "p82": metric / gP_{game_type_id} * 82
      - "p60": metric / mP_{game_type_id} * 60
    """
    s = pd.to_numeric(df_in.get(metric_col), errors="coerce")

    if category_code == "act":
        return s

    if category_code == "p82":
        gp_col = f"gP_{game_type_id}"
        gp = pd.to_numeric(df_in.get(gp_col), errors="coerce")
        return s / gp * 82.0

    if category_code == "p60":
        mp_col = f"mP_{game_type_id}"
        mp = pd.to_numeric(df_in.get(mp_col), errors="coerce")
        return s / mp * 60.0

    return s

# Pull selected row
r = None
if player_id is not None:
    ssa_tmp = ssa.copy()
    ssa_tmp["playerId"] = pd.to_numeric(ssa_tmp.get("playerId"), errors="coerce")
    m = (ssa_tmp["playerId"].astype("Int64") == int(player_id))
    if m.any():
        r = ssa_tmp.loc[m].iloc[0]

# Eligible population for z-scores = players with games played > 0 for this game type
ssa_pop = ssa.copy()
ssa_pop["playerId"] = pd.to_numeric(ssa_pop.get("playerId"), errors="coerce")
gp_col = f"gP_{game_type_id}"
ssa_pop[gp_col] = pd.to_numeric(ssa_pop.get(gp_col), errors="coerce")
ssa_pop = ssa_pop.dropna(subset=["playerId", gp_col]).copy()
ssa_pop = ssa_pop.loc[ssa_pop[gp_col] > 0].copy()

# Metric columns depend on situation
col_iGF  = f"iGF_{game_type_id}_{situation_code}"
col_ixGF = f"ixGF_{game_type_id}_{situation_code}"
col_oGF  = f"oGF_{game_type_id}_{situation_code}"
col_oxGF = f"oxGF_{game_type_id}_{situation_code}"

# --- population for z-scores (players with games played > 0 for this game type) ---
ssa_pop = ssa.copy()
ssa_pop["playerId"] = pd.to_numeric(ssa_pop.get("playerId"), errors="coerce")
gp_col = f"gP_{game_type_id}"
ssa_pop[gp_col] = pd.to_numeric(ssa_pop.get(gp_col), errors="coerce")
ssa_pop = ssa_pop.dropna(subset=["playerId", gp_col]).copy()
ssa_pop = ssa_pop.loc[ssa_pop[gp_col] > 0].copy()

# Compute transformed metric series for population
s_iGF  = _transform_metric(ssa_pop, col_iGF,  category_code, game_type_id) if col_iGF  in ssa_pop.columns else pd.Series(dtype=float)
s_ixGF = _transform_metric(ssa_pop, col_ixGF, category_code, game_type_id) if col_ixGF in ssa_pop.columns else pd.Series(dtype=float)
s_oGF  = _transform_metric(ssa_pop, col_oGF,  category_code, game_type_id) if col_oGF  in ssa_pop.columns else pd.Series(dtype=float)
s_oxGF = _transform_metric(ssa_pop, col_oxGF, category_code, game_type_id) if col_oxGF in ssa_pop.columns else pd.Series(dtype=float)

# Derived Ax series (aligned by index)
s_iGFAx = s_iGF - s_ixGF
s_oGFAx = s_oGF - s_oxGF

def _z_of_player(metric_series: pd.Series, player_val):
    """z = (x - mean) / std over non-null series. std uses ddof=0; if std==0 -> NaN."""
    s = pd.to_numeric(metric_series, errors="coerce").dropna()
    x = pd.to_numeric(pd.Series([player_val]), errors="coerce").iloc[0]
    if pd.isna(x) or s.empty:
        return float("nan")
    mu = float(s.mean())
    sd = float(s.std(ddof=0))
    if sd <= 0 or pd.isna(sd):
        return float("nan")
    return (float(x) - mu) / sd

# Pull selected row
r = None
if player_id is not None:
    ssa_tmp = ssa.copy()
    ssa_tmp["playerId"] = pd.to_numeric(ssa_tmp.get("playerId"), errors="coerce")
    m = (ssa_tmp["playerId"].astype("Int64") == int(player_id))
    if m.any():
        r = ssa_tmp.loc[m].iloc[0]

def _player_metric_value(metric_col: str):
    if r is None or metric_col not in ssa.columns:
        return float("nan")
    one = pd.DataFrame([r])
    return _transform_metric(one, metric_col, category_code, game_type_id).iloc[0]

# Player values (transformed)
p_iGF  = _player_metric_value(col_iGF)
p_ixGF = _player_metric_value(col_ixGF)
p_oGF  = _player_metric_value(col_oGF)
p_oxGF = _player_metric_value(col_oxGF)

# Derived Ax values (AFTER transformation)
p_iGFAx = p_iGF - p_ixGF if (pd.notna(p_iGF) and pd.notna(p_ixGF)) else float("nan")
p_oGFAx = p_oGF - p_oxGF if (pd.notna(p_oGF) and pd.notna(p_oxGF)) else float("nan")

# Z deltas
z_iGF    = _z_of_player(s_iGF,    p_iGF)
z_ixGF   = _z_of_player(s_ixGF,   p_ixGF)
z_iGFAx  = _z_of_player(s_iGFAx,  p_iGFAx)
z_oGF    = _z_of_player(s_oGF,    p_oGF)
z_oxGF   = _z_of_player(s_oxGF,   p_oxGF)
z_oGFAx  = _z_of_player(s_oGFAx,  p_oGFAx)

# Layout: headshot + 6 metrics (iGF, ixGF, iGFAx, oGF, oxGF, oGFAx)
c_hs, c_m1, c_m2, c_m3, c_m4, c_m5, c_m6 = st.columns(
    [1.125, 1, 1, 1, 1, 1, 1],
    gap="small",
    vertical_alignment="center",
)

with c_hs:
    with st.container(border=True):
        if player_id is None:
            st.empty()
        else:
            headshot_path = f"assets/headshots/{int(player_id)}.png"
            _img(headshot_path, fallback="assets/headshots/default.png")

with c_m1:
    with st.container(border=True):
        st.metric("iGF", value=_fmt_int_or_1dp(p_iGF), delta=_fmt_sigma(z_iGF), delta_color="normal")

with c_m2:
    with st.container(border=True):
        st.metric("ixGF", value=_fmt_int_or_1dp(p_ixGF), delta=_fmt_sigma(z_ixGF), delta_color="normal")

with c_m3:
    with st.container(border=True):
        st.metric("iGFAx", value=_fmt_int_or_1dp(p_iGFAx), delta=_fmt_sigma(z_iGFAx), delta_color="normal")

with c_m4:
    with st.container(border=True):
        st.metric("oGF", value=_fmt_int_or_1dp(p_oGF), delta=_fmt_sigma(z_oGF), delta_color="normal")

with c_m5:
    with st.container(border=True):
        st.metric("oxGF", value=_fmt_int_or_1dp(p_oxGF), delta=_fmt_sigma(z_oxGF), delta_color="normal")

with c_m6:
    with st.container(border=True):
        st.metric("oGFAx", value=_fmt_int_or_1dp(p_oGFAx), delta=_fmt_sigma(z_oGFAx), delta_color="normal")

# ------------------------ 1 ROW OF 3 PLOTS ------------------------ #
# One shared plot height so all 3 columns align.
PLOT_H = 400  # tune if you want (e.g., 380/420). All three plots will match.

c1, c2, c3 = st.columns(3, gap="small", vertical_alignment="top")

# ----- (1) SHOT VOLUME + OUTCOME FLOW (SANKEY) ----- #
with c1:
    if player_id is None or r is None:
        st.info("Select a player.")
    else:
        # ---- Helpers for on-the-fly category scaling ----
        def _scaled_value_from_row(row: pd.Series, raw_col: str, category_code: str, game_type_id: int) -> float:
            """Return metric scaled to Actual / Per82 / Per60 based on row totals."""
            x = pd.to_numeric(row.get(raw_col, float("nan")), errors="coerce")
            if pd.isna(x):
                return float("nan")

            if category_code == "act":
                return float(x)

            if category_code == "p82":
                gp = pd.to_numeric(row.get(f"gP_{game_type_id}", float("nan")), errors="coerce")
                if pd.isna(gp) or gp <= 0:
                    return float("nan")
                return float(x) / float(gp) * 82.0

            if category_code == "p60":
                mp = pd.to_numeric(row.get(f"mP_{game_type_id}", float("nan")), errors="coerce")
                if pd.isna(mp) or mp <= 0:
                    return float("nan")
                return float(x) / float(mp) * 60.0

            return float(x)

        # ---- Column names (situation-specific) ----
        corsi_col   = f"iCorsiF_{game_type_id}_{situation_code}"
        fenwick_col = f"iFenwickF_{game_type_id}_{situation_code}"
        sog_col     = f"iSOGF_{game_type_id}_{situation_code}"
        goal_col    = f"iGF_{game_type_id}_{situation_code}"

        needed = [corsi_col, fenwick_col, sog_col, goal_col]
        missing = [c for c in needed if c not in ssa.columns]
        if missing:
            st.info("No Sankey data available for this selection.")
        else:
            # ---- Pull + scale values ----
            corsi   = _scaled_value_from_row(r, corsi_col,   category_code, game_type_id)
            fenwick = _scaled_value_from_row(r, fenwick_col, category_code, game_type_id)
            sog     = _scaled_value_from_row(r, sog_col,     category_code, game_type_id)
            goals   = _scaled_value_from_row(r, goal_col,    category_code, game_type_id)

            # Guard
            if any(pd.isna(v) for v in [corsi, fenwick, sog, goals]):
                st.info("No Sankey data available for this selection.")
            else:
                # ---- Compute drop-offs (clamped) ----
                blocked = max(corsi - fenwick, 0.0)
                missed  = max(fenwick - sog, 0.0)
                saved   = max(sog - goals, 0.0)

                def _fmt_node_val(x: float) -> str:
                    if pd.isna(x):
                        return "N/A"
                    if category_code == "act":
                        return f"{int(round(x))}"
                    return f"{x:.1f}"

                node_names  = ["Corsi", "Blocked", "Fenwick", "Missed", "SOG", "Saved", "Goals"]
                node_vals   = [corsi, blocked, fenwick, missed, sog, saved, goals]
                node_labels = [f"{n} ({_fmt_node_val(v)})" for n, v in zip(node_names, node_vals)]

                sources = [0, 0, 2, 2, 4, 4]
                targets = [2, 1, 4, 3, 6, 5]
                values  = [fenwick, blocked, sog, missed, goals, saved]
                values  = [max(float(v), 0.0) for v in values]

                # Brighter nodes; very transparent links
                NODE_COLORS = {
                    "Corsi":   "rgba(255,  80,  72, 0.9)",
                    "Blocked": "rgba(255,  80,  72, 0.9)",
                    "Fenwick": "rgba(255, 170,  40, 0.9)",
                    "Missed":  "rgba(255, 170,  40, 0.9)",
                    "SOG":     "rgba(255, 225,  70, 0.9)",
                    "Saved":   "rgba(255, 225,  70, 0.9)",
                    "Goals":   "rgba( 90, 220, 120, 0.9)",
                }

                LINK_COLORS = [
                    "rgba(255,  80,  72, 0.1)",  # Corsi -> Fenwick
                    "rgba(255,  80,  72, 0.1)",  # Corsi -> Blocked
                    "rgba(255, 170,  40, 0.1)",  # Fenwick -> SOG
                    "rgba(255, 170,  40, 0.1)",  # Fenwick -> Missed
                    "rgba(255, 225,  70, 0.1)",  # SOG -> Goals
                    "rgba(255, 225,  70, 0.1)",  # SOG -> Saved
                ]

                # Lock node positions to preserve order
                eps = 1e-3
                X = [0.01, 0.99, 0.33, 0.99, 0.66, 0.99, 0.99]
                Y = [eps, 0.70, eps, 0.45, eps, 0.20, eps]

                cat_suffix = {"act": "", "p82": " (Per 82)", "p60": " (Per 60)"}.get(category_code, "")
                title = "Shot Volume & Outcome"

                fig = go.Figure(
                    go.Sankey(
                        arrangement="snap",
                        valueformat=".1f" if category_code != "act" else ".0f",
                        hoverinfo="skip",
                        node=dict(
                            label=node_labels,
                            x=X,
                            y=Y,
                            pad=18,
                            thickness=18,
                            color=[NODE_COLORS[n] for n in node_names],
                            line=dict(width=0.5, color="rgba(255,255,255,0.22)"),
                        ),
                        link=dict(
                            source=sources,
                            target=targets,
                            value=values,
                            color=LINK_COLORS,
                        ),
                    )
                )

                fig.update_layout(
                    title=title,
                    margin=dict(l=10, r=10, t=50, b=10),
                    height=PLOT_H,
                )

                st.plotly_chart(fig, width="stretch", config={"displayModeBar": True})


# ----- (2) HALF RINK + AVG SHOT LOCATION (x/y from CSV) ----- #
with c2:
    def _add_line(fig, x0, y0, x1, y1, color, width=3, dash=None):
        fig.add_shape(
            type="line",
            xref="x", yref="y",
            x0=x0, y0=y0, x1=x1, y1=y1,
            line=dict(color=color, width=width, dash=dash),
            layer="below",
        )

    def _add_circle(fig, cx, cy, r, line_color="rgba(255,255,255,0.40)", width=2):
        fig.add_shape(
            type="circle",
            xref="x", yref="y",
            x0=cx - r, x1=cx + r,
            y0=cy - r, y1=cy + r,
            line=dict(color=line_color, width=width),
            fillcolor="rgba(0,0,0,0)",
            layer="below",
        )

    def _add_rect(fig, x0, y0, x1, y1, line_color="rgba(255,255,255,0.55)", width=2, fill="rgba(0,0,0,0)"):
        fig.add_shape(
            type="rect",
            xref="x", yref="y",
            x0=x0, y0=y0, x1=x1, y1=y1,
            line=dict(color=line_color, width=width),
            fillcolor=fill,
            layer="below",
        )

    def make_half_rink_with_avg_locations(ssa: pd.DataFrame, game_type_id: int, situation_code: str, player_id: int | None):
        # Half rink dimensions (feet): 100 (length) x 85 (width)
        W = 85.0
        HALF_W = W / 2.0          # 42.5
        L = 100.0

        X_MIN, X_MAX = -HALF_W, HALF_W   # width
        Y_MIN, Y_MAX = 0.0, L            # length (0=center line -> 100=end boards)

        # Markings in this view: y=0 is center line; y=100 is end boards
        GOAL_LINE_Y = L - 11.0    # 89
        BLUE_LINE_Y = L - 75.0    # 25
        CENTER_LINE_Y = 0.0

        # End-zone faceoff dots: 20' from goal line; 20' from side boards
        DOT_Y = GOAL_LINE_Y - 20.0     # 69
        DOT_X = HALF_W - 20.0          # 22.5
        FACE_CIRCLE_R = 15.0

        fig = go.Figure()

        # Boards
        _add_rect(fig, X_MIN, Y_MIN, X_MAX, Y_MAX, line_color="rgba(255,255,255,0.55)", width=2)

        # Lines
        _add_line(fig, X_MIN, CENTER_LINE_Y, X_MAX, CENTER_LINE_Y, color="rgba(255,0,0,0.55)", width=4)
        _add_line(fig, X_MIN, BLUE_LINE_Y,   X_MAX, BLUE_LINE_Y,   color="rgba(0,140,255,0.55)", width=4)
        _add_line(fig, X_MIN, GOAL_LINE_Y,   X_MAX, GOAL_LINE_Y,   color="rgba(255,0,0,0.35)", width=3)

        # End-zone circles + dots
        _add_circle(fig, -DOT_X, DOT_Y, FACE_CIRCLE_R)
        _add_circle(fig,  DOT_X, DOT_Y, FACE_CIRCLE_R)
        fig.add_trace(
            go.Scatter(
                x=[-DOT_X, DOT_X],
                y=[DOT_Y,  DOT_Y],
                mode="markers",
                marker=dict(size=7, color="rgba(255,0,0,0.65)"),
                hoverinfo="skip",
                showlegend=False,
            )
        )

        # Neutral-zone faceoff dots (by the blue line): 5' inside the blue line toward center ice
        NZ_DOT_X = DOT_X
        NZ_DOT_Y = BLUE_LINE_Y - 5.0
        fig.add_trace(
            go.Scatter(
                x=[-NZ_DOT_X, NZ_DOT_X],
                y=[NZ_DOT_Y,  NZ_DOT_Y],
                mode="markers",
                marker=dict(size=7, color="rgba(255,0,0,0.65)"),
                hoverinfo="skip",
                showlegend=False,
            )
        )

        # Tiny net marker
        _add_line(fig, -3.0, GOAL_LINE_Y, 3.0, GOAL_LINE_Y, color="rgba(255,255,255,0.65)", width=4)

        # ---------- Avg shot location overlay ----------
        x_col = f"x_{game_type_id}_{situation_code}"  # expected: length in [0,100] (but we abs() to be safe)
        y_col = f"y_{game_type_id}_{situation_code}"  # expected: width  in [-42.5, 42.5]

        if (x_col in ssa.columns) and (y_col in ssa.columns):
            gp_col = f"gP_{game_type_id}"

            pop = ssa.copy()
            pop["playerId"] = pd.to_numeric(pop.get("playerId"), errors="coerce")
            pop[gp_col] = pd.to_numeric(pop.get(gp_col), errors="coerce")
            pop = pop.dropna(subset=["playerId", gp_col]).copy()
            pop = pop.loc[pop[gp_col] > 0].copy()
            pop["playerId"] = pop["playerId"].astype(int)

            # IMPORTANT mapping to rink axes:
            # - CSV x_* is LENGTH (0..100) -> plotly y-axis
            # - CSV y_* is WIDTH  (-42.5..42.5) -> plotly x-axis
            pop["len_y"] = pd.to_numeric(pop.get(x_col), errors="coerce").abs()
            pop["wid_x"] = pd.to_numeric(pop.get(y_col), errors="coerce")

            pop = pop.dropna(subset=["len_y", "wid_x", "playerId"]).copy()

            # Keep only points that fit on our plotted half-rink
            pop = pop.loc[
                (pop["len_y"] >= Y_MIN) & (pop["len_y"] <= Y_MAX) &
                (pop["wid_x"] >= X_MIN) & (pop["wid_x"] <= X_MAX)
            ].copy()

            if not pop.empty:
                pop["is_player"] = (pop["playerId"] == int(player_id)) if player_id is not None else False
                others = pop.loc[~pop["is_player"]].copy()
                mine   = pop.loc[ pop["is_player"]].copy()

                others["name"] = others["playerId"].apply(_name_for)
                mine["name"]   = mine["playerId"].apply(_name_for)

                hover_tmpl = (
                    "Player: %{customdata[0]}<br>"
                    "Avg X (length): %{customdata[1]:.1f}<br>"
                    "Avg Y (width): %{customdata[2]:.1f}"
                    "<extra></extra>"
                )

                # Others (gray circles)
                fig.add_trace(
                    go.Scatter(
                        x=others["wid_x"],
                        y=others["len_y"],
                        mode="markers",
                        marker=dict(size=8, opacity=0.55, color="rgba(160,160,160,0.65)", symbol="circle"),
                        customdata=others[["name", "len_y", "wid_x"]].to_numpy(),
                        hovertemplate=hover_tmpl,
                        showlegend=False,
                    )
                )

                # Selected player (star)
                if not mine.empty:
                    fig.add_trace(
                        go.Scatter(
                            x=mine["wid_x"],
                            y=mine["len_y"],
                            mode="markers",
                            marker=dict(size=16, opacity=1.0, symbol="star", color="yellow"),
                            customdata=mine[["name", "len_y", "wid_x"]].to_numpy(),
                            hovertemplate=hover_tmpl,
                            showlegend=False,
                        )
                    )

        fig.update_layout(
            title="Average Shot Location vs. League",
            margin=dict(l=5, r=5, t=45, b=5),
            height=PLOT_H,
            paper_bgcolor="rgba(0,0,0,0)",
            plot_bgcolor="rgba(0,0,0,0)",
        )

        fig.update_xaxes(
            range=[X_MIN, X_MAX],
            showgrid=False, zeroline=False,
            visible=False,
            fixedrange=True,
        )
        fig.update_yaxes(
            range=[Y_MIN, Y_MAX],
            showgrid=False, zeroline=False,
            visible=False,
            scaleanchor="x",
            scaleratio=1,
            fixedrange=True,
        )

        return fig

    st.plotly_chart(
        make_half_rink_with_avg_locations(ssa, game_type_id, situation_code, player_id),
        width="stretch",
        config={"displayModeBar": False},
    )


# ----- (3) Scatter (fixed: iGF vs ixGF) ----- #
with c3:
    # Fixed metrics for now
    x_metric = "iGF"
    y_metric = "ixGF"

    # Situation-specific columns
    x_col = f"{x_metric}_{game_type_id}_{situation_code}"
    y_col = f"{y_metric}_{game_type_id}_{situation_code}"

    if (x_col not in ssa.columns) or (y_col not in ssa.columns):
        st.info("No data available for iGF vs ixGF.")
    else:
        # Eligible population = players with games played > 0 for this game type
        gp_col = f"gP_{game_type_id}"
        pop = ssa.copy()
        pop["playerId"] = pd.to_numeric(pop.get("playerId"), errors="coerce")
        pop[gp_col] = pd.to_numeric(pop.get(gp_col), errors="coerce")
        pop = pop.dropna(subset=["playerId", gp_col]).copy()
        pop = pop.loc[pop[gp_col] > 0].copy()
        pop["playerId"] = pop["playerId"].astype(int)

        if pop.empty:
            st.info("No players found for this selection.")
        else:
            # Transform both axes according to category (act / p82 / p60)
            pop["x_val"] = _transform_metric(pop, x_col, category_code, game_type_id)
            pop["y_val"] = _transform_metric(pop, y_col, category_code, game_type_id)

            pop = pop.dropna(subset=["x_val", "y_val", "playerId"]).copy()
            if pop.empty:
                st.info("No valid values for this selection.")
            else:
                pop["is_player"] = (pop["playerId"] == int(player_id)) if player_id is not None else False
                others = pop.loc[~pop["is_player"]].copy()
                mine   = pop.loc[ pop["is_player"]].copy()

                others["name"] = others["playerId"].apply(_name_for)
                mine["name"]   = mine["playerId"].apply(_name_for)

                fmt = ".0f" if category_code == "act" else ".1f"
                hover_tmpl = (
                    "Player: %{customdata[0]}<br>"
                    f"{x_metric}: %{{x:{fmt}}}<br>"
                    f"{y_metric}: %{{y:{fmt}}}"
                    "<extra></extra>"
                )

                fig_sc = go.Figure()

                # Others (gray circles)
                fig_sc.add_trace(
                    go.Scatter(
                        x=others["x_val"],
                        y=others["y_val"],
                        mode="markers",
                        marker=dict(
                            size=7,
                            opacity=0.55,
                            color="rgba(160,160,160,0.65)",
                            symbol="circle",
                        ),
                        customdata=others[["name"]].to_numpy(),
                        hovertemplate=hover_tmpl,
                        showlegend=False,
                    )
                )

                # Selected player (star)
                if not mine.empty:
                    fig_sc.add_trace(
                        go.Scatter(
                            x=mine["x_val"],
                            y=mine["y_val"],
                            mode="markers",
                            marker=dict(
                                size=14,
                                opacity=1.0,
                                symbol="star",
                                color="yellow",
                            ),
                            customdata=mine[["name"]].to_numpy(),
                            hovertemplate=hover_tmpl,
                            showlegend=False,
                        )
                    )

                cat_suffix = {"act": "", "p82": " (Per 82)", "p60": " (Per 60)"}.get(category_code, "")
                fig_sc.update_layout(
                    title="Goal Production & Quality vs. League",
                    margin=dict(l=10, r=10, t=45, b=10),
                    xaxis=dict(title=x_metric),
                    yaxis=dict(title=y_metric),
                    height=PLOT_H,  # unchanged; now column height matches others because no filters
                )

                fig_sc.update_xaxes(fixedrange=True)
                fig_sc.update_yaxes(fixedrange=True)

                st.plotly_chart(fig_sc, width="stretch", config={"displayModeBar": True})
