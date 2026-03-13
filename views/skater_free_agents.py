# Import libraries.
import glob
import io
import os
import re
import pandas as pd
import plotly.graph_objects as go
import streamlit as st
from PIL import Image, ImageDraw, ImageFont, ImageOps
from utils import load_biographies, load_skater_contracts, load_skater_free_agents

# Set constants.
PLOT_H = 440
SIGN_WITH_OPTIONS = {'Same Team': 'resign', 'Different Team': 'nosign'}
CARD_W = 2048
CARD_H = 1180
CARD_MARGIN = 26
CARD_GAP = 18
CARD_BG = '#060B18'
CARD_BORDER = '#2A3447'
CARD_PANEL = '#080F1F'
CARD_CONTROL = '#242B3C'
CARD_TXT_MAIN = '#E8EEF8'
CARD_TXT_SUB = '#D3DAE6'
CARD_DELTA_BG = '#113B2E'
CARD_DELTA_TXT = '#53D18E'


def _get_font(size=20, bold=False):
    font_names = (
        ('DejaVuSans-Bold.ttf', 'Arial Bold.ttf', 'Arial.ttf')
        if bold else
        ('DejaVuSans.ttf', 'Arial.ttf')
    )
    for name in font_names:
        try:
            return ImageFont.truetype(name, size=size)
        except OSError:
            continue
    return ImageFont.load_default()


def _text_size(draw, text, font):
    bb = draw.textbbox((0, 0), str(text), font=font)
    return (bb[2] - bb[0], bb[3] - bb[1])


def _draw_metric_tile(draw, box, label, value, delta):
    x0, y0, x1, y1 = [int(v) for v in box]
    draw.rounded_rectangle([x0, y0, x1, y1], radius=16, fill=CARD_PANEL, outline=CARD_BORDER, width=2)

    label_font = _get_font(18, bold=True)
    value_font = _get_font(34, bold=True)
    delta_font = _get_font(17, bold=True)

    draw.text((x0 + 24, y0 + 26), str(label), font=label_font, fill=CARD_TXT_SUB)
    draw.text((x0 + 24, y0 + 88), str(value), font=value_font, fill=CARD_TXT_MAIN)

    delta_txt = str(delta)
    pill_x = x0 + 24
    pill_y = y0 + 188
    if delta_txt != 'N/A':
        tw, th = _text_size(draw, delta_txt, delta_font)
        draw.rounded_rectangle(
            [pill_x, pill_y, pill_x + tw + 30, pill_y + th + 14],
            radius=18,
            fill=CARD_DELTA_BG,
            outline=CARD_DELTA_BG,
            width=1,
        )
        draw.text((pill_x + 15, pill_y + 7), delta_txt, font=delta_font, fill=CARD_DELTA_TXT)
    else:
        draw.text((pill_x, pill_y + 6), delta_txt, font=delta_font, fill='#95A1B4')


def _draw_filter_control(draw, box, label, value, is_button=False):
    x0, y0, x1, y1 = [int(v) for v in box]
    label_font = _get_font(16, bold=True)
    value_font = _get_font(21, bold=True)
    draw.text((x0, y0), str(label), font=label_font, fill=CARD_TXT_SUB)

    fy0 = y0 + 34
    draw.rounded_rectangle([x0, fy0, x1, y1], radius=16, fill=CARD_CONTROL, outline=CARD_BORDER, width=2)
    tw, th = _text_size(draw, value, value_font)
    tx = x0 + 16 if not is_button else x0 + max(16, int((x1 - x0 - tw) / 2))
    ty = fy0 + int((y1 - fy0 - th) / 2) - 2
    draw.text((tx, ty), str(value), font=value_font, fill=CARD_TXT_MAIN)
    if not is_button:
        chevron = _get_font(20, bold=True)
        draw.text((x1 - 32, ty), 'v', font=chevron, fill=CARD_TXT_SUB)


def _style_export_figure(fig, width, height):
    out = go.Figure(fig)
    out.update_layout(
        template='plotly_dark',
        paper_bgcolor=CARD_BG,
        plot_bgcolor=CARD_BG,
        font=dict(color=CARD_TXT_MAIN, size=17),
        width=int(width),
        height=int(height),
        title=dict(font=dict(size=26)),
        legend=dict(font=dict(size=14)),
    )
    out.update_xaxes(
        gridcolor=CARD_BORDER,
        zerolinecolor=CARD_BORDER,
        linecolor=CARD_BORDER,
        tickfont=dict(color=CARD_TXT_SUB, size=14),
        title=dict(font=dict(color=CARD_TXT_SUB, size=17)),
    )
    out.update_yaxes(
        gridcolor=CARD_BORDER,
        zerolinecolor=CARD_BORDER,
        linecolor=CARD_BORDER,
        tickfont=dict(color=CARD_TXT_SUB, size=14),
        title=dict(font=dict(color=CARD_TXT_SUB, size=17)),
    )
    return out


def _to_image_bytes(fig, width, height):
    plain = go.Figure(fig)
    plain.update_layout(width=int(width), height=int(height))
    styled = _style_export_figure(fig, width, height)

    last_exc = None
    for candidate in (styled, plain):
        for kwargs in ({'engine': 'kaleido'}, {}):
            try:
                return candidate.to_image(
                    format='png',
                    width=int(width),
                    height=int(height),
                    scale=1,
                    **kwargs,
                )
            except Exception as exc:
                last_exc = exc
                continue

    raise last_exc if last_exc is not None else RuntimeError('Unknown chart export error.')


def _fig_to_card_tile(fig, width, height, fallback_text):
    tile = Image.new('RGB', (int(width), int(height)), CARD_BG)
    draw = ImageDraw.Draw(tile)
    draw.rounded_rectangle(
        [0, 0, int(width) - 1, int(height) - 1],
        radius=16,
        fill=CARD_BG,
        outline=CARD_BORDER,
        width=2,
    )

    chart_x0, chart_y0 = 10, 10
    chart_x1, chart_y1 = int(width) - 10, int(height) - 10
    chart_w = chart_x1 - chart_x0
    chart_h = chart_y1 - chart_y0

    if fig is None:
        draw.text((chart_x0 + 16, chart_y0 + 16), fallback_text, font=_get_font(24), fill='#95A1B4')
        return tile

    try:
        fig_png = _to_image_bytes(fig, width=chart_w, height=chart_h)
        chart = Image.open(io.BytesIO(fig_png)).convert('RGB')
        if chart.size != (chart_w, chart_h):
            resampling = getattr(getattr(Image, 'Resampling', Image), 'LANCZOS', Image.BICUBIC)
            chart = ImageOps.fit(chart, (chart_w, chart_h), method=resampling)
        tile.paste(chart, (chart_x0, chart_y0))
    except Exception as exc:
        msg = f'{fallback_text}\n{kaleido_hint(exc)}'
        draw.multiline_text((chart_x0 + 16, chart_y0 + 16), msg, spacing=6, font=_get_font(22), fill='#95A1B4')
    return tile


def kaleido_hint(exc):
    msg = str(exc)
    if 'kaleido' in msg.lower():
        return 'Install kaleido in your environment.'
    if 'invalid property' in msg.lower():
        return 'Plotly export layout error.'
    msg1 = re.sub(r'\s+', ' ', msg).strip()
    if msg1:
        return f'Chart export failed: {msg1[:120]}'
    return 'Chart export failed.'


def _build_contract_card_png(
    player_id,
    player_name,
    season_id,
    sign_with_label,
    metrics,
    fig_poss,
    fig_combo,
    fig_sc,
):
    canvas = Image.new('RGB', (CARD_W, CARD_H), CARD_BG)
    draw = ImageDraw.Draw(canvas)

    # Filter row (visual clone of the page controls)
    filter_label_y = CARD_MARGIN
    filter_control_y1 = CARD_MARGIN + 106
    fw = CARD_W - (2 * CARD_MARGIN)
    fw_gap = CARD_GAP
    f_count = 3
    f_cols = []
    f_x = CARD_MARGIN
    for i in range(f_count):
        col_w = int((fw - ((f_count - 1) * fw_gap)) / f_count)
        if i == (f_count - 1):
            col_w = CARD_W - CARD_MARGIN - f_x
        f_cols.append((f_x, col_w))
        f_x += col_w + fw_gap

    _draw_filter_control(
        draw,
        box=[f_cols[0][0], filter_label_y, f_cols[0][0] + f_cols[0][1], filter_control_y1],
        label='Season',
        value=_season_label(season_id),
    )
    _draw_filter_control(
        draw,
        box=[f_cols[1][0], filter_label_y, f_cols[1][0] + f_cols[1][1], filter_control_y1],
        label='Player',
        value=player_name,
    )
    _draw_filter_control(
        draw,
        box=[f_cols[2][0], filter_label_y, f_cols[2][0] + f_cols[2][1], filter_control_y1],
        label='Sign With',
        value=sign_with_label,
    )

    row1_y = filter_control_y1 + CARD_GAP + 10
    row1_h = 300
    headshot_w = 290

    hs_box = [CARD_MARGIN, row1_y, CARD_MARGIN + headshot_w, row1_y + row1_h]
    draw.rounded_rectangle(hs_box, radius=16, fill=CARD_PANEL, outline=CARD_BORDER, width=2)

    headshot_path = f'assets/headshots/{int(player_id)}.png'
    if not os.path.exists(headshot_path):
        headshot_path = 'assets/headshots/default.png'
    if os.path.exists(headshot_path):
        hs = Image.open(headshot_path).convert('RGBA')
        alpha = hs.getchannel('A')
        bbox = alpha.getbbox()
        if bbox is not None:
            hs = hs.crop(bbox)
        resampling = getattr(getattr(Image, 'Resampling', Image), 'LANCZOS', Image.BICUBIC)
        hs = ImageOps.contain(hs, (headshot_w - 26, row1_h - 26), method=resampling)

        hs_layer = Image.new('RGBA', (headshot_w - 26, row1_h - 26), CARD_PANEL)
        ox = int((hs_layer.size[0] - hs.size[0]) / 2)
        oy = int((hs_layer.size[1] - hs.size[1]) / 2)
        hs_layer.alpha_composite(hs, (ox, oy))
        canvas.paste(hs_layer.convert('RGB'), (CARD_MARGIN + 13, row1_y + 13))

    metric_x0 = CARD_MARGIN + headshot_w + CARD_GAP
    metric_total_w = CARD_W - CARD_MARGIN - metric_x0
    metric_w = int((metric_total_w - (3 * CARD_GAP)) / 4)
    metric_boxes = []
    for i in range(4):
        x0 = metric_x0 + i * (metric_w + CARD_GAP)
        metric_boxes.append([x0, row1_y, x0 + metric_w, row1_y + row1_h])

    for box, item in zip(metric_boxes, metrics):
        _draw_metric_tile(
            draw,
            box=box,
            label=item['label'],
            value=item['value'],
            delta=item['delta'],
        )

    row2_y = row1_y + row1_h + CARD_GAP
    row2_h = CARD_H - row2_y - CARD_MARGIN
    tile_w = int((CARD_W - (2 * CARD_MARGIN) - (2 * CARD_GAP)) / 3)

    figures = [fig_poss, fig_combo, fig_sc]
    fallbacks = [
        'No contract possibility data available.',
        'No historical/projection data available.',
        'No peer projection data available.',
    ]

    for i in range(3):
        x0 = CARD_MARGIN + i * (tile_w + CARD_GAP)
        tile = _fig_to_card_tile(
            fig=figures[i],
            width=tile_w,
            height=row2_h,
            fallback_text=fallbacks[i],
        )
        canvas.paste(tile, (x0, row2_y))

    out = io.BytesIO()
    canvas.save(out, format='PNG', optimize=True)
    return out.getvalue()


def _img(path, fallback=None):
    if path and os.path.exists(path):
        st.image(path, width='stretch')
    elif fallback and os.path.exists(fallback):
        st.image(fallback, width='stretch')


def _to_num(series_or_val):
    return pd.to_numeric(series_or_val, errors='coerce')


def _normalize_season_id(value):
    if value is None:
        return None
    s = str(value).strip()
    if not s:
        return None
    digits = re.sub(r'[^0-9]', '', s)
    if len(digits) == 8:
        return digits
    if len(digits) == 4:
        y = int(digits)
        return f'{y}{y + 1}'
    return None


def _season_label(season_id):
    sid = str(season_id)
    if len(sid) == 8 and sid.isdigit():
        return f'{sid[:4]}-{sid[4:]}'
    return sid


def _season_start_year(season_id):
    sid = _normalize_season_id(season_id)
    if sid is None:
        return None
    return int(sid[:4])


def _next_season_from_contracts(contracts_df):
    s_end = _to_num(contracts_df.get('seasonId_end')).dropna()
    if s_end.empty:
        return '20262027'

    mx = int(s_end.max())
    sid = str(mx)
    if len(sid) != 8 or not sid.isdigit():
        return '20262027'

    y0 = int(sid[:4]) + 1
    return f'{y0}{y0 + 1}'


def _discover_fa_sources(contracts_df):
    sources = {}

    base_path = 'data/skater_free_agents.csv'
    if os.path.exists(base_path):
        fa_base = load_skater_free_agents(base_path)
        season_col = None
        for col in ['seasonId', 'season_id', 'season', 'seasonId_start', 'season_start']:
            if col in fa_base.columns:
                season_col = col
                break

        if season_col is not None:
            sids = []
            for value in fa_base[season_col].dropna().unique().tolist():
                sid = _normalize_season_id(value)
                if sid is not None:
                    sids.append(sid)

            for sid in sorted(set(sids), key=int):
                sources[sid] = {
                    'path': base_path,
                    'season_col': season_col,
                    'season_id': sid,
                }
        else:
            sid = _next_season_from_contracts(contracts_df)
            sources[sid] = {
                'path': base_path,
                'season_col': None,
                'season_id': sid,
            }

    for path in sorted(glob.glob('data/skater_free_agents_*.csv')):
        m = re.search(r'(\d{8})', os.path.basename(path))
        if m is None:
            continue

        sid = m.group(1)
        if sid not in sources:
            sources[sid] = {
                'path': path,
                'season_col': None,
                'season_id': sid,
            }

    return sources


def _fmt_usd(x):
    if x is None or pd.isna(x):
        return 'N/A'

    x = float(x)
    ax = abs(x)
    if ax >= 1e9:
        return f'${x / 1e9:.2f}B'
    if ax >= 1e6:
        return f'${x / 1e6:.2f}M'
    if ax >= 1e3:
        return f'${x / 1e3:.0f}K'
    return f'${x:,.0f}'


def _fmt_delta_usd(x):
    if x is None or pd.isna(x):
        return 'N/A'

    x = float(x)
    sign = '+' if x >= 0 else '-'
    return f"{sign}${abs(x) / 1e6:.2f}M"


def _fmt_usd_whole(x):
    if x is None or pd.isna(x):
        return 'N/A'
    x = float(x)
    ax = abs(x)
    if ax >= 1e9:
        return f'${x / 1e9:.0f}B'
    if ax >= 1e6:
        return f'${x / 1e6:.0f}M'
    if ax >= 1e3:
        return f'${x / 1e3:.0f}K'
    return f'${x:,.0f}'


def _fmt_delta_usd_whole(x):
    if x is None or pd.isna(x):
        return 'N/A'
    x = float(x)
    ax = abs(x)
    sign = '+' if x >= 0 else '-'
    if ax >= 1e9:
        return f'{sign}${ax / 1e9:.0f}B'
    if ax >= 1e6:
        return f'{sign}${ax / 1e6:.0f}M'
    if ax >= 1e3:
        return f'{sign}${ax / 1e3:.0f}K'
    return f'{sign}${ax:,.0f}'


def _fmt_years(x):
    if x is None or pd.isna(x):
        return 'N/A'

    x = float(x)
    if abs(x - round(x)) < 1e-9:
        return f'{int(round(x))}'
    return f'{x:.1f}'


def _fmt_delta_years(x):
    if x is None or pd.isna(x):
        return 'N/A'
    return f'{float(x):+.0f} yrs'


def _fmt_delta_num(x):
    if x is None or pd.isna(x):
        return 'N/A'
    return f'{float(x):+.0f}'


def _fmt_pct(x):
    if x is None or pd.isna(x):
        return 'N/A'
    pct = f'{100.0 * float(x):.2f}'.rstrip('0').rstrip('.')
    return f'{pct}%'


def _fmt_delta_pp(x):
    if x is None or pd.isna(x):
        return 'N/A'
    pp = f'{100.0 * float(x):+.2f}'.rstrip('0').rstrip('.')
    return f'{pp} pp'


def _y_range(values, pad=1.0, lo=0.0):
    s = _to_num(values).dropna()
    if s.empty:
        return None
    vmin = float(s.min())
    vmax = float(s.max())
    return [max(lo, vmin - pad), vmax + pad]


def _build_possibilities(row, suffix):
    term_cols = [
        c for c in row.index
        if c.startswith('pTerm_') and c.endswith(f'_{suffix}')
    ]

    terms = sorted({int(c.split('_')[1]) for c in term_cols})
    records = []
    for t in terms:
        records.append(
            {
                'term': float(t),
                'prob': row.get(f'pTerm_{t}_{suffix}', float('nan')),
                'aav': row.get(f'xAAV_{t}_{suffix}', float('nan')),
            }
        )

    dp = pd.DataFrame(records)
    if dp.empty:
        return dp

    dp['prob'] = _to_num(dp['prob'])
    dp['aav'] = _to_num(dp['aav'])

    pmax = dp['prob'].max(skipna=True)
    if pd.notna(pmax) and pmax > 1.0:
        dp['prob'] = dp['prob'] / 100.0

    return dp.sort_values('term').reset_index(drop=True)


def _most_likely(dp):
    if dp is None or dp.empty:
        return float('nan'), float('nan')

    d = dp.dropna(subset=['prob']).copy()
    if d.empty:
        return float('nan'), float('nan')

    i = d['prob'].idxmax()
    term = _to_num(pd.Series([d.loc[i, 'term']])).iloc[0]
    aav = _to_num(pd.Series([d.loc[i, 'aav']])).iloc[0]

    return (float(term) if pd.notna(term) else float('nan'), float(aav) if pd.notna(aav) else float('nan'))


# Load static data.
bio = load_biographies().copy()
contracts = load_skater_contracts().copy()

bio['playerId'] = _to_num(bio.get('playerId'))
bio = bio.dropna(subset=['playerId']).copy()
bio['playerId'] = bio['playerId'].astype(int)
bio['menuName'] = bio.get('playerMenuName', bio.get('menuName', bio.get('playerFullName', ''))).astype(str).str.strip()

contracts['playerId'] = _to_num(contracts.get('playerId'))
contracts = contracts.dropna(subset=['playerId']).copy()
contracts['playerId'] = contracts['playerId'].astype(int)

for col in ['number', 'seasonId_start', 'seasonId_end', 'cap', 'age', 'term', 'aav']:
    contracts[col] = _to_num(contracts.get(col))

season_sources = _discover_fa_sources(contracts)
if not season_sources:
    st.error('No free-agent data found. Expected data/skater_free_agents.csv or data/skater_free_agents_<season>.csv.')
    st.stop()

season_ids = sorted(season_sources.keys(), key=lambda x: int(x), reverse=True)
season_labels = {_season_label(s): s for s in season_ids}
season_options = list(season_labels.keys())

# Set filters.
c_season, c_player, c_sign_with, c_download = st.columns(4, gap='small', vertical_alignment='bottom')
download_slot = c_download.empty()

with c_season:
    season_label = st.selectbox(
        'Season',
        season_options,
        index=0,
        key='sfa_season_label',
    )
season_id = season_labels[season_label]

source = season_sources[season_id]
fa = load_skater_free_agents(source['path']).copy()

season_col = source.get('season_col')
if season_col is not None and season_col in fa.columns:
    season_vec = fa[season_col].map(_normalize_season_id)
    fa = fa.loc[season_vec == str(season_id)].copy()

fa['playerId'] = _to_num(fa.get('playerId'))
fa = fa.dropna(subset=['playerId']).copy()
fa['playerId'] = fa['playerId'].astype(int)

if fa.empty:
    st.info('No free-agent rows available for the selected season.')
    st.stop()

with c_sign_with:
    sign_with_label = st.selectbox(
        'Sign With',
        list(SIGN_WITH_OPTIONS.keys()),
        index=0,
        key='sfa_sign_with_label',
    )
resign_suffix = SIGN_WITH_OPTIONS[sign_with_label]

available_ids = sorted(fa['playerId'].unique().tolist())
bio_sel = bio.loc[bio['playerId'].isin(set(available_ids))].copy()

id_to_name = {int(pid): f'Player {int(pid)}' for pid in available_ids}
id_to_name.update(dict(zip(bio_sel['playerId'].astype(int), bio_sel['menuName'])))

available_ids = sorted(available_ids, key=lambda pid: id_to_name.get(int(pid), f'Player {int(pid)}'))


def _name_for(pid):
    return id_to_name.get(int(pid), f'Player {int(pid)}')


# Default player mirrors skater_shot_analysis behavior:
# keep saved selection if valid, otherwise pick leader by projected total value.
default_player_id = None
value_records = []
for _, row in fa.iterrows():
    pid = int(row['playerId'])
    row_dp = _build_possibilities(row, resign_suffix)
    row_term, row_aav = _most_likely(row_dp)
    if pd.notna(row_term) and pd.notna(row_aav):
        total_value = float(row_term) * float(row_aav)
        value_records.append((pid, total_value))

if value_records:
    default_player_id = sorted(
        value_records,
        key=lambda x: (-x[1], _name_for(x[0])),
    )[0][0]


if 'sfa_player_id_saved' not in st.session_state:
    st.session_state['sfa_player_id_saved'] = None

saved_player_id = st.session_state['sfa_player_id_saved']
fallback_player_id = (
    default_player_id
    if (default_player_id is not None and default_player_id in available_ids)
    else (available_ids[0] if available_ids else None)
)
selected_player_id = saved_player_id if saved_player_id in available_ids else fallback_player_id

player_index = (
    available_ids.index(selected_player_id)
    if (selected_player_id is not None and selected_player_id in available_ids)
    else None
)

with c_player:
    player_id = st.selectbox(
        'Player',
        options=available_ids,
        format_func=_name_for,
        index=player_index,
        key='sfa_player_id',
        placeholder=('N/A' if not available_ids else None),
    )

st.session_state['sfa_player_id_saved'] = player_id

if player_id is None:
    st.info('Select a player.')
    st.stop()

fa_player = fa.loc[fa['playerId'] == int(player_id)].copy()
if fa_player.empty:
    st.info('No free-agent projection found for this player.')
    st.stop()

fa_row = fa_player.iloc[0]
dp = _build_possibilities(fa_row, resign_suffix)
likely_term, likely_aav = _most_likely(dp)

season_int = int(str(season_id))
player_hist = contracts.loc[contracts['playerId'] == int(player_id)].copy()
player_hist = player_hist.sort_values(['seasonId_end', 'number']).reset_index(drop=True)

hist_before = player_hist.loc[player_hist['seasonId_end'] < season_int].copy()
if hist_before.empty:
    hist_before = player_hist.copy()

last_contract = hist_before.iloc[-1] if not hist_before.empty else None

age_signing = _to_num(pd.Series([fa_row.get('age', float('nan'))])).iloc[0]
league_cap = _to_num(pd.Series([fa_row.get('cap', float('nan'))])).iloc[0]

prev_age = (float(last_contract['age']) if last_contract is not None and pd.notna(last_contract.get('age')) else float('nan'))
prev_cap = (float(last_contract['cap']) if last_contract is not None and pd.notna(last_contract.get('cap')) else float('nan'))
prev_term = (float(last_contract['term']) if last_contract is not None and pd.notna(last_contract.get('term')) else float('nan'))
prev_aav = (float(last_contract['aav']) if last_contract is not None and pd.notna(last_contract.get('aav')) else float('nan'))
prev_start_year = (
    _season_start_year(last_contract.get('seasonId_start'))
    if last_contract is not None else None
)
proj_start_year = _season_start_year(season_id)

# Age fields between contracts/free-agent datasets use different conventions.
# Use contract-time elapsed years for the delta so it is consistent with contract term.
if prev_start_year is not None and proj_start_year is not None:
    d_age = float(proj_start_year - prev_start_year)
else:
    d_age = float(age_signing) - prev_age if (pd.notna(age_signing) and pd.notna(prev_age)) else float('nan')
d_term = float(likely_term) - prev_term if (pd.notna(likely_term) and pd.notna(prev_term)) else float('nan')
d_aav = float(likely_aav) - prev_aav if (pd.notna(likely_aav) and pd.notna(prev_aav)) else float('nan')

proj_aav_pct = (
    float(likely_aav) / float(league_cap)
    if (pd.notna(likely_aav) and pd.notna(league_cap) and float(league_cap) > 0)
    else float('nan')
)
prev_aav_pct = (
    float(prev_aav) / float(prev_cap)
    if (pd.notna(prev_aav) and pd.notna(prev_cap) and float(prev_cap) > 0)
    else float('nan')
)
d_aav_pct = (
    float(proj_aav_pct) - float(prev_aav_pct)
    if (pd.notna(proj_aav_pct) and pd.notna(prev_aav_pct))
    else float('nan')
)

# Create row with headshot + metrics.
c_hs, c_m1, c_m2, c_m3, c_m4 = st.columns([0.75, 1, 1, 1, 1], gap='small', vertical_alignment='center')

with c_hs:
    with st.container(border=True):
        headshot_path = f'assets/headshots/{int(player_id)}.png'
        _img(headshot_path, fallback='assets/headshots/default.png')

with c_m1:
    with st.container(border=True):
        st.metric(
            'Age At Signing',
            value=('N/A' if pd.isna(age_signing) else f'{_fmt_years(age_signing)} yrs'),
            delta=('N/A' if pd.isna(d_age) else f'{_fmt_delta_num(d_age)} yrs'),
            delta_color='normal',
        )

with c_m2:
    with st.container(border=True):
        st.metric(
            'Projected Term',
            value=('N/A' if pd.isna(likely_term) else f'{_fmt_years(likely_term)} yrs'),
            delta=_fmt_delta_years(d_term),
            delta_color='normal',
        )

with c_m3:
    with st.container(border=True):
        st.metric(
            'Projected AAV',
            value=_fmt_usd(likely_aav),
            delta=_fmt_delta_usd(d_aav),
            delta_color='normal',
        )

with c_m4:
    with st.container(border=True):
        st.metric(
            'Projected AAV %',
            value=_fmt_pct(proj_aav_pct),
            delta=_fmt_delta_pp(d_aav_pct),
            delta_color='normal',
        )

# Create plots.
fig_poss = None
fig_combo = None
fig_sc = None

c1, c2, c3 = st.columns(3, gap='small', vertical_alignment='top')

with c1:
    with st.container(border=True):
        if dp.empty:
            st.info('No contract possibility data available for this player.')
        else:
            dp_plot = dp.dropna(subset=['aav']).copy()
            if dp_plot.empty:
                st.info('No contract possibility data available for this player.')
            else:
                dp_plot['prob_clamped'] = dp_plot['prob'].clip(lower=0.0, upper=1.0).fillna(0.0)
                dp_plot['msize'] = 10 + 30 * dp_plot['prob_clamped']
                dp_plot['prob_txt'] = dp_plot['prob_clamped'].apply(lambda p: '' if pd.isna(p) else f'{p:.0%}')

                fig_poss = go.Figure(
                    go.Scatter(
                        x=dp_plot['term'],
                        y=dp_plot['aav'],
                        mode='lines+markers+text',
                        line=dict(width=3),
                        marker=dict(size=dp_plot['msize'], sizemode='diameter'),
                        text=dp_plot['prob_txt'],
                        textposition='top center',
                        textfont=dict(size=13),
                        hovertemplate='Term: %{x:.0f} yrs<br>Projected AAV: $%{y:,.0f}<br>Probability: %{text}<extra></extra>',
                        showlegend=False,
                        cliponaxis=False,
                    )
                )

                fig_poss.update_layout(
                    title=dict(text='Contract Possibilities', x=0.5, xanchor='center'),
                    margin=dict(l=10, r=10, t=50, b=10),
                    xaxis=dict(
                        title='Term (years)',
                        tickmode='linear',
                        dtick=1,
                        range=[0.5, float(dp_plot['term'].max()) + 0.5],
                    ),
                    yaxis=dict(title='Projected AAV', tickprefix='$'),
                    height=PLOT_H,
                )

                fig_poss.update_xaxes(fixedrange=True)
                fig_poss.update_yaxes(fixedrange=True)

                st.plotly_chart(fig_poss, width='stretch', config={'displayModeBar': True})

with c2:
    with st.container(border=True):
        hist_proj = hist_before.copy()
        hist_proj = hist_proj.dropna(subset=['term', 'aav']).copy()
        hist_proj = hist_proj.sort_values(['seasonId_end', 'number']).reset_index(drop=True)
        hist_proj['contract_n'] = hist_proj.index + 1

        if hist_proj.empty or pd.isna(likely_term) or pd.isna(likely_aav):
            st.info('No historical/projection data available for contract projection chart.')
        else:
            last_n = int(hist_proj['contract_n'].iloc[-1])
            proj_n = last_n + 1

            term_range = _y_range(pd.concat([hist_proj['term'], pd.Series([likely_term])]), pad=1.0, lo=0.0)
            aav_range = _y_range(pd.concat([hist_proj['aav'], pd.Series([likely_aav])]), pad=1_000_000.0, lo=0.0)

            fig_combo = go.Figure()
            term_color = '#636EFA'
            aav_color = '#EF553B'
            last_term = float(hist_proj['term'].iloc[-1])
            last_aav = float(hist_proj['aav'].iloc[-1])

            fig_combo.add_trace(
                go.Scatter(
                    x=hist_proj['contract_n'],
                    y=hist_proj['term'],
                    mode='lines+markers',
                    line=dict(width=3, color=term_color),
                    marker=dict(size=7, color=term_color),
                    yaxis='y',
                    name='Term',
                    hoverinfo='skip',
                )
            )

            fig_combo.add_trace(
                go.Scatter(
                    x=hist_proj['contract_n'],
                    y=hist_proj['aav'],
                    mode='lines+markers',
                    line=dict(width=3, color=aav_color),
                    marker=dict(size=7, color=aav_color),
                    yaxis='y2',
                    name='AAV',
                    hoverinfo='skip',
                )
            )

            # Dashed projection segments are drawn as shapes so they do not add duplicate
            # hover rows at the crossover contract.
            fig_combo.add_shape(
                type='line',
                xref='x',
                yref='y',
                x0=last_n,
                y0=last_term,
                x1=proj_n,
                y1=float(likely_term),
                line=dict(color=term_color, width=3, dash='dot'),
            )

            fig_combo.add_shape(
                type='line',
                xref='x',
                yref='y2',
                x0=last_n,
                y0=last_aav,
                x1=proj_n,
                y1=float(likely_aav),
                line=dict(color=aav_color, width=3, dash='dot'),
            )

            fig_combo.add_trace(
                go.Scatter(
                    x=[proj_n],
                    y=[float(likely_term)],
                    mode='markers',
                    marker=dict(size=8, color=term_color),
                    yaxis='y',
                    showlegend=False,
                    hoverinfo='skip',
                )
            )

            fig_combo.add_trace(
                go.Scatter(
                    x=[proj_n],
                    y=[float(likely_aav)],
                    mode='markers',
                    marker=dict(size=8, color=aav_color),
                    yaxis='y2',
                    showlegend=False,
                    hoverinfo='skip',
                )
            )

            hover_x = hist_proj['contract_n'].astype(float).tolist() + [float(proj_n)]
            hover_term = hist_proj['term'].astype(float).tolist() + [float(likely_term)]
            hover_aav = hist_proj['aav'].astype(float).tolist() + [float(likely_aav)]
            hover_custom = list(zip(hover_term, hover_aav))

            fig_combo.add_trace(
                go.Scatter(
                    x=hover_x,
                    y=hover_term,
                    mode='markers',
                    marker=dict(size=18, color='rgba(0,0,0,0)'),
                    customdata=hover_custom,
                    hovertemplate='Contract: %{x:.0f}<br>Term: %{customdata[0]:.1f} yrs<br>AAV: $%{customdata[1]:,.0f}<extra></extra>',
                    showlegend=False,
                )
            )

            fig_combo.add_trace(
                go.Scatter(
                    x=hover_x,
                    y=hover_aav,
                    mode='markers',
                    marker=dict(size=18, color='rgba(0,0,0,0)'),
                    yaxis='y2',
                    customdata=hover_custom,
                    hovertemplate='Contract: %{x:.0f}<br>Term: %{customdata[0]:.1f} yrs<br>AAV: $%{customdata[1]:,.0f}<extra></extra>',
                    showlegend=False,
                )
            )

            fig_combo.update_layout(
                title=dict(text='Contract Projection', x=0.5, xanchor='center'),
                margin=dict(l=10, r=10, t=60, b=80),
                xaxis=dict(
                    title='Contract #',
                    tickmode='linear',
                    dtick=1,
                    range=[0.5, float(proj_n) + 0.5],
                    showspikes=False,
                ),
                yaxis=dict(
                    title='Term (years)',
                    range=term_range,
                    showspikes=False,
                ),
                yaxis2=dict(
                    title='AAV',
                    range=aav_range,
                    overlaying='y',
                    side='right',
                    tickprefix='$',
                    showspikes=False,
                ),
                legend=dict(
                    orientation='h',
                    x=0.5,
                    xanchor='center',
                    y=-0.18,
                    yanchor='top',
                ),
                hovermode='closest',
                height=PLOT_H,
            )

            fig_combo.update_xaxes(fixedrange=True, showspikes=False)
            fig_combo.update_yaxes(fixedrange=True, showspikes=False)
            fig_combo.update_layout(yaxis2=dict(fixedrange=True))

            st.plotly_chart(fig_combo, width='stretch', config={'displayModeBar': True})

with c3:
    with st.container(border=True):
        peer_rows = []
        for _, row in fa.iterrows():
            row_dp = _build_possibilities(row, resign_suffix)
            t_star, a_star = _most_likely(row_dp)
            if pd.notna(t_star) and pd.notna(a_star):
                peer_rows.append(
                    {
                        'playerId': int(row['playerId']),
                        'term': float(t_star),
                        'aav': float(a_star),
                    }
                )

        peers = pd.DataFrame(peer_rows)
        if peers.empty:
            st.info('No free-agent projections available for this selection.')
        else:
            peers['name'] = peers['playerId'].apply(_name_for)
            peers['is_player'] = peers['playerId'] == int(player_id)
            peers['term_int'] = pd.to_numeric(peers['term'], errors='coerce').round().astype('Int64')
            peers = peers.dropna(subset=['term_int', 'aav']).copy()
            peers['term_label'] = peers['term_int'].astype(str)

            if peers.empty:
                st.info('No free-agent projections available for this selection.')
            else:
                others = peers.loc[~peers['is_player']].copy()
                mine = peers.loc[peers['is_player']].copy()
                term_order = [str(t) for t in sorted(peers['term_int'].dropna().unique().tolist())]

                fig_sc = go.Figure()

                if not others.empty:
                    fig_sc.add_trace(
                        go.Box(
                            x=others['term_label'],
                            y=others['aav'],
                            boxpoints='all',
                            jitter=0.35,
                            pointpos=0.0,
                            marker=dict(
                                size=6,
                                opacity=0.45,
                                color='rgba(130,130,130,0.75)',
                            ),
                            line=dict(color='rgba(110,110,110,0.75)', width=1.5),
                            fillcolor='rgba(150,150,150,0.10)',
                            customdata=others[['name']].to_numpy(),
                            hoveron='points',
                            hovertemplate='Player: %{customdata[0]}<br>Projected Term: %{x} yrs<br>Projected AAV: $%{y:,.0f}<extra></extra>',
                            showlegend=False,
                        )
                    )

                if not mine.empty:
                    fig_sc.add_trace(
                        go.Scatter(
                            x=mine['term_label'],
                            y=mine['aav'],
                            mode='markers',
                            marker=dict(
                                size=14,
                                opacity=1.0,
                                symbol='star',
                                color='yellow',
                                line=dict(width=1, color='black'),
                            ),
                            customdata=mine[['name']].to_numpy(),
                            hovertemplate='Player: %{customdata[0]}<br>Projected Term: %{x} yrs<br>Projected AAV: $%{y:,.0f}<extra></extra>',
                            showlegend=False,
                        )
                    )

                fig_sc.update_layout(
                    title=dict(text='Projection vs. Other FAs', x=0.5, xanchor='center'),
                    margin=dict(l=10, r=10, t=50, b=10),
                    xaxis=dict(
                        title='Projected Term (years)',
                        type='category',
                        categoryorder='array',
                        categoryarray=term_order,
                    ),
                    yaxis=dict(title='Projected AAV', tickprefix='$'),
                    height=PLOT_H,
                )

                fig_sc.update_xaxes(fixedrange=True)
                fig_sc.update_yaxes(fixedrange=True)

                st.plotly_chart(fig_sc, width='stretch', config={'displayModeBar': True})

metric_payload = [
    {
        'label': 'Age At Signing',
        'value': ('N/A' if pd.isna(age_signing) else f'{_fmt_years(age_signing)} yrs'),
        'delta': ('N/A' if pd.isna(d_age) else f'{_fmt_delta_num(d_age)} yrs'),
    },
    {
        'label': 'Projected Term',
        'value': ('N/A' if pd.isna(likely_term) else f'{_fmt_years(likely_term)} yrs'),
        'delta': _fmt_delta_years(d_term),
    },
    {
        'label': 'Projected AAV',
        'value': _fmt_usd(likely_aav),
        'delta': _fmt_delta_usd(d_aav),
    },
    {
        'label': 'Projected AAV %',
        'value': _fmt_pct(proj_aav_pct),
        'delta': _fmt_delta_pp(d_aav_pct),
    },
]

download_error = None
card_png_bytes = None
try:
    card_png_bytes = _build_contract_card_png(
        player_id=player_id,
        player_name=_name_for(player_id),
        season_id=season_id,
        sign_with_label=sign_with_label,
        metrics=metric_payload,
        fig_poss=fig_poss,
        fig_combo=fig_combo,
        fig_sc=fig_sc,
    )
except Exception as exc:
    download_error = str(exc)

if card_png_bytes is None:
    download_slot.button('Download Card', disabled=True, use_container_width=True)
    if download_error:
        c_download.caption(f'Card export unavailable: {download_error}')
else:
    season_token = str(season_id)
    download_slot.download_button(
        'Download Card',
        data=card_png_bytes,
        file_name=f'contract_projection_{int(player_id)}_{season_token}.png',
        mime='image/png',
        use_container_width=True,
    )
