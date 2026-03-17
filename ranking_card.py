import io
import os

import pandas as pd
from PIL import Image, ImageDraw, ImageFont, ImageOps


SCALE = 2


def _s(value: int) -> int:
    return int(round(value * SCALE))


CARD_WIDTH = _s(1600)
CARD_HEIGHT = _s(900)
CARD_BG = '#272727'
CARD_WHITE = '#FFFFFF'
SUBTITLE_SEPARATOR = ' • '
TEAM_LOGO_TEMPLATE = 'assets/logos/{team_id}.png'
NHLSCRAPER_LOGO_PATH = 'assets/nhlscraper.png'
LETTER_LOGO_PATH = 'assets/letter_raw.png'

TITLE_BOX = (_s(0), _s(16), CARD_WIDTH, _s(64))
SUBTITLE_BOX = (_s(0), _s(80), CARD_WIDTH, _s(32))
FOOTER_BOX = (_s(160), _s(794), _s(1280), _s(80))

LEFT_LOGO_X = _s(24)
LEFT_NAME_X = _s(96)
LEFT_BAR_X = _s(416)
LEFT_VALUE_X = _s(640)

RIGHT_LOGO_X = _s(824)
RIGHT_NAME_X = _s(896)
RIGHT_BAR_X = _s(1216)
RIGHT_VALUE_X = _s(1440)

ROW_START_Y = _s(144)
ROW_STEP = _s(51)
ROW_COUNT = 12
NAME_BOX_WIDTH = _s(320)
VALUE_BOX_WIDTH = _s(128)
ROW_TEXT_HEIGHT = _s(44)
BAR_MAX_WIDTH = _s(224)
BAR_HEIGHT = _s(44)
BAR_RADIUS = _s(8)

FONT_NAMES_REGULAR = (
    'Helvetica.ttc',
    'Arial.ttf',
    'DejaVuSans.ttf',
    'LiberationSans-Regular.ttf',
)
FONT_NAMES_BOLD = (
    'Arial Bold.ttf',
    'Helvetica.ttc',
    'DejaVuSans-Bold.ttf',
    'LiberationSans-Bold.ttf',
)

BAR_COLOR_STOPS = (
    (0.0, (255, 80, 72)),
    (0.45, (255, 170, 40)),
    (0.75, (255, 225, 70)),
    (1.0, (90, 220, 120)),
)

SURNAME_PARTICLES = {
    'da',
    'de',
    'del',
    'della',
    'der',
    'di',
    'du',
    'la',
    'le',
    'st',
    'st.',
    'ten',
    'ter',
    'van',
    'von',
}

FOOTER_LINES = (
    (
        ('Raw data from ', False),
        ('NHL API & HTML Reports', True),
        (' collected via ', False),
        ('@nhlscraper', True),
        (' R package;', False),
    ),
    (
        ('models by ', False),
        ('@RentoSaijo', True),
        (' inspired by ', False),
        ('@EvolvingHockey', True),
        ('; ', False),
        ('{date_start}', True),
        ('-', False),
        ('{date_end}', True),
        ('.', False),
    ),
)


def _get_font(size: int, bold: bool = False):
    font_names = FONT_NAMES_BOLD if bold else FONT_NAMES_REGULAR
    for name in font_names:
        try:
            return ImageFont.truetype(name, size=max(1, int(size)))
        except OSError:
            continue
    return ImageFont.load_default()


def _text_bounds(draw: ImageDraw.ImageDraw, text: str, font) -> tuple[int, int, int, int]:
    left, top, right, bottom = draw.textbbox((0, 0), str(text), font=font)
    return int(left), int(top), int(right), int(bottom)


def _text_size(draw: ImageDraw.ImageDraw, text: str, font) -> tuple[int, int]:
    left, top, right, bottom = _text_bounds(draw, text, font)
    return right - left, bottom - top


def _fit_single_line_font(
    draw: ImageDraw.ImageDraw,
    text: str,
    box_width: int,
    box_height: int,
    bold: bool = False,
    max_size: int | None = None,
    min_size: int = 8,
):
    max_size = max_size or max(box_height * 2, min_size)
    for size in range(max_size, min_size - 1, -1):
        font = _get_font(size=size, bold=bold)
        text_width, text_height = _text_size(draw, text, font)
        if text_width <= box_width and text_height <= box_height:
            return font
    return _get_font(size=min_size, bold=bold)


def _fit_height_font(
    draw: ImageDraw.ImageDraw,
    box_height: int,
    bold: bool = False,
    max_size: int | None = None,
    min_size: int = 8,
    sample_text: str = 'Ag',
):
    max_size = max_size or max(box_height * 2, min_size)
    for size in range(max_size, min_size - 1, -1):
        font = _get_font(size=size, bold=bold)
        _, text_height = _text_size(draw, sample_text, font)
        if text_height <= box_height:
            return font
    return _get_font(size=min_size, bold=bold)


def _draw_single_line_text(
    draw: ImageDraw.ImageDraw,
    text: str,
    box: tuple[int, int, int, int],
    bold: bool = False,
    align: str = 'left',
    fill: str = CARD_WHITE,
    max_size: int | None = None,
):
    x, y, width, height = box
    font = _fit_single_line_font(draw, text, width, height, bold=bold, max_size=max_size)
    left, top, right, bottom = _text_bounds(draw, text, font)
    text_width = right - left
    text_height = bottom - top
    if align == 'center':
        text_x = x + int((width - text_width) / 2) - left
    elif align == 'right':
        text_x = x + width - text_width - left
    else:
        text_x = x - left
    text_y = y + int((height - text_height) / 2) - top
    draw.text((text_x, text_y), str(text), font=font, fill=fill)
    return font


def _interpolate_color(ratio: float) -> tuple[int, int, int]:
    clipped = max(0.0, min(1.0, float(ratio)))
    for idx in range(1, len(BAR_COLOR_STOPS)):
        start_stop, start_color = BAR_COLOR_STOPS[idx - 1]
        end_stop, end_color = BAR_COLOR_STOPS[idx]
        if clipped <= end_stop:
            span = end_stop - start_stop
            local = 0.0 if span <= 0 else (clipped - start_stop) / span
            return tuple(
                int(round(start_color[channel] + ((end_color[channel] - start_color[channel]) * local)))
                for channel in range(3)
            )
    return BAR_COLOR_STOPS[-1][1]


def _normalize_ratio(value: float, min_value: float, max_value: float) -> float:
    if pd.isna(value) or max_value <= min_value:
        return 0.5
    return max(0.0, min(1.0, (float(value) - min_value) / (max_value - min_value)))


def _goodness_ratio(value: float, min_value: float, max_value: float, lower_is_better: bool) -> float:
    base = _normalize_ratio(value, min_value, max_value)
    return 1.0 - base if lower_is_better else base


def _bar_width(value: float, min_value: float, max_value: float) -> int:
    if max_value <= min_value:
        return int(round(BAR_MAX_WIDTH / 2))
    return int(round(_normalize_ratio(value, min_value, max_value) * BAR_MAX_WIDTH))


def _sanitize_teams_label(selected_teams: list[str] | tuple[str, ...] | None) -> str:
    if not selected_teams:
        return 'All Teams'
    return '/'.join(sorted({str(team).strip().upper() for team in selected_teams if str(team).strip()}))


def _format_compact_name(name: str) -> str:
    parts = [part for part in str(name).strip().split() if part]
    if not parts:
        return ''
    if len(parts) == 1:
        return parts[0]
    last_parts = [parts[-1]]
    if len(parts) >= 3 and parts[-2].lower() in SURNAME_PARTICLES:
        last_parts = [parts[-2], parts[-1]]
    return f'{parts[0][0].upper()}. {" ".join(last_parts)}'


def _truncate_text(draw: ImageDraw.ImageDraw, text: str, font, max_width: int) -> str:
    if _text_size(draw, text, font)[0] <= max_width:
        return text
    ellipsis = '...'
    if '. ' in text:
        prefix, tail = text.split('. ', 1)
        prefix = f'{prefix}. '
        candidate_tail = tail
        while candidate_tail:
            candidate = f'{prefix}{candidate_tail}{ellipsis}'
            if _text_size(draw, candidate, font)[0] <= max_width:
                return candidate
            candidate_tail = candidate_tail[:-1]
        return ellipsis

    candidate = text
    while candidate:
        shrunk = f'{candidate}{ellipsis}'
        if _text_size(draw, shrunk, font)[0] <= max_width:
            return shrunk
        candidate = candidate[:-1]
    return ellipsis


def _format_stat_value(value: float, statistic: str) -> str:
    if pd.isna(value):
        value = 0.0
    numeric = float(value)
    if '%' in str(statistic):
        return f'{numeric:.1f}%'
    return f'{numeric:.2f}'


def _draw_bar(draw: ImageDraw.ImageDraw, x: int, y: int, width: int, color: tuple[int, int, int]):
    if width <= 0:
        return
    draw.rounded_rectangle(
        [x, y, x + width - 1, y + BAR_HEIGHT - 1],
        radius=BAR_RADIUS,
        fill=color,
        outline=color,
        width=1,
    )


def _resample_mode():
    return getattr(getattr(Image, 'Resampling', Image), 'LANCZOS', Image.BICUBIC)


def _paste_contained_image(canvas: Image.Image, path: str, x: int, y: int, width: int, height: int):
    if not os.path.exists(path):
        return
    image = Image.open(path).convert('RGBA')
    image = ImageOps.contain(image, (width, height), method=_resample_mode())
    layer = Image.new('RGBA', (width, height), (0, 0, 0, 0))
    offset_x = int((width - image.size[0]) / 2)
    offset_y = int((height - image.size[1]) / 2)
    layer.alpha_composite(image, (offset_x, offset_y))
    canvas.alpha_composite(layer, (x, y))


def _fit_footer_fonts(draw: ImageDraw.ImageDraw, lines, box_width: int, box_height: int):
    spacing = _s(4)
    for size in range(_s(32), _s(11) - 1, -1):
        regular_font = _get_font(size=size, bold=False)
        bold_font = _get_font(size=size, bold=True)
        line_widths = []
        line_heights = []
        for line in lines:
            width = 0
            height = 0
            for text, is_bold in line:
                font = bold_font if is_bold else regular_font
                seg_width, seg_height = _text_size(draw, text, font)
                width += seg_width
                height = max(height, seg_height)
            line_widths.append(width)
            line_heights.append(height)
        total_height = sum(line_heights) + (spacing * (len(lines) - 1))
        if max(line_widths) <= box_width and total_height <= box_height:
            return regular_font, bold_font, spacing
    return _get_font(size=_s(12), bold=False), _get_font(size=_s(12), bold=True), spacing


def _draw_footer(draw: ImageDraw.ImageDraw, box: tuple[int, int, int, int], date_start_text: str, date_end_text: str):
    x, y, width, height = box
    lines = tuple(
        tuple((text.format(date_start=date_start_text, date_end=date_end_text), is_bold) for text, is_bold in line)
        for line in FOOTER_LINES
    )
    regular_font, bold_font, spacing = _fit_footer_fonts(draw, lines, width, height)
    line_heights = []
    for line in lines:
        max_height = 0
        for text, is_bold in line:
            font = bold_font if is_bold else regular_font
            _, seg_height = _text_size(draw, text, font)
            max_height = max(max_height, seg_height)
        line_heights.append(max_height)

    total_height = sum(line_heights) + (spacing * (len(lines) - 1))
    current_y = y + int((height - total_height) / 2)
    for line, line_height in zip(lines, line_heights):
        line_width = 0
        max_ascent = 0
        for text, is_bold in line:
            font = bold_font if is_bold else regular_font
            seg_width, _ = _text_size(draw, text, font)
            line_width += seg_width
            ascent, _ = font.getmetrics()
            max_ascent = max(max_ascent, ascent)
        current_x = x + int((width - line_width) / 2)
        baseline_y = current_y + max_ascent
        for text, is_bold in line:
            font = bold_font if is_bold else regular_font
            left, _, right, _ = _text_bounds(draw, text, font)
            ascent, _ = font.getmetrics()
            draw.text((current_x - left, baseline_y - ascent), text, font=font, fill=CARD_WHITE)
            current_x += right - left
        current_y += line_height + spacing


def _sorted_rankings(df_in: pd.DataFrame, statistic: str, lower_is_better: bool) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    source = df_in[['playerMenuName', 'teamId', statistic]].copy()
    source[statistic] = pd.to_numeric(source[statistic], errors='coerce')
    source['playerMenuName'] = source['playerMenuName'].fillna('').astype(str).str.strip()
    source = source.dropna(subset=[statistic]).copy()
    source = source.loc[source['playerMenuName'] != ''].copy()
    top = source.sort_values([statistic, 'playerMenuName'], ascending=[lower_is_better, True]).head(ROW_COUNT).reset_index(drop=True)
    bottom = source.sort_values([statistic, 'playerMenuName'], ascending=[not lower_is_better, True]).head(ROW_COUNT).reset_index(drop=True)
    return source.reset_index(drop=True), top, bottom


def _draw_player_rows(
    canvas: Image.Image,
    draw: ImageDraw.ImageDraw,
    rows: pd.DataFrame,
    statistic: str,
    all_values: pd.Series,
    lower_is_better: bool,
    logo_x: int,
    name_x: int,
    bar_x: int,
    value_x: int,
):
    min_value = float(pd.to_numeric(all_values, errors='coerce').min()) if not all_values.empty else 0.0
    max_value = float(pd.to_numeric(all_values, errors='coerce').max()) if not all_values.empty else 0.0

    row_font = _fit_height_font(draw, ROW_TEXT_HEIGHT, bold=True, max_size=_s(42), sample_text='Ag')

    for idx, row in rows.iterrows():
        row_y = ROW_START_Y + (idx * ROW_STEP)
        team_id = pd.to_numeric(row.get('teamId'), errors='coerce')
        if pd.notna(team_id):
            _paste_contained_image(canvas, TEAM_LOGO_TEMPLATE.format(team_id=int(team_id)), logo_x, row_y, _s(51), _s(51))

        compact_name = _format_compact_name(row.get('playerMenuName', ''))
        display_name = _truncate_text(draw, compact_name, row_font, NAME_BOX_WIDTH)
        name_left, name_top, _, _ = _text_bounds(draw, display_name, row_font)
        name_y = row_y + _s(4) + int((ROW_TEXT_HEIGHT - _text_size(draw, display_name, row_font)[1]) / 2) - name_top
        draw.text((name_x - name_left, name_y), display_name, font=row_font, fill=CARD_WHITE)

        value = float(pd.to_numeric(row.get(statistic), errors='coerce'))
        width = _bar_width(value, min_value, max_value)
        color = _interpolate_color(_goodness_ratio(value, min_value, max_value, lower_is_better))
        _draw_bar(draw, bar_x, row_y + _s(4), width, color)

        value_text = _format_stat_value(value, statistic)
        value_left, value_top, value_right, _ = _text_bounds(draw, value_text, row_font)
        value_width = value_right - value_left
        value_y = row_y + _s(4) + int((ROW_TEXT_HEIGHT - _text_size(draw, value_text, row_font)[1]) / 2) - value_top
        draw.text((value_x + VALUE_BOX_WIDTH - value_width - value_left, value_y), value_text, font=row_font, fill=CARD_WHITE)


def _format_date_text(value) -> str:
    if value is None or pd.isna(value):
        return 'Unknown'
    ts = pd.to_datetime(value, errors='coerce')
    if pd.isna(ts):
        return 'Unknown'
    return ts.strftime('%Y/%m/%d')


def build_ranking_card_png(
    ranked_df: pd.DataFrame,
    statistic: str,
    position_label: str,
    season_label: str,
    game_type_label: str,
    situation_label: str,
    selected_teams: list[str] | tuple[str, ...] | None,
    minimum_filter_label: str,
    lower_is_better: bool,
    range_start_date,
    range_end_date,
) -> bytes:
    if statistic not in ranked_df.columns:
        raise ValueError(f'Missing statistic column: {statistic}')

    eligible, top_rows, bottom_rows = _sorted_rankings(ranked_df, statistic, lower_is_better)
    if eligible.empty:
        raise ValueError('No ranking data available for card export.')

    canvas = Image.new('RGBA', (CARD_WIDTH, CARD_HEIGHT), CARD_BG)
    draw = ImageDraw.Draw(canvas)

    title = f'Top & Bottom 12 {position_label} by {statistic}'
    teams_label = _sanitize_teams_label(selected_teams)
    subtitle = SUBTITLE_SEPARATOR.join((season_label, game_type_label, situation_label, teams_label, minimum_filter_label))

    _draw_single_line_text(draw, title, TITLE_BOX, bold=True, align='center', max_size=_s(64))
    _draw_single_line_text(draw, subtitle, SUBTITLE_BOX, bold=True, align='center', max_size=_s(32))

    _draw_player_rows(
        canvas=canvas,
        draw=draw,
        rows=top_rows,
        statistic=statistic,
        all_values=eligible[statistic],
        lower_is_better=lower_is_better,
        logo_x=LEFT_LOGO_X,
        name_x=LEFT_NAME_X,
        bar_x=LEFT_BAR_X,
        value_x=LEFT_VALUE_X,
    )
    _draw_player_rows(
        canvas=canvas,
        draw=draw,
        rows=bottom_rows,
        statistic=statistic,
        all_values=eligible[statistic],
        lower_is_better=lower_is_better,
        logo_x=RIGHT_LOGO_X,
        name_x=RIGHT_NAME_X,
        bar_x=RIGHT_BAR_X,
        value_x=RIGHT_VALUE_X,
    )

    _paste_contained_image(canvas, NHLSCRAPER_LOGO_PATH, _s(64), _s(788), _s(96), _s(96))
    _paste_contained_image(canvas, LETTER_LOGO_PATH, _s(1440), _s(772), _s(128), _s(128))
    _draw_footer(draw, FOOTER_BOX, _format_date_text(range_start_date), _format_date_text(range_end_date))

    for start, end in (
        ((_s(0), _s(2)), (CARD_WIDTH - 1, _s(2))),
        ((_s(0), _s(126)), (CARD_WIDTH - 1, _s(126))),
        ((_s(0), _s(770)), (CARD_WIDTH - 1, _s(770))),
        ((_s(0), _s(898)), (CARD_WIDTH - 1, _s(898))),
        ((_s(2), _s(0)), (_s(2), CARD_HEIGHT - 1)),
        ((_s(1598), _s(0)), (_s(1598), CARD_HEIGHT - 1)),
    ):
        draw.line([start, end], fill=CARD_WHITE, width=_s(4))

    output = io.BytesIO()
    canvas.convert('RGB').save(output, format='PNG')
    return output.getvalue()
