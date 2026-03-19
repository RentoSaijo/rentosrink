suppressPackageStartupMessages(library(dplyr))

SEASON_ID <- 20232024L
GOAL_GAME_ID <- 2023020004L
GOAL_EVENT_ID <- 34L
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2L) {
  GOAL_GAME_ID <- as.integer(args[[1]])
  GOAL_EVENT_ID <- as.integer(args[[2]])
}
INPUT_PATH <- file.path(
  "models",
  "passes",
  "data",
  sprintf("pbps_trimmed_%s.csv", SEASON_ID)
)
OUTPUT_PATH <- file.path(
  "models",
  "passes",
  "data",
  sprintf("goal_pass_animation_%s_%s.svg", GOAL_GAME_ID, GOAL_EVENT_ID)
)
EXPORT_MP4_SCRIPT <- file.path("models", "passes", "export_svg_mp4.mjs")

svg_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

fmt_num <- function(x) {
  formatC(x, format = "f", digits = 2)
}

pair_pass_rows <- function(sequence_rows) {
  if (nrow(sequence_rows) < 2L) {
    return(data.frame())
  }

  pairs <- vector("list", length = 0L)
  row_idx <- 1L

  while (row_idx < nrow(sequence_rows)) {
    current_row <- sequence_rows[row_idx, , drop = FALSE]
    next_row <- sequence_rows[row_idx + 1L, , drop = FALSE]

    if (
      identical(current_row$eventTypeDescKey[[1]], "pass") &&
      identical(next_row$eventTypeDescKey[[1]], "reception")
    ) {
      pairs[[length(pairs) + 1L]] <- data.frame(
        passPlayerId = current_row$playerId[[1]],
        receivePlayerId = next_row$playerId[[1]],
        teamId = current_row$eventOwnerTeamId[[1]],
        x0 = current_row$xCoord[[1]],
        y0 = current_row$yCoord[[1]],
        x1 = next_row$xCoord[[1]],
        y1 = next_row$yCoord[[1]]
      )
      row_idx <- row_idx + 2L
    } else {
      row_idx <- row_idx + 1L
    }
  }

  dplyr::bind_rows(pairs)
}

arc_path <- function(cx, cy, radius, start_deg, end_deg) {
  start_rad <- start_deg * pi / 180
  end_rad <- end_deg * pi / 180
  x0 <- cx + radius * cos(start_rad)
  y0 <- cy + radius * sin(start_rad)
  x1 <- cx + radius * cos(end_rad)
  y1 <- cy + radius * sin(end_rad)
  large_arc_flag <- if ((end_deg - start_deg) %% 360 > 180) 1 else 0
  sweep_flag <- if (end_deg > start_deg) 1 else 0

  sprintf(
    "M %s %s A %s %s 0 %s %s %s %s",
    fmt_num(x0),
    fmt_num(y0),
    fmt_num(radius),
    fmt_num(radius),
    large_arc_flag,
    sweep_flag,
    fmt_num(x1),
    fmt_num(y1)
  )
}

line_length <- function(x0, y0, x1, y1) {
  sqrt((x1 - x0) ^ 2 + (y1 - y0) ^ 2)
}

animated_arrow <- function(x0, y0, x1, y1, color, begin_sec, dur_sec, width) {
  len <- max(line_length(x0, y0, x1, y1), 0.01)
  ux <- (x1 - x0) / len
  uy <- (y1 - y0) / len
  head_len <- max(3.2, width * 2.4)
  head_half_width <- max(1.35, width * 1.35)
  shaft_x1 <- x1 - head_len * ux
  shaft_y1 <- y1 - head_len * uy
  left_x <- shaft_x1 + head_half_width * (-uy)
  left_y <- shaft_y1 + head_half_width * ux
  right_x <- shaft_x1 - head_half_width * (-uy)
  right_y <- shaft_y1 - head_half_width * ux

  sprintf(
    paste0(
      "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" ",
      "stroke=\"%s\" stroke-width=\"%s\" stroke-linecap=\"butt\" ",
      "stroke-dasharray=\"%s\" stroke-dashoffset=\"%s\" opacity=\"0\">",
      "<animate attributeName=\"opacity\" from=\"0\" to=\"1\" begin=\"%ss\" dur=\"0.01s\" fill=\"freeze\" />",
      "<animate attributeName=\"stroke-dashoffset\" from=\"%s\" to=\"0\" begin=\"%ss\" dur=\"%ss\" fill=\"freeze\" />",
      "</line>",
      "<polygon points=\"%s,%s %s,%s %s,%s\" fill=\"%s\" opacity=\"0\">",
      "<animate attributeName=\"opacity\" from=\"0\" to=\"1\" begin=\"%ss\" dur=\"0.05s\" fill=\"freeze\" />",
      "</polygon>"
    ),
    fmt_num(x0),
    fmt_num(y0),
    fmt_num(shaft_x1),
    fmt_num(shaft_y1),
    color,
    fmt_num(width),
    fmt_num(max(line_length(x0, y0, shaft_x1, shaft_y1), 0.01)),
    fmt_num(max(line_length(x0, y0, shaft_x1, shaft_y1), 0.01)),
    fmt_num(begin_sec),
    fmt_num(max(line_length(x0, y0, shaft_x1, shaft_y1), 0.01)),
    fmt_num(begin_sec),
    fmt_num(dur_sec),
    fmt_num(x1),
    fmt_num(y1),
    fmt_num(left_x),
    fmt_num(left_y),
    fmt_num(right_x),
    fmt_num(right_y),
    color,
    fmt_num(begin_sec + dur_sec - 0.02)
  )
}

goal_pulse <- function(x, y, begin_sec) {
  sprintf(
    paste0(
      "<circle cx=\"%s\" cy=\"%s\" r=\"1.8\" fill=\"#d62728\" opacity=\"0\">",
      "<animate attributeName=\"opacity\" from=\"0\" to=\"1\" begin=\"%ss\" dur=\"0.05s\" fill=\"freeze\" />",
      "<animate attributeName=\"r\" values=\"1.8;3.8;1.8\" begin=\"%ss\" dur=\"0.9s\" repeatCount=\"indefinite\" />",
      "</circle>"
    ),
    fmt_num(x),
    fmt_num(y),
    fmt_num(begin_sec),
    fmt_num(begin_sec + 0.05)
  )
}

rink_markings <- function() {
  center_line <- "<line x1=\"0\" y1=\"-43\" x2=\"0\" y2=\"43\" stroke=\"#c92a2a\" stroke-width=\"1\" />"
  blue_right <- "<line x1=\"25\" y1=\"-43\" x2=\"25\" y2=\"43\" stroke=\"#1c7ed6\" stroke-width=\"1\" />"
  blue_left <- "<line x1=\"-25\" y1=\"-43\" x2=\"-25\" y2=\"43\" stroke=\"#1c7ed6\" stroke-width=\"1\" />"
  goal_right <- "<line x1=\"89\" y1=\"-43\" x2=\"89\" y2=\"43\" stroke=\"#c92a2a\" stroke-width=\"1\" />"
  goal_left <- "<line x1=\"-89\" y1=\"-43\" x2=\"-89\" y2=\"43\" stroke=\"#c92a2a\" stroke-width=\"1\" />"

  faceoff_circles <- c(
    "<circle cx=\"0\" cy=\"0\" r=\"15\" fill=\"none\" stroke=\"#1c7ed6\" stroke-width=\"0.8\" />",
    "<circle cx=\"69\" cy=\"22\" r=\"15\" fill=\"none\" stroke=\"#c92a2a\" stroke-width=\"0.8\" />",
    "<circle cx=\"69\" cy=\"-22\" r=\"15\" fill=\"none\" stroke=\"#c92a2a\" stroke-width=\"0.8\" />",
    "<circle cx=\"-69\" cy=\"22\" r=\"15\" fill=\"none\" stroke=\"#c92a2a\" stroke-width=\"0.8\" />",
    "<circle cx=\"-69\" cy=\"-22\" r=\"15\" fill=\"none\" stroke=\"#c92a2a\" stroke-width=\"0.8\" />"
  )

  crease_right <- sprintf(
    "<path d=\"%s\" fill=\"none\" stroke=\"#1c7ed6\" stroke-width=\"0.8\" />",
    arc_path(89, 0, 6, -90, 90)
  )
  crease_left <- sprintf(
    "<path d=\"%s\" fill=\"none\" stroke=\"#1c7ed6\" stroke-width=\"0.8\" />",
    arc_path(-89, 0, 6, 90, 270)
  )

  c(center_line, blue_right, blue_left, goal_right, goal_left, faceoff_circles, crease_right, crease_left)
}

if (!file.exists(INPUT_PATH)) {
  stop(sprintf("Trimmed PBP file does not exist: %s", INPUT_PATH))
}

pbps_trimmed <- utils::read.csv(INPUT_PATH, stringsAsFactors = FALSE)

goal_sequence <- pbps_trimmed %>%
  dplyr::filter(
    gameId == GOAL_GAME_ID,
    eventIdGoal == GOAL_EVENT_ID
  ) %>%
  dplyr::arrange(sortOrder)

if (nrow(goal_sequence) == 0L) {
  stop(
    sprintf(
      "No rows found for gameId %s and eventIdGoal %s in %s.",
      GOAL_GAME_ID,
      GOAL_EVENT_ID,
      INPUT_PATH
    )
  )
}

pass_rows <- goal_sequence %>%
  dplyr::filter(eventTypeDescKey %in% c("pass", "reception")) %>%
  dplyr::arrange(sortOrder)

pass_pairs <- pair_pass_rows(pass_rows)
if (nrow(pass_pairs) > 0L) {
  pass_pairs <- pass_pairs %>%
    dplyr::filter(
      stats::complete.cases(x0, y0, x1, y1),
      x0 != x1 | y0 != y1
    )
}

goal_row <- goal_sequence %>%
  dplyr::filter(eventTypeDescKey == "goal", eventId == eventIdGoal) %>%
  dplyr::slice_tail(n = 1L)

if (nrow(goal_row) == 0L) {
  stop("Could not find the terminal goal row for the selected sequence.")
}

goal_team_id <- goal_row$eventOwnerTeamId[[1]]
pass_pairs$isAttackingTeam <- !is.na(pass_pairs$teamId) & pass_pairs$teamId == goal_team_id
pass_dur_sec <- 0.55
pass_spacing_sec <- 0.7
timeline_start_sec <- 0.35

svg_parts <- c(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"-105 -60 210 120\" width=\"1400\" height=\"900\">",
  "<defs>",
  "<clipPath id=\"rink-clip\">",
  "<rect x=\"-100\" y=\"-43\" width=\"200\" height=\"86\" rx=\"28\" ry=\"28\" />",
  "</clipPath>",
  "<style>",
  "text { font-family: Menlo, Consolas, monospace; fill: #111111; text-anchor: middle; }",
  ".small { font-size: 3.3px; }",
  ".title { font-size: 5px; font-weight: 600; }",
  ".legend { text-anchor: start; dominant-baseline: middle; }",
  "</style>",
  "</defs>",
  "<rect x=\"-105\" y=\"-60\" width=\"210\" height=\"120\" fill=\"#ffffff\" />",
  sprintf(
    "<text x=\"0\" y=\"-54\" class=\"title\">Goal Sequence Animation: gameId %s, goal eventId %s</text>",
    GOAL_GAME_ID,
    GOAL_EVENT_ID
  ),
  "<g transform=\"scale(1,-1)\">",
  "<rect x=\"-100\" y=\"-43\" width=\"200\" height=\"86\" rx=\"28\" ry=\"28\" fill=\"#f9fcff\" />",
  "<g clip-path=\"url(#rink-clip)\">"
)

svg_parts <- c(svg_parts, rink_markings())

if (nrow(pass_pairs) > 0L) {
  for (idx in seq_len(nrow(pass_pairs))) {
    begin_sec <- timeline_start_sec + (idx - 1L) * pass_spacing_sec
    pass_color <- if (isTRUE(pass_pairs$isAttackingTeam[[idx]])) "#1f77b4" else "#2b8a3e"

    svg_parts <- c(
      svg_parts,
      animated_arrow(
        x0 = pass_pairs$x0[[idx]],
        y0 = pass_pairs$y0[[idx]],
        x1 = pass_pairs$x1[[idx]],
        y1 = pass_pairs$y1[[idx]],
        color = pass_color,
        begin_sec = begin_sec,
        dur_sec = pass_dur_sec,
        width = 1.45
      )
    )
  }
}

shot_begin_sec <- timeline_start_sec + nrow(pass_pairs) * pass_spacing_sec

svg_parts <- c(
  svg_parts,
  goal_pulse(
    x = goal_row$xCoord[[1]],
    y = goal_row$yCoord[[1]],
    begin_sec = shot_begin_sec + 0.55
  ),
  "</g>",
  "<rect x=\"-100\" y=\"-43\" width=\"200\" height=\"86\" rx=\"28\" ry=\"28\" fill=\"none\" stroke=\"#111111\" stroke-width=\"1.1\" />",
  "</g>",
  "<g transform=\"translate(44, 47)\">",
  "<rect x=\"0\" y=\"0\" width=\"4\" height=\"2\" fill=\"#1f77b4\" />",
  "<text x=\"6\" y=\"1\" class=\"small legend\">Shooting Team Passes</text>",
  "<rect x=\"0\" y=\"4\" width=\"4\" height=\"2\" fill=\"#2b8a3e\" />",
  "<text x=\"6\" y=\"5\" class=\"small legend\">Defending Team Passes</text>",
  "<rect x=\"0\" y=\"8\" width=\"4\" height=\"2\" fill=\"#d62728\" />",
  "<text x=\"6\" y=\"9\" class=\"small legend\">Goal Shot</text>",
  "</g>",
  "</svg>"
)

writeLines(svg_parts, OUTPUT_PATH, useBytes = TRUE)

if (file.exists(EXPORT_MP4_SCRIPT)) {
  mp4_result <- tryCatch(
    system2(
      "node",
      args = c(EXPORT_MP4_SCRIPT, normalizePath(OUTPUT_PATH)),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(err) err
  )

  if (inherits(mp4_result, "error")) {
    warning(sprintf("Failed to export MP4: %s", mp4_result$message), call. = FALSE)
  }
}

cat(
  sprintf(
    "Saved %s using gameId %s and eventIdGoal %s (%s detected passes).\n",
    OUTPUT_PATH,
    GOAL_GAME_ID,
    GOAL_EVENT_ID,
    nrow(pass_pairs)
  )
)
