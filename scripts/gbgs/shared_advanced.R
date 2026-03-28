`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (length(x) == 1L && is.na(x)) return(y)
  x
}

load_existing_gbg_file <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }

  readr::read_csv(path, show_col_types = FALSE)
}

extract_existing_game_ids <- function(existing) {
  if (!"gameId" %in% names(existing)) {
    return(integer())
  }

  existing %>%
    dplyr::transmute(gameId = as.integer(gameId)) %>%
    dplyr::filter(!is.na(gameId)) %>%
    dplyr::distinct() %>%
    dplyr::pull(gameId) %>%
    as.integer()
}

append_gbg_rows <- function(existing, additions, id_cols) {
  if (nrow(existing) == 0L) {
    return(additions %>% dplyr::arrange(dplyr::across(dplyr::all_of(id_cols))))
  }

  if (nrow(additions) == 0L) {
    return(existing %>% dplyr::arrange(dplyr::across(dplyr::all_of(id_cols))))
  }

  all_cols <- union(names(existing), names(additions))

  for (col in setdiff(all_cols, names(existing))) {
    existing[[col]] <- NA
  }

  for (col in setdiff(all_cols, names(additions))) {
    additions[[col]] <- NA
  }

  dplyr::bind_rows(
    existing %>% dplyr::select(dplyr::all_of(all_cols)),
    additions %>% dplyr::select(dplyr::all_of(all_cols))
  ) %>%
    dplyr::distinct(!!!rlang::syms(id_cols), .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)))
}

normalize_gbg_strength <- function(x) {
  out <- stringr::str_to_lower(as.character(x))
  out <- stringr::str_trim(out)

  dplyr::case_when(
    out == "even-strength" ~ "ev",
    out == "power-play" ~ "pp",
    out == "penalty-kill" ~ "sh",
    stringr::str_detect(out, "^ev") | stringr::str_detect(out, "even") ~ "ev",
    stringr::str_detect(out, "^pp") | stringr::str_detect(out, "power") ~ "pp",
    stringr::str_detect(out, "^sh") |
      stringr::str_detect(out, "^pk") |
      stringr::str_detect(out, "short") |
      stringr::str_detect(out, "penalty\\s*-?\\s*kill") ~ "sh",
    TRUE ~ NA_character_
  )
}

flip_strength_code <- function(x) {
  dplyr::case_when(
    x == "pp" ~ "sh",
    x == "sh" ~ "pp",
    x == "ev" ~ "ev",
    TRUE ~ NA_character_
  )
}

make_expected_metric_cols <- function(
    metrics,
    strengths = c("ev", "pp", "sh")
) {
  out <- character()
  for (metric in metrics) {
    for (strength in strengths) {
      out <- c(out, paste0(metric, "_", strength))
    }
  }
  out
}

empty_metric_long <- function(id_name = "entityId") {
  tibble::tibble(
    !!id_name := integer(),
    gameId = integer(),
    strength = character(),
    metric = character(),
    value = double()
  )
}

summarise_entity_metric <- function(
    df,
    id_col,
    metric,
    value_col = "value",
    strength_col = "strength",
    valid_ids = NULL,
    out_id_col = NULL
) {
  out_id_col <- out_id_col %||% id_col

  if (nrow(df) == 0L) {
    return(empty_metric_long(out_id_col))
  }

  out <- df %>%
    dplyr::transmute(
      !!out_id_col := as.integer(.data[[id_col]]),
      gameId = as.integer(gameId),
      strength = as.character(.data[[strength_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(.data[[out_id_col]]), !is.na(strength), !is.na(value))

  if (!is.null(valid_ids)) {
    out <- out %>% dplyr::filter(.data[[out_id_col]] %in% valid_ids)
  }

  if (nrow(out) == 0L) {
    return(empty_metric_long(out_id_col))
  }

  out %>%
    dplyr::group_by(.data[[out_id_col]], gameId, strength) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(metric = metric, .before = value)
}

summarise_list_metric <- function(
    df,
    ids_col,
    metric,
    value_col = "value",
    strength_col = "strength",
    valid_ids = NULL,
    out_id_col = "playerId"
) {
  if (nrow(df) == 0L) {
    return(empty_metric_long(out_id_col))
  }

  out <- df %>%
    dplyr::transmute(
      ids = .data[[ids_col]],
      gameId = as.integer(gameId),
      strength = as.character(.data[[strength_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(strength), !is.na(value)) %>%
    tidyr::unnest_longer(ids, values_to = out_id_col) %>%
    dplyr::mutate(!!out_id_col := as.integer(.data[[out_id_col]])) %>%
    dplyr::filter(!is.na(.data[[out_id_col]]))

  if (!is.null(valid_ids)) {
    out <- out %>% dplyr::filter(.data[[out_id_col]] %in% valid_ids)
  }

  if (nrow(out) == 0L) {
    return(empty_metric_long(out_id_col))
  }

  out %>%
    dplyr::group_by(.data[[out_id_col]], gameId, strength) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(metric = metric, .before = value)
}

ensure_base_game_coverage <- function(base, attempts, label) {
  base_games <- base %>%
    dplyr::transmute(gameId = as.integer(gameId)) %>%
    dplyr::filter(!is.na(gameId)) %>%
    dplyr::distinct()

  attempt_games <- attempts %>%
    dplyr::transmute(gameId = as.integer(gameId)) %>%
    dplyr::filter(!is.na(gameId)) %>%
    dplyr::distinct()

  missing_games <- base_games %>%
    dplyr::anti_join(attempt_games, by = "gameId") %>%
    dplyr::pull(gameId)

  if (length(missing_games) > 0L) {
    stop(
      sprintf(
        "%s is missing sbss coverage for %d base games. First missing gameIds: %s",
        label,
        length(missing_games),
        paste(utils::head(sort(missing_games), 10L), collapse = ", ")
      )
    )
  }

  invisible(NULL)
}

build_gbg_output <- function(base, metric_long, id_cols, metrics) {
  expected_metric_cols <- make_expected_metric_cols(metrics)
  metric_id_cols <- base::intersect(id_cols, names(metric_long))

  wide <- if (nrow(metric_long) == 0L) {
    base
  } else {
    metric_long %>%
      dplyr::mutate(metric_col = paste0(metric, "_", strength)) %>%
      dplyr::select(dplyr::all_of(metric_id_cols), metric_col, value) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(metric_id_cols, "metric_col")))) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = metric_col,
        values_from = value,
        values_fill = 0
      )
  }

  out <- base %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::left_join(wide, by = metric_id_cols)

  missing_cols <- setdiff(expected_metric_cols, names(out))
  if (length(missing_cols) > 0L) {
    for (col in missing_cols) {
      out[[col]] <- 0
    }
  }

  out %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(expected_metric_cols), ~ dplyr::coalesce(as.numeric(.x), 0))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::select(dplyr::all_of(id_cols), dplyr::all_of(expected_metric_cols))
}
