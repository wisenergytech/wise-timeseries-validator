# ── Programmatic API ──────────────────────────────────────────────────────────
# Exported functions for using the validator from R code (without the Shiny UI).
# Accepts either a file path (CSV) or a data.frame / data.table directly.

#' Diagnose a timeseries dataset
#'
#' Runs quality diagnostics (gaps, outliers, stuck sensors, completeness) on
#' each numeric column of a timeseries dataset. Does not modify the data.
#'
#' @param data A data.frame, data.table, or path to a CSV file.
#' @param timestamp_col Name of the timestamp column. If NULL, auto-detected.
#' @param value_cols Character vector of columns to diagnose. If NULL, all
#'   numeric columns are used.
#' @param config List of detection parameters:
#'   \describe{
#'     \item{iqr_k}{IQR multiplier for outlier detection (default 3).}
#'     \item{rle_min_run}{Minimum run length for stuck detection (default 6).}
#'     \item{dump_factor}{Minimum spike/median ratio for dump detection
#'       (default 5).}
#'     \item{gap_l1_hours}{Gap L1/L2 boundary in hours (default 1).}
#'     \item{gap_l2_hours}{Gap L2/L3 boundary in hours (default 24).}
#'   }
#' @return A list with:
#'   \describe{
#'     \item{diagnostics}{Named list of per-column diagnostic results.}
#'     \item{timestamp_col}{Detected or provided timestamp column name.}
#'     \item{value_cols}{Diagnosed column names.}
#'     \item{time_step}{Inferred time step (difftime).}
#'     \item{record_count}{Number of rows.}
#'   }
#' @export
diagnose_timeseries <- function(data,
                                timestamp_col = NULL,
                                value_cols = NULL,
                                config = list()) {
  dt <- .ingest(data)
  config <- .merge_config(config)

  # Detect timestamp column
  if (is.null(timestamp_col)) {
    timestamp_col <- detect_timestamp_col(dt)
    if (is.null(timestamp_col)) {
      stop("Could not auto-detect a timestamp column. ",
           "Please specify timestamp_col explicitly.")
    }
  }

  # Parse timestamps if character
  if (is.character(dt[[timestamp_col]])) {
    dt[[timestamp_col]] <- parse_timestamps(dt[[timestamp_col]])
  }

  # Sort by timestamp
  data.table::setorderv(dt, timestamp_col)
  timestamps <- dt[[timestamp_col]]
  time_step <- infer_time_step(timestamps)

  # Detect value columns
  if (is.null(value_cols)) {
    value_cols <- detect_numeric_cols(dt, timestamp_col)
  }

  if (length(value_cols) == 0) {
    stop("No numeric columns found to validate.")
  }

  # Run diagnostics
  diagnostics <- lapply(value_cols, function(col) {
    run_diagnostic(col, dt[[col]], timestamps, time_step, config)
  })
  names(diagnostics) <- value_cols

  list(
    diagnostics = diagnostics,
    timestamp_col = timestamp_col,
    value_cols = value_cols,
    time_step = time_step,
    record_count = nrow(dt)
  )
}

#' Clean a timeseries dataset
#'
#' Applies all cleaning operations (gap filling, stuck-dump redistribution,
#' outlier replacement) and returns the cleaned data with provenance tracking.
#'
#' @param data A data.frame, data.table, or path to a CSV file.
#' @param timestamp_col Name of the timestamp column. If NULL, auto-detected.
#' @param value_cols Character vector of columns to clean. If NULL, all
#'   numeric columns are used.
#' @param config List of detection/cleaning parameters (see
#'   \code{\link{diagnose_timeseries}} for details).
#' @return A list with:
#'   \describe{
#'     \item{data}{Cleaned data.table with all original columns.}
#'     \item{data_source}{data.table of provenance labels per value column
#'       (values: "measured", "interpolated", "redistributed",
#'       "outlier_replaced", "excluded").}
#'     \item{summary}{Per-column cleaning summary (counts by operation).}
#'     \item{diagnostics}{Post-cleaning diagnostic results.}
#'   }
#' @export
clean_timeseries <- function(data,
                             timestamp_col = NULL,
                             value_cols = NULL,
                             config = list()) {
  dt <- .ingest(data)
  config <- .merge_config(config)

  # Detect timestamp column
  if (is.null(timestamp_col)) {
    timestamp_col <- detect_timestamp_col(dt)
    if (is.null(timestamp_col)) {
      stop("Could not auto-detect a timestamp column. ",
           "Please specify timestamp_col explicitly.")
    }
  }

  # Parse timestamps if character
  if (is.character(dt[[timestamp_col]])) {
    dt[[timestamp_col]] <- parse_timestamps(dt[[timestamp_col]])
  }

  # Sort by timestamp
  data.table::setorderv(dt, timestamp_col)
  timestamps <- dt[[timestamp_col]]
  time_step <- infer_time_step(timestamps)

  # Detect value columns
  if (is.null(value_cols)) {
    value_cols <- detect_numeric_cols(dt, timestamp_col)
  }

  if (length(value_cols) == 0) {
    stop("No numeric columns found to clean.")
  }

  # Initialize data_source tracking
  sources <- list()
  for (col in value_cols) {
    sources[[col]] <- rep("measured", nrow(dt))
  }

  # Step 1: Fill gaps (expands rows)
  first_col <- value_cols[1]
  result <- fill_gaps(timestamps, dt[[first_col]], sources[[first_col]],
                      time_step)
  new_timestamps <- result$timestamps

  new_vals <- list()
  new_sources <- list()
  new_vals[[first_col]] <- result$values
  new_sources[[first_col]] <- result$sources

  if (length(value_cols) > 1) {
    for (col in value_cols[-1]) {
      res <- fill_gaps(timestamps, dt[[col]], sources[[col]], time_step)
      new_vals[[col]] <- res$values
      new_sources[[col]] <- res$sources
    }
  }

  # Build expanded data.table
  new_dt <- data.table::data.table(dummy = seq_along(new_timestamps))
  new_dt[[timestamp_col]] <- new_timestamps
  new_dt[["dummy"]] <- NULL
  for (col in value_cols) {
    new_dt[[col]] <- new_vals[[col]]
  }

  # Preserve non-validated columns (expand with NA)
  other_cols <- setdiff(names(dt), c(timestamp_col, value_cols))
  if (length(other_cols) > 0) {
    old_ts_num <- as.numeric(timestamps)
    new_ts_num <- as.numeric(new_timestamps)
    match_idx <- match(old_ts_num, new_ts_num)
    for (col in other_cols) {
      expanded <- rep(NA, length(new_timestamps))
      expanded[match_idx[!is.na(match_idx)]] <-
        dt[[col]][which(!is.na(match_idx))]
      new_dt[[col]] <- expanded
    }
  }

  timestamps <- new_timestamps

  # Step 2: Clean stuck-dump patterns
  for (col in value_cols) {
    stuck <- detect_stuck_rle(new_dt[[col]], timestamps,
                              min_run = config$rle_min_run,
                              dump_factor = config$dump_factor)
    if (nrow(stuck) > 0 && any(stuck$has_dump)) {
      result <- clean_stuck_dump(new_dt[[col]], timestamps,
                                 new_sources[[col]], stuck)
      new_dt[[col]] <- result$values
      new_sources[[col]] <- result$sources
    }
    # Also clean remaining stuck segments (without dump) by interpolation
    stuck_no_dump <- stuck[!stuck$has_dump, ]
    if (nrow(stuck_no_dump) > 0) {
      result <- clean_stuck(new_dt[[col]], new_sources[[col]],
                            stuck_no_dump)
      new_dt[[col]] <- result$values
      new_sources[[col]] <- result$sources
    }
  }

  # Step 3: Clean outliers
  for (col in value_cols) {
    outliers <- detect_outliers_iqr(new_dt[[col]], timestamps,
                                    k = config$iqr_k)
    if (nrow(outliers) > 0) {
      result <- clean_outliers(new_dt[[col]], new_sources[[col]],
                               outliers, timestamps)
      new_dt[[col]] <- result$values
      new_sources[[col]] <- result$sources
    }
  }

  # Build data_source data.table
  ds_dt <- data.table::as.data.table(new_sources)

  # Per-column summary
  .tbl_count <- function(tbl, label) {
    if (label %in% names(tbl)) as.integer(tbl[label]) else 0L
  }
  summary <- lapply(value_cols, function(col) {
    tbl <- table(new_sources[[col]])
    list(
      column = col,
      measured = .tbl_count(tbl, "measured"),
      interpolated = .tbl_count(tbl, "interpolated"),
      redistributed = .tbl_count(tbl, "redistributed"),
      reinterpolated = .tbl_count(tbl, "reinterpolated"),
      outlier_replaced = .tbl_count(tbl, "outlier_replaced"),
      excluded = .tbl_count(tbl, "excluded")
    )
  })
  names(summary) <- value_cols

  # Post-cleaning diagnostics
  diagnostics <- lapply(value_cols, function(col) {
    run_diagnostic(col, new_dt[[col]], timestamps, time_step, config)
  })
  names(diagnostics) <- value_cols

  list(
    data = new_dt,
    data_source = ds_dt,
    summary = summary,
    diagnostics = diagnostics
  )
}

# ── Internal helpers ─────────────────────────────────────────────────────────

#' Ingest data from CSV path or data.frame into data.table
#' @param data A data.frame, data.table, or character path to CSV.
#' @return A data.table (always a copy, never modifies the original).
#' @noRd
.ingest <- function(data) {
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) {
      stop("File not found: ", data)
    }
    return(parse_csv(data))
  }
  if (is.data.frame(data)) {
    return(data.table::as.data.table(data))
  }
  stop("data must be a file path (character), data.frame, or data.table.")
}

#' Merge user config with defaults
#' @noRd
.merge_config <- function(config) {
  defaults <- list(
    iqr_k = 3,
    rle_min_run = 6,
    dump_factor = 5,
    gap_l1_hours = 1,
    gap_l2_hours = 24
  )
  for (key in names(defaults)) {
    if (is.null(config[[key]])) config[[key]] <- defaults[[key]]
  }
  config
}
