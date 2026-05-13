# ── Quality Detection Functions ───────────────────────────────────────────────

#' Detect gaps in a timeseries
#'
#' A gap is where the interval between consecutive timestamps exceeds the
#' expected time step. Gaps are classified by duration.
#' @param timestamps Sorted POSIXct vector.
#' @param time_step difftime representing the expected interval.
#' @param config List with gap_l1_hours and gap_l2_hours thresholds.
#' @return data.table with columns: start, end, duration, severity.
#' @noRd
detect_gaps <- function(timestamps, time_step, config) {
  if (length(timestamps) < 2) {
    return(data.table::data.table(
      start = as.POSIXct(character(0)),
      end = as.POSIXct(character(0)),
      duration = numeric(0),
      severity = character(0)
    ))
  }

  diffs_secs <- diff(as.numeric(timestamps))
  step_secs <- as.numeric(time_step, units = "secs")
  # Allow 50% tolerance for floating point / minor irregularities
  gap_idx <- which(diffs_secs > step_secs * 1.5)

  if (length(gap_idx) == 0) {
    return(data.table::data.table(
      start = as.POSIXct(character(0)),
      end = as.POSIXct(character(0)),
      duration = numeric(0),
      severity = character(0)
    ))
  }

  starts <- timestamps[gap_idx]
  ends <- timestamps[gap_idx + 1]
  durations <- as.numeric(difftime(ends, starts, units = "hours"))

  l1 <- config$gap_l1_hours
  l2 <- config$gap_l2_hours

  severity <- ifelse(durations < l1, "L1",
               ifelse(durations < l2, "L2", "L3"))

  data.table::data.table(
    start = starts,
    end = ends,
    duration = durations,
    severity = severity
  )
}

#' Detect outliers using IQR/Tukey method
#'
#' Values outside [Q1 - k*IQR, Q3 + k*IQR] are flagged as outliers.
#' @param values Numeric vector.
#' @param timestamps POSIXct vector (same length as values).
#' @param k IQR multiplier (default 3).
#' @return data.table with columns: timestamp, value, lower_fence, upper_fence.
#' @noRd
detect_outliers_iqr <- function(values, timestamps, k = 3) {
  clean_vals <- values[!is.na(values)]

  if (length(clean_vals) < 4) {
    return(data.table::data.table(
      timestamp = as.POSIXct(character(0)),
      value = numeric(0),
      lower_fence = numeric(0),
      upper_fence = numeric(0)
    ))
  }

  q <- stats::quantile(clean_vals, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower <- q[1] - k * iqr
  upper <- q[2] + k * iqr

  outlier_idx <- which(!is.na(values) & (values < lower | values > upper))

  if (length(outlier_idx) == 0) {
    return(data.table::data.table(
      timestamp = as.POSIXct(character(0)),
      value = numeric(0),
      lower_fence = numeric(0),
      upper_fence = numeric(0)
    ))
  }

  data.table::data.table(
    timestamp = timestamps[outlier_idx],
    value = values[outlier_idx],
    lower_fence = lower,
    upper_fence = upper
  )
}

#' Detect stuck sensor segments using Run Length Encoding
#'
#' Consecutive identical values with run length >= min_run are flagged.
#' Also detects stuck-dump patterns: a stuck segment followed by a spike
#' that contains the accumulated energy from the stuck period.
#' @param values Numeric vector.
#' @param timestamps POSIXct vector (same length as values).
#' @param min_run Minimum run length to consider stuck (default 6).
#' @param dump_factor Minimum ratio of dump value to median for stuck-dump
#'   detection (default 5).
#' @return data.table with columns: start, end, value, run_length,
#'   has_dump, dump_idx, dump_value.
#' @noRd
detect_stuck_rle <- function(values, timestamps, min_run = 6,
                             dump_factor = 5) {
  empty <- data.table::data.table(
    start = as.POSIXct(character(0)),
    end = as.POSIXct(character(0)),
    value = numeric(0),
    run_length = integer(0),
    has_dump = logical(0),
    dump_idx = integer(0),
    dump_value = numeric(0)
  )

  if (length(values) < min_run) return(empty)

  r <- rle(values)
  stuck_idx <- which(r$lengths >= min_run & !is.na(r$values))

  if (length(stuck_idx) == 0) return(empty)

  # Compute cumulative positions
  ends_pos <- cumsum(r$lengths)
  starts_pos <- ends_pos - r$lengths + 1

  # Median of positive values for dump detection
  typical <- stats::median(values[values > 0 & !is.na(values)], na.rm = TRUE)

  # Check each stuck segment for a dump spike immediately after
  has_dump <- logical(length(stuck_idx))
  dump_idx <- integer(length(stuck_idx))
  dump_value <- numeric(length(stuck_idx))

  for (i in seq_along(stuck_idx)) {
    si <- stuck_idx[i]
    next_pos <- ends_pos[si] + 1
    if (next_pos <= length(values) && !is.na(values[next_pos]) &&
        !is.na(typical) && typical > 0 &&
        values[next_pos] > typical * dump_factor) {
      has_dump[i] <- TRUE
      dump_idx[i] <- next_pos
      dump_value[i] <- values[next_pos]
    }
  }

  data.table::data.table(
    start = timestamps[starts_pos[stuck_idx]],
    end = timestamps[ends_pos[stuck_idx]],
    value = r$values[stuck_idx],
    run_length = r$lengths[stuck_idx],
    has_dump = has_dump,
    dump_idx = dump_idx,
    dump_value = dump_value
  )
}

#' Compute completeness percentage
#'
#' completeness = non_missing / expected_count * 100
#' @param values Numeric vector.
#' @param timestamps POSIXct vector.
#' @param time_step difftime expected interval.
#' @return Numeric percentage (0-100).
#' @noRd
compute_completeness <- function(values, timestamps, time_step) {
  step_secs <- as.numeric(time_step, units = "secs")
  range_secs <- as.numeric(difftime(max(timestamps), min(timestamps), units = "secs"))
  expected_count <- round(range_secs / step_secs) + 1

  non_missing <- sum(!is.na(values))
  # actual rows present (may be fewer than expected if gaps exist)
  actual_rows <- length(values)

  # Completeness based on expected count
  round(non_missing / expected_count * 100, 1)
}

#' Compute basic statistics for a numeric vector
#' @param values Numeric vector.
#' @return Named list with min, max, mean, sd, Q1, Q3.
#' @noRd
compute_stats <- function(values) {
  clean <- values[!is.na(values)]
  if (length(clean) == 0) {
    return(list(min = NA, max = NA, mean = NA, sd = NA, Q1 = NA, Q3 = NA))
  }
  q <- stats::quantile(clean, probs = c(0.25, 0.75))
  list(
    min = min(clean),
    max = max(clean),
    mean = round(mean(clean), 2),
    sd = round(stats::sd(clean), 2),
    Q1 = q[1],
    Q3 = q[2]
  )
}

#' Run full diagnostic for a single column
#'
#' @param col_name Column name.
#' @param values Numeric vector of values.
#' @param timestamps Sorted POSIXct vector.
#' @param time_step difftime expected interval.
#' @param config List with iqr_k, rle_min_run, gap_l1_hours, gap_l2_hours.
#' @return List (QualityDiagnostic) with all diagnostic results.
#' @noRd
run_diagnostic <- function(col_name, values, timestamps, time_step, config) {
  gaps <- detect_gaps(timestamps, time_step, config)
  outliers <- detect_outliers_iqr(values, timestamps, k = config$iqr_k)
  stuck <- detect_stuck_rle(values, timestamps, min_run = config$rle_min_run)
  completeness <- compute_completeness(values, timestamps, time_step)
  stats <- compute_stats(values)

  list(
    column_name = col_name,
    gaps = gaps,
    outliers = outliers,
    stuck_segments = stuck,
    completeness = completeness,
    stats = stats,
    gap_count = nrow(gaps),
    outlier_count = nrow(outliers),
    stuck_count = nrow(stuck)
  )
}
