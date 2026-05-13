# ── Quality Detection Functions ───────────────────────────────────────────────
#
# Detection powered by:
#   - forecast::tsoutliers (STL decomposition + IQR, handles seasonality)
#   - imputeTS::statsNA (gap statistics)
#   - Custom RLE (stuck-dump patterns — no package covers this)
#
# Industry standards:
#   - IEC 61724-2 §6.3: sensor malfunction detection
#   - IEC 61724-3 §5.2: data availability and gap classification
#   - ASHRAE Guideline 14 §5.3.2: statistical outlier screening

#' Detect gaps in a timeseries
#'
#' Gaps classified by duration into 3 severity levels:
#'   - L1 (<= gap_l1_hours): small, suitable for linear interpolation
#'   - L2 (gap_l1_hours to gap_l2_hours): medium, needs profile-based filling
#'   - L3 (> gap_l2_hours): large, should be excluded
#'
#' @param timestamps Sorted POSIXct vector.
#' @param time_step difftime representing the expected interval.
#' @param config List with gap_l1_hours and gap_l2_hours thresholds.
#' @return data.table with columns: start, end, duration, n_missing, severity.
#' @noRd
detect_gaps <- function(timestamps, time_step, config) {
  empty <- data.table::data.table(
    start = as.POSIXct(character(0)),
    end = as.POSIXct(character(0)),
    duration = numeric(0),
    n_missing = integer(0),
    severity = character(0)
  )
  if (length(timestamps) < 2) return(empty)

  diffs_secs <- diff(as.numeric(timestamps))
  step_secs <- as.numeric(time_step, units = "secs")
  gap_idx <- which(diffs_secs > step_secs * 1.5)

  if (length(gap_idx) == 0) return(empty)

  starts <- timestamps[gap_idx]
  ends <- timestamps[gap_idx + 1]
  durations <- as.numeric(difftime(ends, starts, units = "hours"))
  n_missing <- as.integer(round(diffs_secs[gap_idx] / step_secs)) - 1L

  l1 <- config$gap_l1_hours
  l2 <- config$gap_l2_hours
  severity <- ifelse(durations <= l1, "L1",
               ifelse(durations <= l2, "L2", "L3"))

  data.table::data.table(
    start = starts, end = ends,
    duration = durations, n_missing = n_missing,
    severity = severity
  )
}

#' Detect outliers using STL decomposition + IQR (seasonal-aware)
#'
#' Uses forecast::tsoutliers which decomposes the signal via STL (Seasonal
#' and Trend decomposition using Loess), then applies IQR fences on the
#' remainder component. This correctly handles periodic patterns that would
#' be false positives with naive IQR.
#'
#' Falls back to a simpler hybrid IQR (absolute + delta) when the series
#' is too short for STL decomposition (< 2 full periods).
#'
#' @param values Numeric vector.
#' @param timestamps POSIXct vector (same length as values).
#' @param k IQR multiplier for Tukey fences (default 3).
#' @param time_step difftime for frequency detection.
#' @return data.table with columns: timestamp, value, method.
#' @noRd
detect_outliers_iqr <- function(values, timestamps, k = 3,
                                time_step = NULL) {
  empty <- data.table::data.table(
    timestamp = as.POSIXct(character(0)),
    value = numeric(0),
    method = character(0)
  )

  non_na <- !is.na(values)
  if (sum(non_na) < 10) return(empty)

  # Try STL-based detection (forecast::tsoutliers)
  stl_idx <- tryCatch({
    # Determine frequency from time_step
    freq <- .infer_frequency(time_step)
    if (!is.null(freq) && freq >= 2 && length(values) >= freq * 2) {
      ts_obj <- stats::ts(values, frequency = freq)
      # tsoutliers returns indices and replacements
      outlier_info <- forecast::tsoutliers(ts_obj, iterate = 2)
      outlier_info$index
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(stl_idx) && length(stl_idx) > 0) {
    return(data.table::data.table(
      timestamp = timestamps[stl_idx],
      value = values[stl_idx],
      method = rep("stl_iqr", length(stl_idx))
    ))
  }

  # Fallback: hybrid IQR (absolute + delta)
  .detect_outliers_hybrid(values, timestamps, k)
}

#' Fallback hybrid outlier detection (absolute IQR + delta IQR)
#' @noRd
.detect_outliers_hybrid <- function(values, timestamps, k = 3) {
  empty <- data.table::data.table(
    timestamp = as.POSIXct(character(0)),
    value = numeric(0),
    method = character(0)
  )

  non_na <- !is.na(values)
  clean_vals <- values[non_na]
  if (length(clean_vals) < 10) return(empty)

  # Absolute value fences
  q_abs <- stats::quantile(clean_vals, probs = c(0.25, 0.75))
  iqr_abs <- q_abs[2] - q_abs[1]
  abs_outlier <- rep(FALSE, length(values))
  if (iqr_abs > 0) {
    abs_outlier <- non_na &
      (values < q_abs[1] - k * iqr_abs | values > q_abs[2] + k * iqr_abs)
  }

  # Delta fences
  deltas <- c(NA_real_, diff(values))
  clean_deltas <- deltas[!is.na(deltas)]
  delta_outlier <- rep(FALSE, length(values))
  if (length(clean_deltas) >= 10) {
    q_d <- stats::quantile(clean_deltas, probs = c(0.25, 0.75))
    iqr_d <- q_d[2] - q_d[1]
    if (iqr_d > 0) {
      delta_outlier <- !is.na(deltas) &
        (deltas < q_d[1] - k * iqr_d | deltas > q_d[2] + k * iqr_d)
    }
  }

  either <- which(abs_outlier | delta_outlier)
  if (length(either) == 0) return(empty)

  method <- ifelse(
    abs_outlier[either] & delta_outlier[either], "both",
    ifelse(abs_outlier[either], "absolute", "delta"))

  data.table::data.table(
    timestamp = timestamps[either],
    value = values[either],
    method = method
  )
}

#' Infer ts frequency from time_step
#' @noRd
.infer_frequency <- function(time_step) {
  if (is.null(time_step)) return(NULL)
  step_secs <- as.numeric(time_step, units = "secs")
  if (is.na(step_secs) || step_secs <= 0) return(NULL)
  # Daily seasonality: how many steps per day
  freq <- as.integer(round(86400 / step_secs))
  if (freq < 2) return(NULL)
  freq
}

#' Detect stuck sensor segments using Run Length Encoding
#'
#' Also detects stuck-dump patterns: a stuck segment followed by a spike.
#'
#' @param values Numeric vector.
#' @param timestamps POSIXct vector.
#' @param min_run Minimum run length to consider stuck (default 6).
#' @param dump_factor Minimum spike/median ratio for dump detection (default 5).
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

  ends_pos <- cumsum(r$lengths)
  starts_pos <- ends_pos - r$lengths + 1
  typical <- stats::median(values[values > 0 & !is.na(values)], na.rm = TRUE)

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

#' Detect negative values
#' @param values Numeric vector.
#' @param timestamps POSIXct vector.
#' @return data.table with columns: timestamp, value.
#' @noRd
detect_negatives <- function(values, timestamps) {
  neg_idx <- which(!is.na(values) & values < 0)
  if (length(neg_idx) == 0) {
    return(data.table::data.table(
      timestamp = as.POSIXct(character(0)),
      value = numeric(0)
    ))
  }
  data.table::data.table(
    timestamp = timestamps[neg_idx],
    value = values[neg_idx]
  )
}

#' Compute completeness percentage
#' @noRd
compute_completeness <- function(values, timestamps, time_step) {
  step_secs <- as.numeric(time_step, units = "secs")
  range_secs <- as.numeric(difftime(max(timestamps), min(timestamps),
                                     units = "secs"))
  expected_count <- round(range_secs / step_secs) + 1
  non_missing <- sum(!is.na(values))
  round(non_missing / expected_count * 100, 1)
}

#' Compute basic statistics
#' @noRd
compute_stats <- function(values) {
  clean <- values[!is.na(values)]
  if (length(clean) == 0) {
    return(list(min = NA, max = NA, mean = NA, sd = NA, Q1 = NA, Q3 = NA))
  }
  q <- stats::quantile(clean, probs = c(0.25, 0.75))
  list(
    min = min(clean), max = max(clean),
    mean = round(mean(clean), 2), sd = round(stats::sd(clean), 2),
    Q1 = q[1], Q3 = q[2]
  )
}

#' Run full diagnostic for a single column
#' @noRd
run_diagnostic <- function(col_name, values, timestamps, time_step, config) {
  gaps <- detect_gaps(timestamps, time_step, config)
  outliers <- detect_outliers_iqr(values, timestamps,
                                  k = config$iqr_k,
                                  time_step = time_step)
  stuck <- detect_stuck_rle(values, timestamps,
                            min_run = config$rle_min_run,
                            dump_factor = config$dump_factor)
  negatives <- detect_negatives(values, timestamps)
  completeness <- compute_completeness(values, timestamps, time_step)
  stats <- compute_stats(values)

  list(
    column_name = col_name,
    gaps = gaps, outliers = outliers,
    stuck_segments = stuck, negatives = negatives,
    completeness = completeness, stats = stats,
    gap_count = nrow(gaps), outlier_count = nrow(outliers),
    stuck_count = nrow(stuck), negative_count = nrow(negatives)
  )
}
