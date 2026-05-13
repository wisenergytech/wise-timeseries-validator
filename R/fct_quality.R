# ── Quality Detection Functions ───────────────────────────────────────────────
#
# Industry standards referenced:
#   - IEC 61724-2 §6.3: sensor malfunction detection (stuck, range)
#   - IEC 61724-3 §5.2: data availability and gap classification
#   - ASHRAE Guideline 14 §5.3.2: statistical outlier screening
#   - ISO 17025 / JCGM 100: measurement uncertainty principles

#' Detect gaps in a timeseries
#'
#' A gap is where the interval between consecutive timestamps exceeds the
#' expected time step. Gaps are classified by duration into 3 severity levels:
#'   - L1 (< gap_l1_hours): small, suitable for linear interpolation
#'   - L2 (gap_l1_hours to gap_l2_hours): medium, needs profile-based filling
#'   - L3 (> gap_l2_hours): large, should be excluded
#'
#' Ref: IEC 61724-3 §5.2 (data availability requirements)
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
  # Allow 50% tolerance for minor irregularities
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
    start = starts,
    end = ends,
    duration = durations,
    n_missing = n_missing,
    severity = severity
  )
}

#' Detect outliers using hybrid IQR method (values + deltas)
#'
#' Two complementary detection strategies:
#'   1. Absolute: values outside [Q1 - k*IQR, Q3 + k*IQR] (catches extreme
#'      magnitudes)
#'   2. Delta: consecutive differences outside [Q1 - k*IQR, Q3 + k*IQR]
#'      (catches sudden jumps even when absolute value is in range)
#'
#' A point flagged by either method is considered an outlier.
#'
#' Ref: ASHRAE Guideline 14 §5.3.2 (statistical outlier screening)
#'      Tukey, J.W. (1977). Exploratory Data Analysis.
#'
#' @param values Numeric vector.
#' @param timestamps POSIXct vector (same length as values).
#' @param k IQR multiplier for Tukey fences (default 3).
#' @return data.table with columns: timestamp, value, method
#'   (method = "absolute", "delta", or "both").
#' @noRd
detect_outliers_iqr <- function(values, timestamps, k = 3) {
  empty <- data.table::data.table(
    timestamp = as.POSIXct(character(0)),
    value = numeric(0),
    method = character(0)
  )

  non_na <- !is.na(values)
  clean_vals <- values[non_na]
  if (length(clean_vals) < 10) return(empty)

  # --- Strategy 1: Absolute value fences ---
  q_abs <- stats::quantile(clean_vals, probs = c(0.25, 0.75))
  iqr_abs <- q_abs[2] - q_abs[1]
  abs_outlier <- rep(FALSE, length(values))
  if (iqr_abs > 0) {
    lower_abs <- q_abs[1] - k * iqr_abs
    upper_abs <- q_abs[2] + k * iqr_abs
    abs_outlier <- non_na & (values < lower_abs | values > upper_abs)
  }

  # --- Strategy 2: Delta (consecutive difference) fences ---
  deltas <- c(NA_real_, diff(values))
  clean_deltas <- deltas[!is.na(deltas)]
  delta_outlier <- rep(FALSE, length(values))
  if (length(clean_deltas) >= 10) {
    q_delta <- stats::quantile(clean_deltas, probs = c(0.25, 0.75))
    iqr_delta <- q_delta[2] - q_delta[1]
    if (iqr_delta > 0) {
      lower_delta <- q_delta[1] - k * iqr_delta
      upper_delta <- q_delta[2] + k * iqr_delta
      is_delta_extreme <- !is.na(deltas) &
        (deltas < lower_delta | deltas > upper_delta)
      # A delta outlier flags the point that caused the jump
      delta_outlier <- is_delta_extreme
    }
  }

  # --- Combine ---
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

#' Detect stuck sensor segments using Run Length Encoding
#'
#' Consecutive identical values with run length >= min_run are flagged.
#' Also detects stuck-dump patterns: a stuck segment followed by a spike
#' that contains the accumulated energy from the stuck period.
#'
#' Ref: IEC 61724-2 §6.3 (sensor malfunction detection)
#'
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

#' Detect negative values in a quantity that should be non-negative
#'
#' Flags timesteps where the value is strictly negative. Generic check
#' applicable to energy, power, flow, or any physical quantity with a
#' natural zero floor.
#'
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
#'
#' completeness = non_missing / expected_count * 100
#'
#' Ref: IEC 61724-3 §5.2 (data availability requirements)
#'
#' @param values Numeric vector.
#' @param timestamps POSIXct vector.
#' @param time_step difftime expected interval.
#' @return Numeric percentage (0-100).
#' @noRd
compute_completeness <- function(values, timestamps, time_step) {
  step_secs <- as.numeric(time_step, units = "secs")
  range_secs <- as.numeric(difftime(max(timestamps), min(timestamps),
                                     units = "secs"))
  expected_count <- round(range_secs / step_secs) + 1
  non_missing <- sum(!is.na(values))
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
  stuck <- detect_stuck_rle(values, timestamps,
                            min_run = config$rle_min_run,
                            dump_factor = config$dump_factor)
  negatives <- detect_negatives(values, timestamps)
  completeness <- compute_completeness(values, timestamps, time_step)
  stats <- compute_stats(values)

  list(
    column_name = col_name,
    gaps = gaps,
    outliers = outliers,
    stuck_segments = stuck,
    negatives = negatives,
    completeness = completeness,
    stats = stats,
    gap_count = nrow(gaps),
    outlier_count = nrow(outliers),
    stuck_count = nrow(stuck),
    negative_count = nrow(negatives)
  )
}
