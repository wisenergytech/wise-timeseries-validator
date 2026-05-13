# ── Cleaning Functions ────────────────────────────────────────────────────────
#
# Industry standards referenced:
#   - IEC 61724-3 §5.2: 3-tier gap filling (interpolation, profile, exclusion)
#   - IEC 61724-2 §6.3: sensor malfunction correction
#   - ASHRAE Guideline 14 §5.3.2: outlier replacement

# ============================================================================
# Gap Filling — 3-tier system (IEC 61724-3 §5.2)
# ============================================================================

#' Fill gaps by inserting missing timestamps and repairing values
#'
#' 3-tier gap repair strategy:
#'   - L1 (small, <= gap_l1_hours): linear interpolation between neighbors
#'   - L2 (medium, gap_l1_hours to gap_l2_hours): periodic profile from
#'     surrounding data (same hour-of-day, same day-type)
#'   - L3 (large, > gap_l2_hours): excluded (marked NA)
#'
#' @param timestamps Sorted POSIXct vector.
#' @param values Numeric vector.
#' @param sources Character vector of data_source labels.
#' @param time_step difftime expected interval.
#' @param config List with gap_l1_hours and gap_l2_hours thresholds.
#' @return List with timestamps, values, sources (all expanded).
#' @noRd
fill_gaps <- function(timestamps, values, sources, time_step,
                      config = list(gap_l1_hours = 1, gap_l2_hours = 24)) {
  step_secs <- as.numeric(time_step, units = "secs")
  n <- length(timestamps)
  if (n < 2) {
    return(list(timestamps = timestamps, values = values, sources = sources))
  }

  l1_secs <- config$gap_l1_hours * 3600
  l2_secs <- config$gap_l2_hours * 3600

  # Pre-compute hour and day-type for profile-based repair
  hours_all <- as.integer(format(timestamps, "%H"))
  is_weekend_all <- format(timestamps, "%u") %in% c("6", "7")

  new_ts <- timestamps
  new_vals <- values
  new_src <- sources

  # Find and insert all gaps
  i <- 1
  while (i < length(new_ts)) {
    gap_secs <- as.numeric(difftime(new_ts[i + 1], new_ts[i], units = "secs"))
    if (gap_secs > step_secs * 1.5) {
      n_missing <- round(gap_secs / step_secs) - 1
      if (n_missing > 0) {
        insert_ts <- new_ts[i] + step_secs * (1:n_missing)

        # Classify this gap
        tier <- if (gap_secs <= l1_secs) "L1"
          else if (gap_secs <= l2_secs) "L2"
          else "L3"

        insert_vals <- rep(NA_real_, n_missing)
        insert_src <- rep(paste0("gap_", tier), n_missing)

        new_ts <- c(new_ts[1:i], insert_ts, new_ts[(i + 1):length(new_ts)])
        new_vals <- c(new_vals[1:i], insert_vals,
                      new_vals[(i + 1):length(new_vals)])
        new_src <- c(new_src[1:i], insert_src,
                     new_src[(i + 1):length(new_src)])

        i <- i + n_missing + 1
        next
      }
    }
    i <- i + 1
  }

  # --- L1: Linear interpolation ---
  l1_idx <- which(new_src == "gap_L1")
  if (length(l1_idx) > 0) {
    interpolated <- zoo::na.approx(new_vals, na.rm = FALSE)
    for (idx in l1_idx) {
      if (!is.na(interpolated[idx])) {
        new_vals[idx] <- interpolated[idx]
        new_src[idx] <- "interpolated"
      } else {
        new_src[idx] <- "excluded"
      }
    }
  }

  # --- L2: Periodic profile (same hour + day-type from ±2 weeks) ---
  l2_idx <- which(new_src == "gap_L2")
  if (length(l2_idx) > 0) {
    # Compute hours and day-type for expanded series
    hours_exp <- as.integer(format(new_ts, "%H"))
    is_weekend_exp <- format(new_ts, "%u") %in% c("6", "7")

    # Indices of measured (non-gap) values for profile lookup
    measured_idx <- which(new_src == "measured" & !is.na(new_vals))

    for (idx in l2_idx) {
      target_hour <- hours_exp[idx]
      target_weekend <- is_weekend_exp[idx]

      # Window: ±2 weeks of measured data
      window_secs <- 14 * 86400
      ts_num <- as.numeric(new_ts[idx])
      in_window <- measured_idx[
        abs(as.numeric(new_ts[measured_idx]) - ts_num) <= window_secs]

      # Match hour + day-type
      matches <- in_window[
        hours_exp[in_window] == target_hour &
        is_weekend_exp[in_window] == target_weekend]

      if (length(matches) >= 3) {
        new_vals[idx] <- stats::median(new_vals[matches], na.rm = TRUE)
        new_src[idx] <- "profiled"
      } else {
        # Fallback: same hour, any day-type
        matches_any <- in_window[hours_exp[in_window] == target_hour]
        if (length(matches_any) >= 3) {
          new_vals[idx] <- stats::median(new_vals[matches_any], na.rm = TRUE)
          new_src[idx] <- "profiled"
        } else {
          # Last resort: overall median of window
          if (length(in_window) > 0) {
            new_vals[idx] <- stats::median(new_vals[in_window], na.rm = TRUE)
            new_src[idx] <- "profiled"
          } else {
            new_src[idx] <- "excluded"
          }
        }
      }
    }
  }

  # --- L3: Exclusion (mark NA, keep timestamps for alignment) ---
  l3_idx <- which(new_src == "gap_L3")
  if (length(l3_idx) > 0) {
    # Values stay NA
    new_src[l3_idx] <- "excluded"
  }

  list(timestamps = new_ts, values = new_vals, sources = new_src)
}

# ============================================================================
# Stuck Sensor Cleaning (IEC 61724-2 §6.3)
# ============================================================================

#' Clean stuck sensor segments by interpolation
#'
#' Replaces stuck values with linear interpolation between boundary neighbors.
#' Skips already-cleaned values (flag-once).
#' @param values Numeric vector.
#' @param sources Character vector of data_source labels.
#' @param stuck data.table from detect_stuck_rle.
#' @return List with values, sources.
#' @noRd
clean_stuck <- function(values, sources, stuck) {
  if (nrow(stuck) == 0) return(list(values = values, sources = sources))

  new_vals <- values
  new_src <- sources

  for (i in seq_len(nrow(stuck))) {
    stuck_val <- stuck$value[i]

    # Find indices of the stuck segment by matching value runs
    r <- rle(values)
    ends_pos <- cumsum(r$lengths)
    starts_pos <- ends_pos - r$lengths + 1

    for (j in seq_along(r$values)) {
      if (!is.na(r$values[j]) && r$values[j] == stuck_val &&
          r$lengths[j] >= stuck$run_length[i]) {
        seg_start <- starts_pos[j]
        seg_end <- ends_pos[j]

        left_val <- if (seg_start > 1) new_vals[seg_start - 1] else NA
        right_val <- if (seg_end < length(new_vals)) {
          new_vals[seg_end + 1]
        } else NA

        for (k in seg_start:seg_end) {
          if (new_src[k] != "measured") next

          if (!is.na(left_val) && !is.na(right_val)) {
            frac <- (k - seg_start + 1) / (seg_end - seg_start + 2)
            new_vals[k] <- left_val + frac * (right_val - left_val)
            new_src[k] <- "reinterpolated"
          } else if (!is.na(left_val) || !is.na(right_val)) {
            new_vals[k] <- if (!is.na(left_val)) left_val else right_val
            new_src[k] <- "reinterpolated"
          }
        }
        break
      }
    }
  }

  list(values = new_vals, sources = new_src)
}

#' Clean stuck-dump patterns by redistributing accumulated energy
#'
#' When a sensor accumulates energy during a communication failure (stuck period)
#' and then dumps it all in a single timestep, this function redistributes the
#' dump value across the stuck period using an hourly typical profile from
#' surrounding data (median of same hour-of-day within ±1 week).
#'
#' @param values Numeric vector.
#' @param timestamps POSIXct vector (same length as values).
#' @param sources Character vector of data_source labels.
#' @param stuck data.table from detect_stuck_rle (must include has_dump,
#'   dump_idx, dump_value columns).
#' @param window_size Number of timesteps before/after for typical profile
#'   (default 672, ~1 week at 15-min).
#' @return List with values, sources.
#' @noRd
clean_stuck_dump <- function(values, timestamps, sources, stuck,
                             window_size = 672) {
  dumps <- stuck[stuck$has_dump == TRUE, ]
  if (nrow(dumps) == 0) return(list(values = values, sources = sources))

  new_vals <- values
  new_src <- sources

  hours <- as.integer(format(timestamps, "%H"))

  for (i in seq_len(nrow(dumps))) {
    seg_start <- which(timestamps == dumps$start[i])
    seg_end <- which(timestamps == dumps$end[i])
    d_idx <- dumps$dump_idx[i]

    if (length(seg_start) == 0 || length(seg_end) == 0 ||
        d_idx < 1 || d_idx > length(values)) next

    seg_start <- seg_start[1]
    seg_end <- seg_end[1]
    dump_val <- dumps$dump_value[i]
    target_range <- seg_start:d_idx

    # Build typical profile per hour from surrounding data
    window_start <- max(1, seg_start - window_size)
    window_end <- min(length(values), d_idx + window_size)
    window_idx <- setdiff(window_start:window_end, target_range)

    stuck_hours <- hours[target_range]
    profile <- numeric(length(target_range))

    for (j in seq_along(target_range)) {
      same_hour <- window_idx[hours[window_idx] == stuck_hours[j]]
      vals <- new_vals[same_hour]
      vals <- vals[!is.na(vals) & vals > 0]
      profile[j] <- if (length(vals) >= 3) {
        stats::median(vals)
      } else {
        dump_val / length(target_range)
      }
    }

    profile_sum <- sum(profile)
    if (profile_sum > 0) {
      redistributed <- dump_val * profile / profile_sum
    } else {
      redistributed <- rep(dump_val / length(target_range),
                           length(target_range))
    }

    new_vals[target_range] <- redistributed
    new_src[target_range] <- "redistributed"
  }

  list(values = new_vals, sources = new_src)
}

# ============================================================================
# Outlier Cleaning (ASHRAE Guideline 14 §5.3.2)
# ============================================================================

#' Clean outliers by interpolation from nearest clean neighbors
#'
#' Replaces outlier values with linear interpolation between the nearest
#' non-outlier neighbors on each side. Handles consecutive outliers correctly
#' by searching past them for clean values.
#'
#' @param values Numeric vector.
#' @param sources Character vector of data_source labels.
#' @param outliers data.table from detect_outliers_iqr.
#' @param timestamps POSIXct vector.
#' @return List with values, sources.
#' @noRd
clean_outliers <- function(values, sources, outliers, timestamps) {
  if (nrow(outliers) == 0) return(list(values = values, sources = sources))

  new_vals <- values
  new_src <- sources

  outlier_times <- outliers$timestamp

  for (ot in outlier_times) {
    idx <- which(timestamps == ot)
    if (length(idx) == 0) next
    idx <- idx[1]

    # Flag-once: skip already-cleaned values
    if (new_src[idx] != "measured") next

    # Find nearest non-outlier neighbors
    left_val <- NA
    for (j in (idx - 1):1) {
      if (j < 1) break
      if (!(timestamps[j] %in% outlier_times) && !is.na(new_vals[j])) {
        left_val <- new_vals[j]
        break
      }
    }

    right_val <- NA
    for (j in (idx + 1):length(new_vals)) {
      if (j > length(new_vals)) break
      if (!(timestamps[j] %in% outlier_times) && !is.na(new_vals[j])) {
        right_val <- new_vals[j]
        break
      }
    }

    if (!is.na(left_val) && !is.na(right_val)) {
      new_vals[idx] <- (left_val + right_val) / 2
    } else if (!is.na(left_val)) {
      new_vals[idx] <- left_val
    } else if (!is.na(right_val)) {
      new_vals[idx] <- right_val
    }

    new_src[idx] <- "outlier_replaced"
  }

  list(values = new_vals, sources = new_src)
}
