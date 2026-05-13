# ── Cleaning Functions ────────────────────────────────────────────────────────
#
# Cleaning powered by:
#   - imputeTS::na_interpolation (L1 gap filling — linear/spline)
#   - imputeTS::na_seadec (L2 gap filling — seasonal decomposition)
#   - forecast::tsoutliers (outlier replacement values)
#   - Custom RLE redistribution (stuck-dump — no package covers this)
#
# Industry standards:
#   - IEC 61724-3 §5.2: 3-tier gap filling
#   - IEC 61724-2 §6.3: sensor malfunction correction
#   - ASHRAE Guideline 14 §5.3.2: outlier replacement

# ============================================================================
# Gap Filling — 3-tier (IEC 61724-3 §5.2)
# ============================================================================

#' Fill gaps by inserting missing timestamps and repairing values
#'
#' 3-tier strategy:
#'   - L1 (<= gap_l1_hours): linear interpolation (imputeTS::na_interpolation)
#'   - L2 (gap_l1_hours to gap_l2_hours): seasonal decomposition
#'     (imputeTS::na_seadec) with fallback to periodic profile
#'   - L3 (> gap_l2_hours): excluded (marked NA)
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

  new_ts <- timestamps
  new_vals <- values
  new_src <- sources

  # Pass 1: Insert missing timestamps with tier labels
  i <- 1
  while (i < length(new_ts)) {
    gap_secs <- as.numeric(difftime(new_ts[i + 1], new_ts[i], units = "secs"))
    if (gap_secs > step_secs * 1.5) {
      n_missing <- round(gap_secs / step_secs) - 1
      if (n_missing > 0) {
        insert_ts <- new_ts[i] + step_secs * (1:n_missing)
        tier <- if (gap_secs <= l1_secs) "gap_L1"
          else if (gap_secs <= l2_secs) "gap_L2"
          else "gap_L3"

        new_ts <- c(new_ts[1:i], insert_ts,
                    new_ts[(i + 1):length(new_ts)])
        new_vals <- c(new_vals[1:i], rep(NA_real_, n_missing),
                      new_vals[(i + 1):length(new_vals)])
        new_src <- c(new_src[1:i], rep(tier, n_missing),
                     new_src[(i + 1):length(new_src)])
        i <- i + n_missing + 1
        next
      }
    }
    i <- i + 1
  }

  # Pass 2: Fill L1 gaps (linear interpolation via imputeTS)
  l1_idx <- which(new_src == "gap_L1")
  if (length(l1_idx) > 0) {
    # Create a working copy where only L1 gaps are NA
    work_vals <- new_vals
    # Temporarily fill L2/L3 with a sentinel so imputeTS skips them
    l2l3_idx <- which(new_src %in% c("gap_L2", "gap_L3"))
    if (length(l2l3_idx) > 0) {
      # Use last known value as placeholder (will be overwritten)
      work_vals[l2l3_idx] <- NA_real_
    }

    filled <- tryCatch(
      imputeTS::na_interpolation(work_vals, option = "linear"),
      error = function(e) zoo::na.approx(work_vals, na.rm = FALSE)
    )

    for (idx in l1_idx) {
      if (!is.na(filled[idx])) {
        new_vals[idx] <- filled[idx]
        new_src[idx] <- "interpolated"
      } else {
        new_src[idx] <- "excluded"
      }
    }
  }

  # Pass 3: Fill L2 gaps (seasonal decomposition via imputeTS)
  l2_idx <- which(new_src == "gap_L2")
  if (length(l2_idx) > 0) {
    freq <- .infer_frequency(time_step)

    filled_l2 <- tryCatch({
      if (!is.null(freq) && freq >= 2 && length(new_vals) >= freq * 2) {
        ts_obj <- stats::ts(new_vals, frequency = freq)
        as.numeric(imputeTS::na_seadec(ts_obj))
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(filled_l2)) {
      for (idx in l2_idx) {
        if (!is.na(filled_l2[idx])) {
          new_vals[idx] <- filled_l2[idx]
          new_src[idx] <- "profiled"
        } else {
          new_src[idx] <- "excluded"
        }
      }
    } else {
      # Fallback: periodic profile (same hour + day-type ±2 weeks)
      hours_exp <- as.integer(format(new_ts, "%H"))
      is_weekend_exp <- format(new_ts, "%u") %in% c("6", "7")
      measured_idx <- which(new_src == "measured" & !is.na(new_vals))

      for (idx in l2_idx) {
        target_hour <- hours_exp[idx]
        target_we <- is_weekend_exp[idx]
        ts_num <- as.numeric(new_ts[idx])
        window_secs <- 14 * 86400

        in_window <- measured_idx[
          abs(as.numeric(new_ts[measured_idx]) - ts_num) <= window_secs]
        matches <- in_window[
          hours_exp[in_window] == target_hour &
          is_weekend_exp[in_window] == target_we]

        if (length(matches) < 3) {
          matches <- in_window[hours_exp[in_window] == target_hour]
        }
        if (length(matches) < 3) matches <- in_window

        if (length(matches) > 0) {
          new_vals[idx] <- stats::median(new_vals[matches], na.rm = TRUE)
          new_src[idx] <- "profiled"
        } else {
          new_src[idx] <- "excluded"
        }
      }
    }
  }

  # Pass 4: L3 gaps stay NA
  l3_idx <- which(new_src == "gap_L3")
  if (length(l3_idx) > 0) {
    new_src[l3_idx] <- "excluded"
  }

  list(timestamps = new_ts, values = new_vals, sources = new_src)
}

# ============================================================================
# Stuck Sensor Cleaning (IEC 61724-2 §6.3)
# ============================================================================

#' Clean stuck sensor segments by interpolation
#' @noRd
clean_stuck <- function(values, sources, stuck) {
  if (nrow(stuck) == 0) return(list(values = values, sources = sources))

  new_vals <- values
  new_src <- sources

  for (i in seq_len(nrow(stuck))) {
    stuck_val <- stuck$value[i]
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
    redistributed <- if (profile_sum > 0) {
      dump_val * profile / profile_sum
    } else {
      rep(dump_val / length(target_range), length(target_range))
    }

    new_vals[target_range] <- redistributed
    new_src[target_range] <- "redistributed"
  }
  list(values = new_vals, sources = new_src)
}

# ============================================================================
# Outlier Cleaning (ASHRAE Guideline 14 §5.3.2)
# ============================================================================

#' Clean outliers using forecast::tsclean or neighbor interpolation
#'
#' Tries forecast::tsclean first (STL-aware replacement values).
#' Falls back to neighbor interpolation if tsclean fails.
#'
#' @param values Numeric vector.
#' @param sources Character vector of data_source labels.
#' @param outliers data.table from detect_outliers_iqr.
#' @param timestamps POSIXct vector.
#' @param time_step difftime for frequency detection.
#' @return List with values, sources.
#' @noRd
clean_outliers <- function(values, sources, outliers, timestamps,
                           time_step = NULL) {
  if (nrow(outliers) == 0) return(list(values = values, sources = sources))

  new_vals <- values
  new_src <- sources
  outlier_times <- outliers$timestamp

  # Try forecast::tsclean for replacement values
  tsclean_vals <- tryCatch({
    freq <- .infer_frequency(time_step)
    if (!is.null(freq) && freq >= 2 && length(values) >= freq * 2) {
      ts_obj <- stats::ts(values, frequency = freq)
      as.numeric(forecast::tsclean(ts_obj))
    } else {
      NULL
    }
  }, error = function(e) NULL)

  for (ot in outlier_times) {
    idx <- which(timestamps == ot)
    if (length(idx) == 0) next
    idx <- idx[1]
    if (new_src[idx] != "measured") next

    if (!is.null(tsclean_vals) && !is.na(tsclean_vals[idx])) {
      # Use STL-aware replacement
      new_vals[idx] <- tsclean_vals[idx]
    } else {
      # Fallback: neighbor interpolation
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
    }
    new_src[idx] <- "outlier_replaced"
  }

  list(values = new_vals, sources = new_src)
}
