# ── Cleaning Functions ────────────────────────────────────────────────────────

#' Fill gaps by inserting rows and interpolating
#'
#' Inserts missing timestamps and fills values using linear interpolation.
#' Boundary gaps (no neighbor on one side) are flagged as "excluded".
#' @param timestamps Sorted POSIXct vector.
#' @param values Numeric vector.
#' @param sources Character vector of data_source labels.
#' @param time_step difftime expected interval.
#' @return List with timestamps, values, sources (all expanded).
#' @noRd
fill_gaps <- function(timestamps, values, sources, time_step) {
  step_secs <- as.numeric(time_step, units = "secs")
  n <- length(timestamps)
  if (n < 2) return(list(timestamps = timestamps, values = values, sources = sources))

  new_ts <- timestamps
  new_vals <- values
  new_src <- sources

  # Find gaps and insert missing timestamps
  i <- 1
  while (i < length(new_ts)) {
    gap_secs <- as.numeric(difftime(new_ts[i + 1], new_ts[i], units = "secs"))
    if (gap_secs > step_secs * 1.5) {
      # Number of missing points
      n_missing <- round(gap_secs / step_secs) - 1
      if (n_missing > 0) {
        insert_ts <- new_ts[i] + step_secs * (1:n_missing)
        insert_vals <- rep(NA_real_, n_missing)
        insert_src <- rep("gap_placeholder", n_missing)

        # Insert at position i+1
        new_ts <- c(new_ts[1:i], insert_ts, new_ts[(i + 1):length(new_ts)])
        new_vals <- c(new_vals[1:i], insert_vals, new_vals[(i + 1):length(new_vals)])
        new_src <- c(new_src[1:i], insert_src, new_src[(i + 1):length(new_src)])

        i <- i + n_missing + 1
        next
      }
    }
    i <- i + 1
  }

  # Interpolate gap placeholders
  placeholder_idx <- which(new_src == "gap_placeholder")
  if (length(placeholder_idx) > 0) {
    # Use zoo::na.approx for interpolation
    interpolated <- zoo::na.approx(new_vals, na.rm = FALSE)

    for (idx in placeholder_idx) {
      if (!is.na(interpolated[idx])) {
        new_vals[idx] <- interpolated[idx]
        new_src[idx] <- "interpolated"
      } else {
        # Boundary gap — cannot interpolate (no neighbor on one side)
        new_src[idx] <- "excluded"
      }
    }
  }

  list(timestamps = new_ts, values = new_vals, sources = new_src)
}

#' Clean stuck sensor segments by interpolation
#'
#' Replaces stuck values with linear interpolation. Skips already-cleaned values.
#' @param values Numeric vector.
#' @param sources Character vector of data_source labels.
#' @param stuck data.table from detect_stuck_rle with start/end/value/run_length.
#' @return List with values, sources.
#' @noRd
clean_stuck <- function(values, sources, stuck) {
  if (nrow(stuck) == 0) return(list(values = values, sources = sources))

  new_vals <- values
  new_src <- sources

  for (i in seq_len(nrow(stuck))) {
    stuck_val <- stuck$value[i]
    stuck_start <- stuck$start[i]
    stuck_end <- stuck$end[i]

    # Find indices of the stuck segment by matching value runs
    r <- rle(values)
    ends_pos <- cumsum(r$lengths)
    starts_pos <- ends_pos - r$lengths + 1

    for (j in seq_along(r$values)) {
      if (!is.na(r$values[j]) && r$values[j] == stuck_val &&
          r$lengths[j] >= stuck$run_length[i]) {
        seg_start <- starts_pos[j]
        seg_end <- ends_pos[j]

        # Get boundary values for interpolation
        left_val <- if (seg_start > 1) new_vals[seg_start - 1] else NA
        right_val <- if (seg_end < length(new_vals)) new_vals[seg_end + 1] else NA

        for (k in seg_start:seg_end) {
          # Flag-once: skip already-cleaned values
          if (new_src[k] != "measured") next

          if (!is.na(left_val) && !is.na(right_val)) {
            # Linear interpolation
            frac <- (k - seg_start + 1) / (seg_end - seg_start + 2)
            new_vals[k] <- left_val + frac * (right_val - left_val)
            new_src[k] <- "reinterpolated"
          } else if (!is.na(left_val) || !is.na(right_val)) {
            # One-sided: use the available neighbor
            new_vals[k] <- if (!is.na(left_val)) left_val else right_val
            new_src[k] <- "reinterpolated"
          }
          # If both boundaries are NA, leave value unchanged
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
#' surrounding data.
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

  # Pre-compute hours for profile matching
  hours <- as.integer(format(timestamps, "%H"))

  for (i in seq_len(nrow(dumps))) {
    # Find positional indices from timestamps
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

    # Scale profile to match dump total
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

#' Clean outliers by interpolation
#'
#' Replaces outlier values with linear interpolation between nearest non-outlier
#' neighbors. Skips already-cleaned values (flag-once).
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
