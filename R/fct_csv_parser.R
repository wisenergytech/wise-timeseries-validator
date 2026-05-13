# ── CSV Parser Functions ──────────────────────────────────────────────────────

#' Parse a CSV file with auto-detection
#'
#' Wraps data.table::fread() to auto-detect separator, decimal, and encoding.
#' @param path Path to the CSV file.
#' @return A data.table with parsed data.
#' @noRd
parse_csv <- function(path) {
  data.table::fread(
    path,
    header = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "N/A", "n/a", "null", "NULL", "error", "#N/A", "#VALUE!")
  )
}

#' Detect the timestamp column in a data.table
#'
#' Tries to parse each character column as datetime. Returns the name of the
#' first column that successfully parses.
#' @param dt A data.table.
#' @return Column name (character) or NULL if none found.
#' @noRd
detect_timestamp_col <- function(dt) {
  # First check for columns already parsed as POSIXct/Date by fread
  for (col in names(dt)) {
    if (inherits(dt[[col]], "POSIXct") || inherits(dt[[col]], "Date") ||
        inherits(dt[[col]], "IDate") || inherits(dt[[col]], "POSIXlt")) {
      return(col)
    }
  }

  # Then try to parse character columns
  char_cols <- names(dt)[vapply(dt, is.character, logical(1))]

  for (col in char_cols) {
    sample_vals <- head(stats::na.omit(dt[[col]]), 20)
    if (length(sample_vals) == 0) next

    parsed <- parse_timestamps(sample_vals)
    if (!all(is.na(parsed)) && sum(!is.na(parsed)) >= length(sample_vals) * 0.8) {
      return(col)
    }
  }

  NULL
}

#' Parse character timestamps into POSIXct
#'
#' Uses lubridate::parse_date_time with a priority list of common formats.
#' @param x Character vector of timestamp strings.
#' @return POSIXct vector.
#' @noRd
parse_timestamps <- function(x) {
  formats <- c(
    "ymd HMS", "ymd HM", "ymd",
    "dmy HMS", "dmy HM", "dmy",
    "mdy HMS", "mdy HM", "mdy",
    "ymd_HMS", "ymd_HM"
  )

  suppressWarnings(
    lubridate::parse_date_time(x, orders = formats, quiet = TRUE)
  )
}

#' Infer the dominant time step from timestamps
#'
#' Computes the mode of diff(timestamps).
#' @param timestamps Sorted POSIXct vector.
#' @return A difftime object representing the dominant interval.
#' @noRd
infer_time_step <- function(timestamps) {
  diffs <- diff(as.numeric(timestamps))
  if (length(diffs) == 0) return(as.difftime(NA_real_, units = "secs"))

  # Mode: most frequent diff value
  freq <- table(diffs)
  mode_val <- as.numeric(names(freq)[which.max(freq)])

  as.difftime(mode_val, units = "secs")
}

#' Detect numeric columns (excluding timestamp)
#'
#' @param dt A data.table.
#' @param ts_col Name of the timestamp column to exclude.
#' @return Character vector of numeric column names.
#' @noRd
detect_numeric_cols <- function(dt, ts_col) {
  cols <- names(dt)
  cols <- setdiff(cols, ts_col)

  is_num <- vapply(cols, function(col) {
    is.numeric(dt[[col]]) || is.integer(dt[[col]])
  }, logical(1))

  cols[is_num]
}
