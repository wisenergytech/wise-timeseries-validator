# ── Visualization Module ──────────────────────────────────────────────────────
# Provides visualization helper functions used by mod_diagnostic.
# Visualizations are rendered directly in the diagnostic tab.

#' Visualization Module Server (placeholder for shared state observation)
#' @param id Module namespace ID.
#' @param app_state Shared reactiveValues.
#' @noRd
mod_viz_server <- function(id, app_state) {
  # Visualizations are rendered in mod_diagnostic_server.
  # This module exists for future extensibility if visualizations
  # need their own tab or independent state.
  invisible(NULL)
}

#' Data source color palette
#' @return Named character vector mapping data_source labels to colors.
#' @noRd
ds_color_palette <- function() {
  c(
    measured = "#1D4345",
    interpolated = "#E9A345",
    profiled = "#9B59B6",
    redistributed = "#2ECC71",
    reinterpolated = "#6BAED6",
    outlier_replaced = "#E74C3C",
    excluded = "#999999"
  )
}

#' Build a timeseries Plotly plot color-coded by data_source
#' @param timestamps POSIXct vector.
#' @param values Numeric vector.
#' @param sources Character vector of data_source labels.
#' @param col_name Column name for title.
#' @return A plotly object.
#' @noRd
build_ts_plot <- function(timestamps, values, sources, col_name) {
  colors <- ds_color_palette()
  p <- plotly::plot_ly()

  for (src in intersect(unique(sources), names(colors))) {
    idx <- which(sources == src)
    p <- p |> plotly::add_trace(
      x = timestamps[idx], y = values[idx],
      type = "scatter", mode = "markers",
      name = src,
      marker = list(color = colors[src], size = 4)
    )
  }

  p |> wise_layout(title = paste("Timeseries:", col_name))
}

#' Build a completeness bar chart
#' @param diagnostics List of QualityDiagnostic results.
#' @return A plotly object.
#' @noRd
build_completeness_bar <- function(diagnostics) {
  cols <- vapply(diagnostics, function(d) d$column_name, character(1))
  pcts <- vapply(diagnostics, function(d) d$completeness, numeric(1))

  plotly::plot_ly(
    x = cols, y = pcts,
    type = "bar",
    marker = list(color = "#1D4345")
  ) |>
    plotly::layout(
      yaxis = list(title = "Completeness (%)", range = c(0, 105)),
      xaxis = list(title = "")
    ) |>
    wise_layout(title = "Data Completeness")
}
