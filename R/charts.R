# ── Wise Plotly Theme + Color Palette ────────────────────────────────────────

# Wise color palette
wise_colors <- list(
  primary   = "#1D4345",
  secondary = "#E9A345",
  success   = "#BCC9B9",
  text      = "#171616",
  muted     = "#475569",
  border    = "#E2E8F0",
  bg        = "#F6F7F8",
  surface   = "#FFFFFF",
  error     = "#FF0000"
)

# Color vector for Plotly traces
wise_colors_vec <- c(wise_colors$primary, wise_colors$secondary, wise_colors$success)

# Wise fonts for Plotly
wise_font <- list(family = "Raleway, sans-serif", color = wise_colors$text)
wise_title_font <- list(family = "Raleway, sans-serif", color = wise_colors$primary, size = 16)

#' Apply Wise layout defaults to a plotly object
#' @param p A plotly object
#' @param title Optional chart title
#' @return The plotly object with Wise styling applied
wise_layout <- function(p, title = NULL) {
  p |> plotly::layout(
    title = if (!is.null(title)) list(text = title, font = wise_title_font),
    font = wise_font,
    plot_bgcolor = wise_colors$bg,
    paper_bgcolor = wise_colors$bg,
    xaxis = list(gridcolor = wise_colors$border),
    yaxis = list(gridcolor = wise_colors$border),
    legend = list(orientation = "h", y = 1.1)
  )
}
