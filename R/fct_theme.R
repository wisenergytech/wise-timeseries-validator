#' Wise bslib Theme
#'
#' Returns the Wise Bootstrap 5 theme for use in bslib page layouts.
#' @importFrom bslib bs_theme font_google
#' @noRd
wise_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#F6F7F8",
    fg = "#171616",
    primary = "#1D4345",
    secondary = "#E9A345",
    success = "#BCC9B9",
    warning = "#EFECEB",
    danger = "#FF0000",
    base_font = bslib::font_google("Raleway"),
    heading_font = bslib::font_google("Raleway"),
    "navbar-bg" = "#1D4345",
    "card-bg" = "#FFFFFF",
    "card-border-color" = "#E2E8F0"
  )
}
