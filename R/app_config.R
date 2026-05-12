#' Access files in the current app
#'
#' @param ... Character vectors, specifying subdirectory and file(s)
#'   within your package. The default, none, returns the root of the app.
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "wiseapp")
}

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config R_CONFIG_ACTIVE value.
#' @param use_parent Logical, scan parent directory for config.
#' @noRd
get_golem_config <- function(
    value,
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    use_parent = TRUE) {
  config::get(
    value = value,
    config = config,
    file = app_sys("golem-config.yml"),
    use_parent = use_parent
  )
}
