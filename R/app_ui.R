#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @importFrom bslib page_sidebar sidebar
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_sidebar(
      title = "Wise App",
      theme = wise_theme() |>
        bslib::bs_add_rules(sass::sass_file(app_sys("app/www/wise.scss"))),
      sidebar = sidebar(
        p("Sidebar content here")
      ),
      uiOutput("app_content")
    )
  )
}

#' Add external Resources to the Application
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Wise App"
    )
  )
}
