#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Initialize auth state
  session$userData$authenticated <- !auth_is_configured()

  # Auth login module
  mod_auth_server("auth", parent_session = session)

  # Render either login or app content
  output$app_content <- renderUI({
    if (!isTRUE(session$userData$authenticated)) {
      mod_auth_ui("auth")
    } else {
      div(
        h3("Welcome to Wise App"),
        p("Authentication successful. Start building your app here.")
      )
    }
  })
}
