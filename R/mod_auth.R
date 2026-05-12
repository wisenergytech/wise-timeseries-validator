# ── Supabase Auth Module ──────────────────────────────────────────────────────

auth_login <- function(email, password) {
  url <- Sys.getenv("SUPABASE_URL")
  key <- Sys.getenv("SUPABASE_KEY")
  if (url == "" || key == "") return(NULL)

  tryCatch({
    resp <- httr2::request(paste0(url, "/auth/v1/token?grant_type=password")) |>
      httr2::req_headers(apikey = key, `Content-Type` = "application/json") |>
      httr2::req_body_json(list(email = email, password = password)) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) == 200) {
      httr2::resp_body_json(resp)
    } else {
      NULL
    }
  }, error = function(e) {
    message("[auth] Login failed: ", conditionMessage(e))
    NULL
  })
}

auth_verify <- function(token) {
  url <- Sys.getenv("SUPABASE_URL")
  key <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")
  if (url == "" || key == "") return(NULL)

  tryCatch({
    resp <- httr2::request(paste0(url, "/auth/v1/user")) |>
      httr2::req_headers(
        apikey = key,
        Authorization = paste("Bearer", token)
      ) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) == 200) {
      httr2::resp_body_json(resp)
    } else {
      NULL
    }
  }, error = function(e) {
    message("[auth] Token verification failed: ", conditionMessage(e))
    NULL
  })
}

auth_is_configured <- function() {
  Sys.getenv("SUPABASE_URL") != "" && Sys.getenv("SUPABASE_KEY") != ""
}

# ── UI ───────────────────────────────────────────────────────────────────────

auth_login_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "max-width: 400px; margin: 100px auto; padding: 20px;",
    shiny::h2("Login", style = "color: #1D4345;"),
    shiny::textInput(ns("email"), "Email"),
    shiny::passwordInput(ns("password"), "Password"),
    shiny::actionButton(ns("login_btn"), "Login",
      style = "background-color: #1D4345; color: white; width: 100%;"),
    shiny::uiOutput(ns("error_msg"))
  )
}

# ── Server ───────────────────────────────────────────────────────────────────

auth_login_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$login_btn, {
      result <- auth_login(input$email, input$password)
      if (!is.null(result) && !is.null(result$access_token)) {
        parent_session$userData$access_token <- result$access_token
        parent_session$userData$user <- result$user
        parent_session$userData$authenticated <- TRUE
      } else {
        output$error_msg <- shiny::renderUI(
          shiny::tags$p("Invalid credentials", style = "color: red; margin-top: 10px;")
        )
      }
    })
  })
}
