# ── Upload Module ─────────────────────────────────────────────────────────────

#' Upload Module UI
#' @param id Module namespace ID.
#' @noRd
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Upload CSV Timeseries"),
    shiny::fileInput(ns("file"), "Choose CSV File", accept = ".csv"),
    shiny::uiOutput(ns("error_msg")),
    shiny::uiOutput(ns("detection_summary")),
    shiny::uiOutput(ns("column_mapping")),
    DT::dataTableOutput(ns("preview"))
  )
}

#' Upload Module Server
#' @param id Module namespace ID.
#' @param app_state Shared reactiveValues.
#' @noRd
mod_upload_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local reactive for parsed data before confirmation
    parsed <- shiny::reactiveValues(
      dt = NULL,
      ts_col = NULL,
      num_cols = NULL,
      time_step = NULL,
      file_name = NULL
    )

    # Handle file upload
    shiny::observeEvent(input$file, {
      req(input$file)

      # Clear previous state
      output$error_msg <- shiny::renderUI(NULL)
      parsed$dt <- NULL

      path <- input$file$datapath
      file_size <- file.size(path)

      # Large file warning
      if (file_size > 100 * 1024 * 1024) {
        output$error_msg <- shiny::renderUI(
          shiny::tags$div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            " Large file detected (>100MB). Loading may take longer than usual."
          )
        )
      }

      # Parse CSV
      tryCatch({
        dt <- parse_csv(path)

        # Check for empty file
        if (nrow(dt) == 0) {
          output$error_msg <- shiny::renderUI(
            shiny::tags$div(
              class = "alert alert-danger",
              "The file contains no data rows. Please upload a CSV with at least one data row."
            )
          )
          return()
        }

        # Detect timestamp column
        ts_col <- detect_timestamp_col(dt)
        if (is.null(ts_col)) {
          output$error_msg <- shiny::renderUI(
            shiny::tags$div(
              class = "alert alert-danger",
              "Could not detect a timestamp column. Supported formats: ",
              "ISO 8601 (2024-01-01T00:00:00), European (01/01/2024 00:00), ",
              "US (01-01-2024 00:00). Please select the timestamp column manually."
            )
          )
          # Still allow manual selection — show all columns
          ts_col <- names(dt)[1]
        }

        # Detect numeric columns
        num_cols <- detect_numeric_cols(dt, ts_col)
        if (length(num_cols) == 0) {
          output$error_msg <- shiny::renderUI(
            shiny::tags$div(
              class = "alert alert-danger",
              "No numeric columns found. The validator requires at least one numeric column."
            )
          )
          return()
        }

        # Parse timestamps if still character
        if (is.character(dt[[ts_col]])) {
          dt[[ts_col]] <- parse_timestamps(dt[[ts_col]])
        }

        # Infer time step
        timestamps <- sort(dt[[ts_col]])
        time_step <- infer_time_step(timestamps)

        # Store parsed results
        parsed$dt <- dt
        parsed$ts_col <- ts_col
        parsed$num_cols <- num_cols
        parsed$time_step <- time_step
        parsed$file_name <- input$file$name

      }, error = function(e) {
        output$error_msg <- shiny::renderUI(
          shiny::tags$div(
            class = "alert alert-danger",
            "Error reading CSV file: ", conditionMessage(e)
          )
        )
      })
    })

    # Detection summary
    output$detection_summary <- shiny::renderUI({
      req(parsed$dt)

      dt <- parsed$dt
      ts_col <- parsed$ts_col
      timestamps <- sort(dt[[ts_col]])
      step_str <- format_time_step(parsed$time_step)

      shiny::tags$div(
        class = "card p-3 mb-3",
        shiny::h5("Detection Summary"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("File: "), parsed$file_name),
          shiny::tags$li(shiny::strong("Rows: "), format(nrow(dt), big.mark = ",")),
          shiny::tags$li(shiny::strong("Columns: "), ncol(dt)),
          shiny::tags$li(shiny::strong("Timestamp column: "), ts_col),
          shiny::tags$li(shiny::strong("Date range: "),
            format(min(timestamps, na.rm = TRUE)), " to ",
            format(max(timestamps, na.rm = TRUE))),
          shiny::tags$li(shiny::strong("Detected time step: "), step_str),
          shiny::tags$li(shiny::strong("Numeric columns: "),
            paste(parsed$num_cols, collapse = ", "))
        )
      )
    })

    # Column mapping UI
    output$column_mapping <- shiny::renderUI({
      req(parsed$dt)

      shiny::tagList(
        shiny::selectInput(ns("ts_col_select"), "Timestamp Column",
          choices = names(parsed$dt), selected = parsed$ts_col),
        shiny::checkboxGroupInput(ns("value_cols"), "Columns to Validate",
          choices = parsed$num_cols, selected = parsed$num_cols),
        shiny::actionButton(ns("confirm"), "Confirm & Proceed",
          class = "btn-primary")
      )
    })

    # Data preview
    output$preview <- DT::renderDataTable({
      req(parsed$dt)
      DT::datatable(
        head(parsed$dt, 100),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    # Confirm button: store dataset in app_state
    shiny::observeEvent(input$confirm, {
      req(parsed$dt, input$ts_col_select, input$value_cols)

      dt <- data.table::copy(parsed$dt)
      ts_col <- input$ts_col_select
      value_cols <- input$value_cols

      # Re-parse timestamps if user changed column
      if (ts_col != parsed$ts_col && is.character(dt[[ts_col]])) {
        dt[[ts_col]] <- parse_timestamps(dt[[ts_col]])
      }

      # Sort by timestamp
      data.table::setorderv(dt, ts_col)

      timestamps <- dt[[ts_col]]
      time_step <- infer_time_step(timestamps)

      # Initialize data_source (all "measured")
      ds <- data.table::data.table(
        matrix("measured", nrow = nrow(dt), ncol = length(value_cols),
               dimnames = list(NULL, value_cols))
      )

      # Store in app_state
      app_state$dataset <- list(
        raw_data = data.table::copy(dt),
        timestamp_col = ts_col,
        value_cols = value_cols,
        timestamps = timestamps,
        time_step = time_step,
        file_name = parsed$file_name,
        date_range = list(start = min(timestamps, na.rm = TRUE),
                          end = max(timestamps, na.rm = TRUE)),
        record_count = nrow(dt)
      )
      app_state$working_data <- dt
      app_state$data_source <- ds
      app_state$diagnostics <- NULL
      app_state$undo_stack <- list()
      app_state$cleaning_log <- character(0)
      app_state$state <- "loaded"

      message("[upload] Dataset loaded: ", nrow(dt), " rows, ",
              length(value_cols), " columns, step=",
              format_time_step(time_step))
    })
  })
}

#' Format a difftime as human-readable string
#' @noRd
format_time_step <- function(step) {
  secs <- as.numeric(step, units = "secs")
  if (is.na(secs)) return("unknown")
  if (secs < 60) return(paste(secs, "seconds"))
  if (secs < 3600) return(paste(round(secs / 60), "minutes"))
  if (secs < 86400) return(paste(round(secs / 3600, 1), "hours"))
  paste(round(secs / 86400, 1), "days")
}
