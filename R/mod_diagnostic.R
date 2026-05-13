# ── Diagnostic Module ─────────────────────────────────────────────────────────

#' Diagnostic Module UI
#' @param id Module namespace ID.
#' @noRd
mod_diagnostic_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Quality Diagnostic"),
    shiny::actionButton(ns("run"), "Run Diagnostic", class = "btn-primary mb-3"),
    shiny::uiOutput(ns("status_msg")),
    DT::dataTableOutput(ns("summary_table")),
    shiny::hr(),
    shiny::h4("Detail Tables"),
    shiny::uiOutput(ns("detail_selector")),
    shiny::h5("Gaps"),
    DT::dataTableOutput(ns("gaps_table")),
    shiny::h5("Outliers"),
    DT::dataTableOutput(ns("outliers_table")),
    shiny::h5("Stuck Segments"),
    DT::dataTableOutput(ns("stuck_table")),
    shiny::hr(),
    shiny::h4("Visualizations"),
    plotly::plotlyOutput(ns("ts_plot"), height = "400px"),
    plotly::plotlyOutput(ns("completeness_bar"), height = "250px"),
    shiny::h5("Data Source Summary"),
    DT::dataTableOutput(ns("source_summary"))
  )
}

#' Diagnostic Module Server
#' @param id Module namespace ID.
#' @param app_state Shared reactiveValues.
#' @noRd
mod_diagnostic_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Run diagnostic
    shiny::observeEvent(input$run, {
      req(app_state$working_data, app_state$dataset, app_state$config)

      dt <- app_state$working_data
      ts_col <- app_state$dataset$timestamp_col
      value_cols <- app_state$dataset$value_cols
      timestamps <- dt[[ts_col]]
      time_step <- app_state$dataset$time_step
      config <- app_state$config

      diagnostics <- lapply(value_cols, function(col) {
        run_diagnostic(col, dt[[col]], timestamps, time_step, config)
      })
      names(diagnostics) <- value_cols

      app_state$diagnostics <- diagnostics
      app_state$state <- "diagnosed"

      output$status_msg <- shiny::renderUI(
        shiny::tags$div(class = "alert alert-success", "Diagnostic complete.")
      )

      message("[diagnostic] Diagnostic complete for ", length(value_cols), " columns")
    })

    # Summary table
    output$summary_table <- DT::renderDataTable({
      req(app_state$diagnostics)

      rows <- lapply(app_state$diagnostics, function(d) {
        data.frame(
          Column = d$column_name,
          Records = app_state$dataset$record_count,
          `Completeness %` = d$completeness,
          Gaps = d$gap_count,
          Outliers = d$outlier_count,
          Stuck = d$stuck_count,
          Min = round(d$stats$min, 2),
          Max = round(d$stats$max, 2),
          Mean = round(d$stats$mean, 2),
          check.names = FALSE
        )
      })

      DT::datatable(
        do.call(rbind, rows),
        options = list(dom = "t", pageLength = 50),
        rownames = FALSE
      )
    })

    # Detail column selector
    output$detail_selector <- shiny::renderUI({
      req(app_state$diagnostics)
      cols <- names(app_state$diagnostics)
      shiny::selectInput(ns("detail_col"), "Show details for column:", choices = cols)
    })

    # Gap details
    output$gaps_table <- DT::renderDataTable({
      req(app_state$diagnostics, input$detail_col)
      d <- app_state$diagnostics[[input$detail_col]]
      if (nrow(d$gaps) == 0) return(DT::datatable(data.frame(Message = "No gaps detected")))
      gaps <- d$gaps
      gaps$duration <- paste(round(gaps$duration, 1), "hours")
      gaps$start <- format(gaps$start, "%Y-%m-%d %H:%M")
      gaps$end <- format(gaps$end, "%Y-%m-%d %H:%M")
      DT::datatable(gaps, options = list(dom = "tp", pageLength = 10), rownames = FALSE)
    })

    # Outlier details
    output$outliers_table <- DT::renderDataTable({
      req(app_state$diagnostics, input$detail_col)
      d <- app_state$diagnostics[[input$detail_col]]
      if (nrow(d$outliers) == 0) return(DT::datatable(data.frame(Message = "No outliers detected")))
      out <- d$outliers
      out$timestamp <- format(out$timestamp, "%Y-%m-%d %H:%M")
      out$value <- round(out$value, 2)
      out$lower_fence <- round(out$lower_fence, 2)
      out$upper_fence <- round(out$upper_fence, 2)
      DT::datatable(out, options = list(dom = "tp", pageLength = 10), rownames = FALSE)
    })

    # Stuck segment details
    output$stuck_table <- DT::renderDataTable({
      req(app_state$diagnostics, input$detail_col)
      d <- app_state$diagnostics[[input$detail_col]]
      if (nrow(d$stuck_segments) == 0) return(DT::datatable(data.frame(Message = "No stuck segments detected")))
      s <- d$stuck_segments
      s$start <- format(s$start, "%Y-%m-%d %H:%M")
      s$end <- format(s$end, "%Y-%m-%d %H:%M")
      DT::datatable(s, options = list(dom = "tp", pageLength = 10), rownames = FALSE)
    })

    # Timeseries plot (color-coded by data_source)
    output$ts_plot <- plotly::renderPlotly({
      req(app_state$working_data, app_state$data_source, input$detail_col)

      dt <- app_state$working_data
      ds <- app_state$data_source
      ts_col <- app_state$dataset$timestamp_col
      col <- input$detail_col

      timestamps <- dt[[ts_col]]
      values <- dt[[col]]
      sources <- ds[[col]]

      # Data source color mapping
      ds_colors <- c(
        measured = "#1D4345",
        interpolated = "#E9A345",
        profiled = "#9B59B6",
        redistributed = "#2ECC71",
        reinterpolated = "#6BAED6",
        outlier_replaced = "#E74C3C",
        excluded = "#999999"
      )

      p <- plotly::plot_ly()

      for (src in intersect(unique(sources), names(ds_colors))) {
        idx <- which(sources == src)
        p <- p |> plotly::add_trace(
          x = timestamps[idx], y = values[idx],
          type = "scatter", mode = "markers",
          name = src,
          marker = list(color = ds_colors[src], size = 4)
        )
      }

      p |> wise_layout(title = paste("Timeseries:", col))
    })

    # Completeness bar
    output$completeness_bar <- plotly::renderPlotly({
      req(app_state$diagnostics)

      cols <- names(app_state$diagnostics)
      completeness <- vapply(app_state$diagnostics, function(d) d$completeness, numeric(1))

      p <- plotly::plot_ly(
        x = cols, y = completeness,
        type = "bar",
        marker = list(color = "#1D4345")
      ) |>
        plotly::layout(
          yaxis = list(title = "Completeness (%)", range = c(0, 105)),
          xaxis = list(title = "")
        ) |>
        wise_layout(title = "Data Completeness")

      p
    })

    # Data source summary
    output$source_summary <- DT::renderDataTable({
      req(app_state$data_source)

      ds <- app_state$data_source
      total <- nrow(ds)

      rows <- lapply(names(ds), function(col) {
        tbl <- table(ds[[col]])
        labels <- c("measured", "interpolated", "profiled", "redistributed", "reinterpolated", "outlier_replaced", "excluded")
        counts <- vapply(labels, function(l) {
          if (l %in% names(tbl)) as.integer(tbl[l]) else 0L
        }, integer(1))
        pcts <- round(counts / total * 100, 1)

        data.frame(
          Column = col,
          Measured = paste0(counts[1], " (", pcts[1], "%)"),
          Interpolated = paste0(counts[2], " (", pcts[2], "%)"),
          Profiled = paste0(counts[3], " (", pcts[3], "%)"),
          Redistributed = paste0(counts[4], " (", pcts[4], "%)"),
          Reinterpolated = paste0(counts[5], " (", pcts[5], "%)"),
          Outlier_Replaced = paste0(counts[6], " (", pcts[6], "%)"),
          Excluded = paste0(counts[7], " (", pcts[7], "%)"),
          check.names = FALSE
        )
      })

      DT::datatable(
        do.call(rbind, rows),
        options = list(dom = "t", pageLength = 50),
        rownames = FALSE
      )
    })
  })
}
