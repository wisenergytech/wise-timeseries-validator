# ── Cleaning & Export Module ──────────────────────────────────────────────────

#' Cleaning Module UI
#' @param id Module namespace ID.
#' @noRd
mod_cleaning_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Cleaning & Export"),
    shiny::fluidRow(
      shiny::column(3, shiny::actionButton(ns("fill_gaps"), "Fill Gaps", class = "btn-outline-primary w-100")),
      shiny::column(3, shiny::actionButton(ns("clean_stuck"), "Clean Stuck", class = "btn-outline-primary w-100")),
      shiny::column(3, shiny::actionButton(ns("clean_outliers"), "Clean Outliers", class = "btn-outline-primary w-100")),
      shiny::column(3, shiny::actionButton(ns("clean_all"), "Clean All", class = "btn-primary w-100"))
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(3, shiny::actionButton(ns("undo"), "Undo", class = "btn-warning w-100")),
      shiny::column(3, shiny::downloadButton(ns("export"), "Export CSV", class = "btn-success w-100"))
    ),
    shiny::hr(),
    shiny::h4("Cleaning Log"),
    shiny::verbatimTextOutput(ns("log")),
    shiny::hr(),
    shiny::h4("Current State"),
    DT::dataTableOutput(ns("source_summary"))
  )
}

#' Cleaning Module Server
#' @param id Module namespace ID.
#' @param app_state Shared reactiveValues.
#' @noRd
mod_cleaning_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Helper: push current state to undo stack
    push_undo <- function(operation) {
      snapshot <- list(
        data_snapshot = data.table::copy(app_state$working_data),
        source_snapshot = data.table::copy(app_state$data_source),
        operation = operation,
        timestamp = Sys.time()
      )
      app_state$undo_stack <- c(app_state$undo_stack, list(snapshot))
    }

    # Helper: refresh diagnostic after cleaning
    refresh_diagnostic <- function() {
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
    }

    # Helper: log a cleaning operation
    log_operation <- function(op_name, affected_count) {
      entry <- paste0(
        format(Sys.time(), "%H:%M:%S"), " | ", op_name,
        " | ", affected_count, " values affected"
      )
      app_state$cleaning_log <- c(app_state$cleaning_log, entry)
    }

    # ── Fill Gaps ──────────────────────────────────────────────────────────────
    shiny::observeEvent(input$fill_gaps, {
      req(app_state$working_data, app_state$dataset, app_state$data_source)
      push_undo("fill_gaps")

      dt <- app_state$working_data
      ds <- app_state$data_source
      ts_col <- app_state$dataset$timestamp_col
      value_cols <- app_state$dataset$value_cols
      time_step <- app_state$dataset$time_step
      timestamps <- dt[[ts_col]]
      total_affected <- 0

      # Fill gaps for the first column to get new timestamps
      result <- fill_gaps(timestamps, dt[[value_cols[1]]], ds[[value_cols[1]]], time_step, app_state$config)
      new_timestamps <- result$timestamps

      # Build new data.table with expanded timestamps
      new_dt <- data.table::data.table(dummy = seq_along(new_timestamps))
      new_dt[[ts_col]] <- new_timestamps
      new_dt[["dummy"]] <- NULL

      new_ds <- data.table::data.table(dummy = seq_along(new_timestamps))
      new_ds[["dummy"]] <- NULL

      # First column already done
      new_dt[[value_cols[1]]] <- result$values
      new_ds[[value_cols[1]]] <- result$sources
      total_affected <- total_affected + sum(result$sources %in% c("interpolated", "excluded"))

      # Fill remaining columns
      if (length(value_cols) > 1) {
        for (col in value_cols[-1]) {
          res <- fill_gaps(timestamps, dt[[col]], ds[[col]], time_step, app_state$config)
          new_dt[[col]] <- res$values
          new_ds[[col]] <- res$sources
          total_affected <- total_affected + sum(res$sources %in% c("interpolated", "excluded"))
        }
      }

      # Preserve non-validated columns
      other_cols <- setdiff(names(dt), c(ts_col, value_cols))
      for (col in other_cols) {
        # Expand with NA for inserted rows
        old_vals <- dt[[col]]
        expanded <- rep(NA, nrow(new_dt))
        # Map old positions to new positions
        old_ts_num <- as.numeric(timestamps)
        new_ts_num <- as.numeric(new_timestamps)
        for (i in seq_along(old_ts_num)) {
          match_idx <- which(new_ts_num == old_ts_num[i])
          if (length(match_idx) > 0) expanded[match_idx[1]] <- old_vals[i]
        }
        new_dt[[col]] <- expanded
      }

      app_state$working_data <- new_dt
      app_state$data_source <- new_ds
      app_state$state <- "partially_cleaned"
      log_operation("Fill Gaps", total_affected)
      refresh_diagnostic()

      message("[cleaning] Fill gaps: ", total_affected, " values affected")
    })

    # ── Clean Stuck ────────────────────────────────────────────────────────────
    shiny::observeEvent(input$clean_stuck, {
      req(app_state$working_data, app_state$dataset, app_state$data_source, app_state$config)
      push_undo("clean_stuck")

      dt <- app_state$working_data
      ds <- app_state$data_source
      ts_col <- app_state$dataset$timestamp_col
      value_cols <- app_state$dataset$value_cols
      timestamps <- dt[[ts_col]]
      total_affected <- 0

      for (col in value_cols) {
        stuck <- detect_stuck_rle(dt[[col]], timestamps,
                                  min_run = app_state$config$rle_min_run)
        if (nrow(stuck) > 0) {
          # First: redistribute stuck-dump patterns
          if (any(stuck$has_dump)) {
            result <- clean_stuck_dump(dt[[col]], timestamps,
                                       ds[[col]], stuck)
            data.table::set(dt, j = col, value = result$values)
            data.table::set(ds, j = col, value = result$sources)
          }
          # Then: interpolate remaining stuck segments (no dump)
          stuck_no_dump <- stuck[!stuck$has_dump, ]
          if (nrow(stuck_no_dump) > 0) {
            result <- clean_stuck(dt[[col]], ds[[col]], stuck_no_dump)
            data.table::set(dt, j = col, value = result$values)
            data.table::set(ds, j = col, value = result$sources)
          }
          affected <- sum(ds[[col]] %in% c("reinterpolated", "redistributed"))
          total_affected <- total_affected + affected
        }
      }

      app_state$working_data <- dt
      app_state$data_source <- ds
      app_state$state <- "partially_cleaned"
      log_operation("Clean Stuck", total_affected)
      refresh_diagnostic()

      message("[cleaning] Clean stuck: ", total_affected, " values affected")
    })

    # ── Clean Outliers ─────────────────────────────────────────────────────────
    shiny::observeEvent(input$clean_outliers, {
      req(app_state$working_data, app_state$dataset, app_state$data_source, app_state$config)
      push_undo("clean_outliers")

      dt <- app_state$working_data
      ds <- app_state$data_source
      ts_col <- app_state$dataset$timestamp_col
      value_cols <- app_state$dataset$value_cols
      timestamps <- dt[[ts_col]]
      total_affected <- 0

      for (col in value_cols) {
        outliers <- detect_outliers_iqr(dt[[col]], timestamps,
                                        k = app_state$config$iqr_k)
        if (nrow(outliers) > 0) {
          result <- clean_outliers(dt[[col]], ds[[col]], outliers, timestamps)
          affected <- sum(result$sources == "outlier_replaced" & ds[[col]] != "outlier_replaced")
          total_affected <- total_affected + affected
          data.table::set(dt, j = col, value = result$values)
          data.table::set(ds, j = col, value = result$sources)
        }
      }

      app_state$working_data <- dt
      app_state$data_source <- ds
      app_state$state <- "partially_cleaned"
      log_operation("Clean Outliers", total_affected)
      refresh_diagnostic()

      message("[cleaning] Clean outliers: ", total_affected, " values affected")
    })

    # ── Clean All ──────────────────────────────────────────────────────────────
    shiny::observeEvent(input$clean_all, {
      req(app_state$working_data, app_state$dataset, app_state$data_source, app_state$config)
      push_undo("clean_all")

      # Step 1: Fill gaps
      dt <- app_state$working_data
      ds <- app_state$data_source
      ts_col <- app_state$dataset$timestamp_col
      value_cols <- app_state$dataset$value_cols
      time_step <- app_state$dataset$time_step
      timestamps <- dt[[ts_col]]

      # Fill gaps
      result <- fill_gaps(timestamps, dt[[value_cols[1]]], ds[[value_cols[1]]], time_step, app_state$config)
      new_timestamps <- result$timestamps
      new_dt <- data.table::data.table(dummy = seq_along(new_timestamps))
      new_dt[[ts_col]] <- new_timestamps
      new_dt[["dummy"]] <- NULL
      new_ds <- data.table::data.table(dummy = seq_along(new_timestamps))
      new_ds[["dummy"]] <- NULL

      new_dt[[value_cols[1]]] <- result$values
      new_ds[[value_cols[1]]] <- result$sources

      if (length(value_cols) > 1) {
        for (col in value_cols[-1]) {
          res <- fill_gaps(timestamps, dt[[col]], ds[[col]], time_step, app_state$config)
          new_dt[[col]] <- res$values
          new_ds[[col]] <- res$sources
        }
      }

      # Preserve other columns
      other_cols <- setdiff(names(dt), c(ts_col, value_cols))
      for (col in other_cols) {
        old_vals <- dt[[col]]
        expanded <- rep(NA, nrow(new_dt))
        old_ts_num <- as.numeric(timestamps)
        new_ts_num <- as.numeric(new_timestamps)
        for (i in seq_along(old_ts_num)) {
          match_idx <- which(new_ts_num == old_ts_num[i])
          if (length(match_idx) > 0) expanded[match_idx[1]] <- old_vals[i]
        }
        new_dt[[col]] <- expanded
      }

      timestamps <- new_timestamps

      # Step 2: Clean stuck sensors (dump redistribution + interpolation)
      for (col in value_cols) {
        stuck <- detect_stuck_rle(new_dt[[col]], timestamps,
                                  min_run = app_state$config$rle_min_run)
        if (nrow(stuck) > 0) {
          if (any(stuck$has_dump)) {
            result <- clean_stuck_dump(new_dt[[col]], timestamps,
                                       new_ds[[col]], stuck)
            data.table::set(new_dt, j = col, value = result$values)
            data.table::set(new_ds, j = col, value = result$sources)
          }
          stuck_no_dump <- stuck[!stuck$has_dump, ]
          if (nrow(stuck_no_dump) > 0) {
            result <- clean_stuck(new_dt[[col]], new_ds[[col]],
                                  stuck_no_dump)
            data.table::set(new_dt, j = col, value = result$values)
            data.table::set(new_ds, j = col, value = result$sources)
          }
        }
      }

      # Step 3: Clean outliers
      for (col in value_cols) {
        outliers <- detect_outliers_iqr(new_dt[[col]], timestamps,
                                        k = app_state$config$iqr_k)
        if (nrow(outliers) > 0) {
          result <- clean_outliers(new_dt[[col]], new_ds[[col]], outliers, timestamps)
          data.table::set(new_dt, j = col, value = result$values)
          data.table::set(new_ds, j = col, value = result$sources)
        }
      }

      app_state$working_data <- new_dt
      app_state$data_source <- new_ds
      app_state$state <- "partially_cleaned"

      total_affected <- sum(vapply(value_cols, function(col) {
        sum(new_ds[[col]] != "measured")
      }, integer(1)))
      log_operation("Clean All (gaps+stuck+outliers)", total_affected)
      refresh_diagnostic()

      message("[cleaning] Clean all: ", total_affected, " total values affected")
    })

    # ── Undo ───────────────────────────────────────────────────────────────────
    shiny::observeEvent(input$undo, {
      stack <- app_state$undo_stack
      if (length(stack) == 0) {
        shiny::showNotification("Nothing to undo.", type = "warning")
        return()
      }

      last <- stack[[length(stack)]]
      app_state$working_data <- last$data_snapshot
      app_state$data_source <- last$source_snapshot
      app_state$undo_stack <- stack[-length(stack)]

      log_operation(paste("Undo:", last$operation), 0)
      refresh_diagnostic()

      if (length(app_state$undo_stack) == 0) {
        app_state$state <- "diagnosed"
      }

      message("[cleaning] Undo: ", last$operation)
    })

    # ── Export (T024 — US5) ──────────────────────────────────────────────────
    output$export <- shiny::downloadHandler(
      filename = function() {
        base <- tools::file_path_sans_ext(app_state$dataset$file_name)
        paste0(base, "_cleaned.csv")
      },
      content = function(file) {
        dt <- data.table::copy(app_state$working_data)
        ds <- app_state$data_source
        ts_col <- app_state$dataset$timestamp_col
        value_cols <- app_state$dataset$value_cols

        # Add data_source columns
        for (col in value_cols) {
          ds_col_name <- paste0(col, "_data_source")
          dt[[ds_col_name]] <- ds[[col]]
        }

        data.table::fwrite(dt, file)
        message("[export] Exported ", nrow(dt), " rows to ", file)
      }
    )

    # ── Cleaning Log ──────────────────────────────────────────────────────────
    output$log <- shiny::renderText({
      if (length(app_state$cleaning_log) == 0) return("No cleaning operations performed yet.")
      paste(app_state$cleaning_log, collapse = "\n")
    })

    # ── Data Source Summary ───────────────────────────────────────────────────
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
