# Shiny Charts (Plotly) — Conventions Wise

## Package

- Primary: `plotly` (native R API via `plot_ly()` + `renderPlotly()`)
- Do NOT use `ggplot2` + `ggplotly()` — use Plotly's native R interface directly.

## Usage Pattern

```r
library(plotly)

output$metrics_chart <- renderPlotly({
  plot_ly(df, x = ~timestamp, y = ~value, color = ~metric,
          type = "scatter", mode = "lines",
          colors = wise_colors_vec) |>
    layout(
      title = list(text = "Metrics Over Time", font = wise_title_font),
      xaxis = list(title = "Time"),
      yaxis = list(title = "Value"),
      font = wise_font,
      plot_bgcolor = "#F6F7F8",
      paper_bgcolor = "#F6F7F8",
      legend = list(orientation = "h", y = 1.1)
    )
})
```

## Wise Plotly Theme

Define Wise layout defaults in `R/charts.R`:

```r
# Wise color vector for Plotly traces
wise_colors_vec <- c("#1D4345", "#E9A345", "#BCC9B9")

# Wise fonts for Plotly
wise_font <- list(family = "Raleway, sans-serif", color = "#171616")
wise_title_font <- list(family = "Raleway, sans-serif", color = "#1D4345", size = 16)

# Apply Wise layout defaults to a plotly object
wise_layout <- function(p, title = NULL) {
  p |> layout(
    title = if (!is.null(title)) list(text = title, font = wise_title_font),
    font = wise_font,
    plot_bgcolor = "#F6F7F8",
    paper_bgcolor = "#F6F7F8",
    xaxis = list(gridcolor = "#E2E8F0"),
    yaxis = list(gridcolor = "#E2E8F0"),
    legend = list(orientation = "h", y = 1.1)
  )
}
```

## Conventions

- **Time series**: Always parse timestamps to `POSIXct` before plotting.
- **Interactive**: All charts are interactive by default (Plotly native).
- **Responsive**: Use `plotlyOutput` with `width = "100%"`.
- **Multiple series**: Use `color` parameter with `colors = wise_colors_vec`.
- **Consistent layout**: Wrap all charts in `wise_layout()` for consistent styling.
- **Performance**: For large datasets (>10,000 points), consider downsampling server-side before rendering.

## Chart Types Used

| Use case | Plotly function |
|---|---|
| Time series metrics | `plot_ly(type = "scatter", mode = "lines")` |
| Power/energy comparison | `plot_ly(type = "bar")` |
| Distribution / composition | `plot_ly(type = "pie")` |
| Real-time gauge | `plot_ly(type = "indicator")` |
| Geospatial | `leaflet::leaflet()` |

## Error State

When no data is available, show a message via `validate(need(nrow(df) > 0, "No data available"))` instead of rendering an empty chart.
