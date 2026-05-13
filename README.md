# wise-timeseries-validator

Generic time series validation and cleaning tool built with R/Shiny. Accepts any CSV with timestamped numeric data and provides quality diagnostics, interactive visualizations, step-by-step cleaning with provenance tracking, and CSV export.

No domain-specific logic (energy, weather, etc.) — purely statistical data quality operations.

## Features

- **CSV upload** with auto-detection of delimiters, timestamp formats, and column mapping
- **Quality diagnostics**: gap detection, outlier detection (IQR), stuck sensor detection (RLE)
- **Interactive visualization**: Plotly time series plots with completeness indicators
- **Step-by-step cleaning**: linear interpolation with undo support and provenance flagging
- **Export**: cleaned CSV with quality flags

## Stack

- **R 4.3+** with [golem](https://thinkr-open.github.io/golem/) framework
- **UI**: shiny + bslib
- **Data**: data.table, lubridate, zoo
- **Viz**: plotly, DT

## Installation

```r
# Install dependencies
renv::restore()
```

## Usage

```r
# Run the app
wisetsvali::run_app()
```

## Project Structure

```
R/
  mod_upload.R        # CSV upload, auto-detection, column mapping
  mod_diagnostic.R    # Quality analysis display and summary tables
  mod_viz.R           # Plotly timeseries plot, completeness bar
  mod_cleaning.R      # Step-by-step cleaning, undo, export
  fct_csv_parser.R    # fread() wrapper, timestamp parsing, format detection
  fct_quality.R       # Gap, outlier (IQR), stuck sensor (RLE) detection
  fct_cleaning.R      # Linear interpolation, provenance flagging
  fct_theme.R         # Wise bslib theme + Plotly palette
```

## License

MIT
