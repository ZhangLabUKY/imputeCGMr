# Launch the imputeCGM Shiny App

Launches a Shiny app for uploading a CGM data file, selecting the
target, subject, timestamp, and feature columns, running
[`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/imputeCGM/reference/run_missing_glucose_imputation.md),
previewing the imputed data, and downloading the completed data as a CSV
file.

## Usage

``` r
run_app()
```

## Value

Invisibly returns the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Examples

``` r
if (FALSE) { # \dontrun{
# Run the imputeCGM Shiny app
run_app()
} # }

```
