
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CGMissingDataR

<!-- badges: start -->

[![R-CMD-check](https://github.com/ZhangLabUKY/CGMissingDataR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZhangLabUKY/CGMissingDataR/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/CGMissingDataR)](https://CRAN.R-project.org/package=CGMissingDataR)
[![CRAN
checks](https://badges.cranchecks.info/summary/CGMissingDataR.svg)](https://cran.r-project.org/web/checks/check_results_CGMissingDataR.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/CGMissingDataR)](https://cran.r-project.org/package=CGMissingDataR)
[![Last Commit
Release](https://img.shields.io/github/last-commit/ZhangLabUKY/CGMissingDataR/master)](https://github.com/ZhangLabUKY/CGMissingDataR/commits/master/)
<!-- badges: end -->

## Installation

Install the released version from CRAN:

``` r
install.packages("CGMissingDataR")
```

Or install the development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("ZhangLabUKY/CGMissingDataR")
```

CGMissingDataR imputes glucose values that are already missing in
continuous glucose monitoring (CGM) data. The main public workflow is:

``` r
run_missing_glucose_imputation()
```

The function accepts a data frame with a subject identifier, timestamp
column, glucose column, and optional subject-level or visit-level
covariates. It returns the original data with completed glucose values
in `imputed_glucose_value` while leaving the original glucose column
unchanged.

## What the imputation workflow does

`run_missing_glucose_imputation()` performs the following steps:

1.  reads a data frame or CSV file,
2.  creates or reuses a `TimeSeries` column,
3.  encodes `SEX` when present,
4.  creates internal lag and rolling-mean glucose features,
5.  imputes the target and feature matrix,
6.  chooses the final model from the observed missing rate:
    - `MICE+ARIMA` when missing rate is `<= 0.05`,
    - `MICE+XGBoost` when missing rate is `> 0.05`,
7.  returns a single completed data frame.

Generated lag columns and `rollmean` are used internally and removed
before the final data frame is returned.

The default R-native backend uses the R package `mice`. For closest
agreement with the Python reference workflow, install `reticulate` and
use the optional Python backend.

``` r
install.packages("reticulate")
```

The Python backend uses these Python packages through `reticulate`:

``` r
reticulate::py_require(c(
  "numpy",
  "pandas",
  "scikit-learn",
  "statsmodels",
  "xgboost"
))
```

## Basic use

``` r
library(CGMissingDataR)

data("CGMExmplDat10Pct")

out <- run_missing_glucose_imputation(
  CGMExmplDat10Pct,
  target_col = "LBORRES",
  feature_cols = c("AGE", "hba1c"),
  id_col = "USUBJID",
  time_col = "Time",
  imputer_backend = "mice",
  prefer_cgmanalyzer_equal_interval = FALSE
)

head(out[c(
  "USUBJID",
  "Time",
  "LBORRES",
  "imputed_glucose_value",
  "imputation_method",
  "missing_rate"
)])
```

The original target column is not overwritten. Rows that were missing in
`LBORRES` remain missing there, and the completed value is stored in
`imputed_glucose_value`.

``` r
missing_rows <- is.na(out$LBORRES)
head(out[missing_rows, c(
  "USUBJID",
  "Time",
  "LBORRES",
  "imputed_glucose_value",
  "imputation_method"
)])
```

## Bundled Shiny app

CGMissingDataR also includes a small Shiny app for users who prefer an
interactive workflow. The app lets users upload a CSV file or load one
of the built-in example data sets, choose the target glucose, subject
ID, timestamp, and feature columns, run
`run_missing_glucose_imputation()`, preview the rows that were
originally missing and imputed, and download the completed data as a CSV
file.

Launch the app from R with:

``` r
run_app()
```

The app supports the same two imputation backends as the main function:

- `mice`, the default CRAN-safe R backend;
- `sklearn`, the optional Python-compatible backend using `reticulate`.

The Shiny app is optional. If it is not already installed, install Shiny
with:

``` r
install.packages("shiny")
```

For package developers, the app is stored under
`inst/shiny/cgm_imputation_app/` and is launched through the exported
`run_app()` helper.

## Optional Python-compatible backend

Use `imputer_backend = "sklearn"` to run the strict Python-compatible
path. In that path, `reticulate` sends the data to Python, where pandas,
scikit-learn, statsmodels, and Python xgboost perform the preprocessing
and calculations. The completed pandas data frame is then converted back
to R.

``` r
out_py <- run_missing_glucose_imputation(
  CGMExmplDat10Pct,
  target_col = "LBORRES",
  feature_cols = c("AGE", "hba1c"),
  id_col = "USUBJID",
  time_col = "Time",
  imputer_backend = "sklearn"
)
```

The Python backend is optional. It is not required for package
installation, loading, or CRAN examples.

## Learn more

The main vignette contains a detailed walkthrough of data requirements,
return columns, backend selection, optional Python setup, and
troubleshooting:

<https://zhanglabuky.github.io/CGMissingDataR/articles/How-To-Use-CGMissingDataR.html>

A separate Shiny app vignette walks through the interactive interface:

<https://zhanglabuky.github.io/CGMissingDataR/articles/Using-the-CGMissingDataR-Shiny-App.html>

## Changelog

The changelog is available at:

<https://zhanglabuky.github.io/CGMissingDataR/news/index.html>
