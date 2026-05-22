# How To Use CGMissingDataR

## Overview

CGMissingDataR imputes missing glucose values in continuous glucose
monitoring (CGM) data. The main user-facing function is:

``` r

run_missing_glucose_imputation()
```

The function is designed for real missing glucose values. It handles two
common forms of CGM missingness:

1.  explicit missing glucose values, where a row exists but the glucose
    value is `NA`; and
2.  implicit missing readings, where expected timestamps are absent from
    the data.

Before imputation, the function regularizes each subject to an equal
`interval_minutes` timestamp grid. Missing timestamp gaps are converted
into explicit rows with `target_col = NA`, then imputed by the same
workflow used for explicit missing glucose values.

The returned data frame is intentionally minimal. It contains the
original user-supplied columns plus a completed glucose column named
`imputed_glucose_value`. Internal columns used for timestamp
regularization, time features, lag features, rolling means, model
fitting, and missingness tracking are not returned.

The core workflow is:

1.  read a data frame or CSV file;
2.  parse and sort timestamps by subject;
3.  regularize each subject to an equal `interval_minutes` timestamp
    grid;
4.  insert missing timestamp rows with `target_col = NA`;
5.  create internal time, lag, and rolling-mean features;
6.  impute the target and feature matrix;
7.  choose `MICE+ARIMA` or `MICE+XGBoost` from the post-regularization
    missing rate;
8.  return the original columns plus `imputed_glucose_value`.

## Installation

Install the CRAN release with:

``` r

install.packages("CGMissingDataR")
```

Install the development version with:

``` r

install.packages("devtools")
devtools::install_github("ZhangLabUKY/CGMissingDataR")
```

Load the package:

``` r

library(CGMissingDataR)
```

## Example data

`CGMExmplDat10Pct` is a small multi-subject CGM data set included with
the package. It contains a subject identifier, raw timestamp column,
glucose column, age, and HbA1c.

``` r

data("CGMExmplDat10Pct")

summary_table <- data.frame(
  Rows = nrow(CGMExmplDat10Pct),
  Columns = ncol(CGMExmplDat10Pct),
  Subjects = length(unique(CGMExmplDat10Pct$USUBJID)),
  MissingGlucose = sum(is.na(CGMExmplDat10Pct$LBORRES)),
  MissingPercent = round(mean(is.na(CGMExmplDat10Pct$LBORRES)) * 100, 1)
)

summary_table
#>   Rows Columns Subjects MissingGlucose MissingPercent
#> 1 1440       6        5            144             10
head(CGMExmplDat10Pct)
#>    USUBJID    SEX LBORRES             Time   AGE hba1c
#>      <int> <char>   <num>           <char> <int> <num>
#> 1:      11      F     150 2020:01:16:00:00    34   6.4
#> 2:      11      F     134 2020:01:16:00:05    34   6.4
#> 3:      11      F     125 2020:01:16:00:10    34   6.4
#> 4:      11      F     132 2020:01:16:00:15    34   6.4
#> 5:      11      F     132 2020:01:16:00:20    34   6.4
#> 6:      11      F     132 2020:01:16:00:25    34   6.4
```

The example data intentionally does not include `TimeSeries`. The
imputation function creates required time features internally from the
raw `Time` column.

## Required input columns

At minimum, the imputation function needs:

| Role                    | Argument       | Example column        |
|-------------------------|----------------|-----------------------|
| Glucose value to impute | `target_col`   | `LBORRES`             |
| Subject identifier      | `id_col`       | `USUBJID`             |
| Raw timestamp           | `time_col`     | `Time`                |
| Additional predictors   | `feature_cols` | `AGE`, `hba1c`, `SEX` |

The target column may contain missing values. Predictor columns should
be numeric or coercible to numeric. The `SEX` column, when present, is
internally encoded as `M = 1` and `F = 0`.

## What counts as missing?

CGM exports can represent missingness in two ways.

### Explicit missing glucose values

A row exists, but the glucose value is missing:

| Time  | LBORRES |
|-------|--------:|
| 00:00 |     120 |
| 00:05 |      NA |
| 00:10 |     125 |

The row with `LBORRES = NA` is imputed.

### Timestamp gaps

A row is absent entirely, producing a jump in the timestamp sequence:

| Time  | LBORRES |
|-------|--------:|
| 00:00 |     120 |
| 00:05 |     122 |
| 00:30 |     130 |

With `interval_minutes = 5`, the function internally regularizes this
to:

| Time  | LBORRES |
|-------|--------:|
| 00:00 |     120 |
| 00:05 |     122 |
| 00:10 |      NA |
| 00:15 |      NA |
| 00:20 |      NA |
| 00:25 |      NA |
| 00:30 |     130 |

The inserted rows are then imputed using the same workflow as explicit
`NA` values. Because of this, the returned data frame may have more rows
than the input data when timestamp gaps are present.

## Basic real-imputation workflow

For the CRAN-safe R-native path, use `imputer_backend = "mice"`.

``` r

impute_out <- suppressWarnings(
  run_missing_glucose_imputation(
    CGMExmplDat10Pct,
    target_col = "LBORRES",
    feature_cols = c("AGE", "hba1c", "SEX"),
    id_col = "USUBJID",
    time_col = "Time",
    imputer_backend = "mice",
    xgb_nrounds = 5
  )
)
```

The result is a data frame:

``` r

class(impute_out)
#> [1] "data.frame"
nrow(impute_out)
#> [1] 1440
names(impute_out)
#> [1] "USUBJID"               "SEX"                   "LBORRES"              
#> [4] "Time"                  "AGE"                   "hba1c"                
#> [7] "imputed_glucose_value"
```

The returned columns are the original user-supplied columns plus
`imputed_glucose_value`.

| Column | Meaning |
|----|----|
| Original columns | The user’s input columns, including the original glucose column. |
| Original target column, e.g. `LBORRES` | The original glucose column. Values originally missing or inserted from timestamp gaps remain `NA`. |
| `imputed_glucose_value` | Completed glucose values after imputation. |

``` r

head(impute_out[c(
  "USUBJID",
  "SEX",
  "Time",
  "LBORRES",
  "AGE",
  "hba1c",
  "imputed_glucose_value"
)])
#>   USUBJID SEX                Time LBORRES AGE hba1c imputed_glucose_value
#> 1      11   0 2020-01-16 00:00:00     150  34   6.4                   150
#> 2      11   0 2020-01-16 00:05:00     134  34   6.4                   134
#> 3      11   0 2020-01-16 00:10:00     125  34   6.4                   125
#> 4      11   0 2020-01-16 00:15:00     132  34   6.4                   132
#> 5      11   0 2020-01-16 00:20:00     132  34   6.4                   132
#> 6      11   0 2020-01-16 00:25:00     132  34   6.4                   132
```

The original target column is not overwritten:

``` r

sum(is.na(CGMExmplDat10Pct$LBORRES))
#> [1] 144
sum(is.na(impute_out$LBORRES))
#> [1] 144
sum(is.na(impute_out$imputed_glucose_value))
#> [1] 0
```

Inspect rows where the original target column is missing. These include
explicit missing glucose values and, when timestamp gaps are present,
rows inserted during timestamp regularization.

``` r

missing_rows <- is.na(impute_out$LBORRES)
head(impute_out[missing_rows, c(
  "USUBJID",
  "Time",
  "LBORRES",
  "imputed_glucose_value"
)])
#>    USUBJID                Time LBORRES imputed_glucose_value
#> 10      11 2020-01-16 00:45:00      NA              169.5005
#> 31      11 2020-01-16 02:30:00      NA              158.6228
#> 32      11 2020-01-16 02:35:00      NA              160.8414
#> 33      11 2020-01-16 02:40:00      NA              158.6228
#> 34      11 2020-01-16 02:45:00      NA              167.9637
#> 55      11 2020-01-16 04:30:00      NA              158.6228
```

## How the method is selected

The function automatically chooses the final imputation model from the
target missing rate after timestamp-gap regularization:

- if the missing rate is less than or equal to
  `use_arima_if_missing_leq`, the final method is `MICE+ARIMA`;
- otherwise, the final method is `MICE+XGBoost`.

The default threshold is `0.05`.

Method labels and missingness-tracking columns are internal
implementation details in the minimal user-facing output. The returned
data frame keeps only the original input columns plus
`imputed_glucose_value`.

## Time handling and timestamp regularization

The function accepts common timestamp formats, including
colon-separated, hyphen-separated, slash-separated, ISO-style, and
`POSIXct` inputs.

Examples of accepted character formats include:

``` r

"2020:01:16:00:00"
"2020-01-16 00:00:00"
"2020/01/16 00:00:00"
"01/16/2020 00:00"
"2020-01-16T00:00:00"
```

The function uses the timestamp column and `interval_minutes` to
regularize each subject’s data to an expected CGM interval. The default
is:

``` r

interval_minutes = 5
```

Observed timestamps are aligned to the subject-level interval grid,
missing grid positions are inserted, and the inserted target values are
set to `NA` before imputation.

## Internal engineered features

The workflow creates `TimeSeries`, `TimeDifferenceMinutes`, lag
features, and a rolling mean before imputation. These features help the
model use temporal order, time spacing, and recent glucose history.

For example, after timestamp regularization, lag features are created on
the expanded grid:

| Time  | LBORRES | lag1 | lag2 | lag3 |
|-------|--------:|-----:|-----:|-----:|
| 00:00 |     120 |   NA |   NA |   NA |
| 00:05 |     122 |  120 |   NA |   NA |
| 00:10 |      NA |  122 |  120 |   NA |
| 00:15 |      NA |   NA |  122 |  120 |
| 00:20 |      NA |   NA |   NA |  122 |

These engineered columns are used internally by the imputer and final
model but are removed from the returned data frame.

``` r

grep("^lag[0-9]+$|^rollmean$|^TimeSeries$|^TimeDifferenceMinutes$", names(impute_out), value = TRUE)
#> character(0)
```

This should return an empty character vector because those features are
internal implementation details.

## Continuous imputed values

`imputed_glucose_value` is returned as a continuous numeric model
estimate. It is not rounded to the nearest whole number by default
because downstream analyses may benefit from retaining the
model-estimated precision.

Users who need whole-number glucose values for reporting can round after
imputation:

``` r

impute_out$imputed_glucose_value_rounded <- round(impute_out$imputed_glucose_value)
```

## Optional Python-compatible backend

For closest agreement with the Python reference workflow, use:

``` r

imputer_backend = "sklearn"
```

In that mode, the function sends the input data frame to Python through
`reticulate`. Python then performs preprocessing and imputation with:

- `pandas` for data-frame operations;
- `scikit-learn` for `IterativeImputer`;
- `statsmodels` for ARIMA;
- Python `xgboost` for XGBoost regression.

The completed pandas data frame is then converted back to R.

### Installing optional Python dependencies

Install `reticulate` in R:

``` r

install.packages("reticulate")
```

Declare the Python dependencies before running the Python backend:

``` r

reticulate::py_require(c(
  "numpy",
  "pandas",
  "scikit-learn",
  "statsmodels",
  "xgboost"
))
```

Then call the function with `imputer_backend = "sklearn"`:

``` r

out_py <- run_missing_glucose_imputation(
  CGMExmplDat10Pct,
  target_col = "LBORRES",
  feature_cols = c("AGE", "hba1c"),
  id_col = "USUBJID",
  time_col = "Time",
  imputer_backend = "sklearn",
  xgb_nrounds = 5
)

head(out_py[c(
  "USUBJID",
  "Time",
  "LBORRES",
  "imputed_glucose_value"
)])
```

The Python backend is optional. It is not required for package
installation or for building this vignette.

## Choosing a backend

| Backend | Use case | Notes |
|----|----|----|
| `mice` | Default R-native workflow | CRAN-safe and does not require Python. |
| `sklearn` | Closest Python-compatible workflow | Requires `reticulate` and Python packages. |

Use `mice` for simple installation and CRAN-safe examples. Use `sklearn`
when comparing with the Python reference workflow or when you want
Python libraries to perform the full strict path.

## Exporting results

Set `export = TRUE` to write the returned imputed data frame to a
timestamped CSV file in the current working directory.

``` r

out <- run_missing_glucose_imputation(
  CGMExmplDat10Pct,
  target_col = "LBORRES",
  feature_cols = c("AGE", "hba1c"),
  id_col = "USUBJID",
  time_col = "Time",
  imputer_backend = "mice",
  export = TRUE
)
```

The exported CSV contains the original input columns plus
`imputed_glucose_value`.

## Troubleshooting

### Timestamp parsing errors

If you see an error such as:

``` r
Some timestamp values could not be parsed
```

check the values in your timestamp column:

``` r

head(unique(your_data$Time))
```

Use a standard format such as `YYYY-mm-dd HH:MM:SS`, `YYYY:mm:dd:HH:MM`,
or a `POSIXct` column.

### Unexpected row counts

If the returned data frame has more rows than the input data, this is
expected when timestamp gaps are present. The function creates rows for
missing expected CGM readings before imputation.

If the increase is larger than expected, inspect whether the timestamp
column contains off-grid times such as seconds, irregular minutes, or
mixed timestamp formats.

### Python module errors

If the Python backend reports a missing module such as `sklearn`,
remember that the package is installed as `scikit-learn` but imported as
`sklearn`.

``` r

reticulate::py_require(c("scikit-learn", "pandas", "statsmodels", "xgboost"))
```

If Python was already initialized before declaring requirements, restart
R and run the call again.

### Warnings from `mice`

Small or highly collinear data sets can cause `mice` to report logged
events. This is common with tiny examples and does not necessarily
indicate failure. With real data, inspect those warnings to decide
whether columns should be removed, recoded, or simplified.

## Session information

``` r

utils::sessionInfo()
#> R version 4.6.0 (2026-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] CGMissingDataR_0.0.2
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10       generics_0.1.4    tidyr_1.3.2       CGManalyzer_1.3.1
#>  [5] shape_1.4.6.1     lattice_0.22-9    lme4_2.0-1        digest_0.6.39    
#>  [9] magrittr_2.0.5    mitml_0.4-5       evaluate_1.0.5    grid_4.6.0       
#> [13] iterators_1.0.14  mice_3.19.0       fastmap_1.2.0     xgboost_3.2.1.1  
#> [17] foreach_1.5.2     jomo_2.7-6        jsonlite_2.0.0    glmnet_5.0       
#> [21] Matrix_1.7-5      nnet_7.3-20       backports_1.5.1   survival_3.8-6   
#> [25] purrr_1.2.2       codetools_0.2-20  textshaping_1.0.5 jquerylib_0.1.4  
#> [29] reformulas_0.4.4  Rdpack_2.6.6      cli_3.6.6         rlang_1.2.0      
#> [33] rbibutils_2.4.1   splines_4.6.0     cachem_1.1.0      yaml_2.3.12      
#> [37] pan_1.9           otel_0.2.0        FNN_1.1.4.1       tools_4.6.0      
#> [41] nloptr_2.2.1      minqa_1.2.8       dplyr_1.2.1       ranger_0.18.0    
#> [45] boot_1.3-32       broom_1.0.13      rpart_4.1.27      vctrs_0.7.3      
#> [49] R6_2.6.1          lifecycle_1.0.5   fs_2.1.0          MASS_7.3-65      
#> [53] ragg_1.5.2        pkgconfig_2.0.3   desc_1.4.3        pkgdown_2.2.0    
#> [57] pillar_1.11.1     bslib_0.11.0      data.table_1.18.4 glue_1.8.1       
#> [61] Rcpp_1.1.1-1.1    systemfonts_1.3.2 xfun_0.57         tibble_3.3.1     
#> [65] tidyselect_1.2.1  knitr_1.51        nlme_3.1-169      htmltools_0.5.9  
#> [69] rmarkdown_2.31    compiler_4.6.0
```
