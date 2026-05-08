# How To Use CGMissingDataR

## Overview

CGMissingDataR imputes glucose values that are already missing in
continuous glucose monitoring (CGM) data. The main user-facing function
is:

``` r

run_missing_glucose_imputation()
```

The function is designed for real missing glucose values. It returns a
single data frame with the original glucose column left unchanged and a
new completed glucose column named `imputed_glucose_value`.

The core workflow is:

1.  read a data frame or CSV file;
2.  create or reuse a `TimeSeries` column;
3.  encode `SEX` when present;
4.  create internal lag and rolling-mean glucose features;
5.  impute the target and feature matrix;
6.  choose `MICE+ARIMA` or `MICE+XGBoost` from the observed missing
    rate;
7.  remove internal-only engineered features before returning the data
    frame.

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

`CGMExampleData` is a small multi-subject CGM data set included with the
package. It contains a subject identifier, raw timestamp column, glucose
column, age, and HbA1c.

``` r

data("CGMExampleData")

summary_table <- data.frame(
  Rows = nrow(CGMExampleData),
  Columns = ncol(CGMExampleData),
  Subjects = length(unique(CGMExampleData$USUBJID)),
  MissingGlucose = sum(is.na(CGMExampleData$LBORRES)),
  MissingPercent = round(mean(is.na(CGMExampleData$LBORRES)) * 100, 1)
)

summary_table
#>   Rows Columns Subjects MissingGlucose MissingPercent
#> 1  500       5        5             50             10
head(CGMExampleData)
#>   USUBJID LBORRES             Time AGE hba1c
#> 1      11     150 2020:01:16:00:00  34   6.4
#> 2      11     134 2020:01:16:00:05  34   6.4
#> 3      11     125 2020:01:16:00:10  34   6.4
#> 4      11     132 2020:01:16:00:15  34   6.4
#> 5      11     132 2020:01:16:00:20  34   6.4
#> 6      11     132 2020:01:16:00:25  34   6.4
```

The example data intentionally does not include `TimeSeries`. The
imputation function creates that column internally from the raw `Time`
column.

## Required input columns

At minimum, the imputation function needs:

| Role                    | Argument       | Example column |
|-------------------------|----------------|----------------|
| Glucose value to impute | `target_col`   | `LBORRES`      |
| Subject identifier      | `id_col`       | `USUBJID`      |
| Raw timestamp           | `time_col`     | `Time`         |
| Additional predictors   | `feature_cols` | `AGE`, `hba1c` |

The target column may contain missing values. Predictor columns should
be numeric or coercible to numeric. The `SEX` column, when present, is
internally encoded as `M = 1` and `F = 0`.

## Basic real-imputation workflow

For the CRAN-safe R-native path, use `imputer_backend = "mice"`.

``` r

impute_out <- suppressWarnings(
  run_missing_glucose_imputation(
    CGMExampleData,
    target_col = "LBORRES",
    feature_cols = c("AGE", "hba1c"),
    id_col = "USUBJID",
    time_col = "Time",
    imputer_backend = "mice",
    prefer_cgmanalyzer_equal_interval = FALSE,
    xgb_nrounds = 5
  )
)
```

The result is a data frame:

``` r

class(impute_out)
#> [1] "data.frame"
nrow(impute_out)
#> [1] 500
names(impute_out)
#> [1] "USUBJID"               "LBORRES"               "Time"                 
#> [4] "AGE"                   "hba1c"                 "TimeSeries"           
#> [7] "imputed_glucose_value" "imputation_method"     "missing_rate"
```

The most important returned columns are:

| Column | Meaning |
|----|----|
| Original target column, e.g. `LBORRES` | The original glucose column. Values originally missing remain `NA`. |
| `TimeSeries` | Numeric elapsed time feature derived from the timestamp column. |
| `imputed_glucose_value` | Completed glucose values after imputation. |
| `imputation_method` | Final method used: `MICE+ARIMA` or `MICE+XGBoost`. |
| `missing_rate` | Original missing rate of the target column. |

``` r

head(impute_out[c(
  "USUBJID",
  "Time",
  "TimeSeries",
  "LBORRES",
  "imputed_glucose_value",
  "imputation_method",
  "missing_rate"
)])
#>   USUBJID             Time TimeSeries LBORRES imputed_glucose_value
#> 1      11 2020:01:16:00:00          0     150                   150
#> 2      11 2020:01:16:00:05          5     134                   134
#> 3      11 2020:01:16:00:10         10     125                   125
#> 4      11 2020:01:16:00:15         15     132                   132
#> 5      11 2020:01:16:00:20         20     132                   132
#> 6      11 2020:01:16:00:25         25     132                   132
#>   imputation_method missing_rate
#> 1      MICE+XGBoost          0.1
#> 2      MICE+XGBoost          0.1
#> 3      MICE+XGBoost          0.1
#> 4      MICE+XGBoost          0.1
#> 5      MICE+XGBoost          0.1
#> 6      MICE+XGBoost          0.1
```

The original target column is not overwritten:

``` r

sum(is.na(CGMExampleData$LBORRES))
#> [1] 50
sum(is.na(impute_out$LBORRES))
#> [1] 50
sum(is.na(impute_out$imputed_glucose_value))
#> [1] 0
```

Inspect only the rows where glucose was originally missing:

``` r

missing_rows <- is.na(impute_out$LBORRES)
head(impute_out[missing_rows, c(
  "USUBJID",
  "Time",
  "LBORRES",
  "imputed_glucose_value",
  "imputation_method"
)])
#>    USUBJID             Time LBORRES imputed_glucose_value imputation_method
#> 10      11 2020:01:16:00:45      NA              158.1355      MICE+XGBoost
#> 31      11 2020:01:16:02:30      NA              148.1590      MICE+XGBoost
#> 32      11 2020:01:16:02:35      NA              152.9331      MICE+XGBoost
#> 33      11 2020:01:16:02:40      NA              151.6519      MICE+XGBoost
#> 34      11 2020:01:16:02:45      NA              152.9331      MICE+XGBoost
#> 55      11 2020:01:16:04:30      NA              147.2657      MICE+XGBoost
```

## How the method is selected

The function automatically chooses the final imputation model from the
target missing rate:

- if missing rate is less than or equal to `use_arima_if_missing_leq`,
  the final method is `MICE+ARIMA`;
- otherwise, the final method is `MICE+XGBoost`.

The default threshold is `0.05`.

``` r

unique(impute_out$missing_rate)
#> [1] 0.1
unique(impute_out$imputation_method)
#> [1] "MICE+XGBoost"
```

For data sets with more than 5% target missingness, the default final
method is usually `MICE+XGBoost`. For lower missingness, the function
uses segmentwise ARIMA when enough subject-level history is available.

## Time handling

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

If a non-empty `TimeSeries` column is already present, the function
reuses it. Otherwise, it creates `TimeSeries` from the timestamp column.

For quieter reproducible examples, this vignette uses:

``` r

prefer_cgmanalyzer_equal_interval = FALSE
```

That bypasses
[`CGManalyzer::equalInterval.fn()`](https://rdrr.io/pkg/CGManalyzer/man/equalInterval.fn.html)
and uses elapsed minutes by subject. Users who want the CGManalyzer
equal-interval behavior can leave the default value as `TRUE`.

## Internal engineered features

The workflow creates lag and rolling-mean features before imputation.
These features are used internally by the imputer and final model, but
they are not included in the returned data frame.

``` r

grep("^lag[0-9]+$|^rollmean$", names(impute_out), value = TRUE)
#> character(0)
```

This should return an empty character vector because those features are
internal implementation details.

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
  CGMExampleData,
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
  "imputed_glucose_value",
  "imputation_method"
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
  CGMExampleData,
  target_col = "LBORRES",
  feature_cols = c("AGE", "hba1c"),
  id_col = "USUBJID",
  time_col = "Time",
  imputer_backend = "mice",
  export = TRUE
)
```

The function still returns the data frame invisibly to the assignment
target.

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
#> [1] CGMissingDataR_0.0.1.9000
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
#> [37] pan_1.9           FNN_1.1.4.1       tools_4.6.0       nloptr_2.2.1     
#> [41] minqa_1.2.8       dplyr_1.2.1       ranger_0.18.0     boot_1.3-32      
#> [45] broom_1.0.12      rpart_4.1.27      vctrs_0.7.3       R6_2.6.1         
#> [49] lifecycle_1.0.5   fs_2.1.0          MASS_7.3-65       ragg_1.5.2       
#> [53] pkgconfig_2.0.3   desc_1.4.3        pkgdown_2.2.0     pillar_1.11.1    
#> [57] bslib_0.10.0      data.table_1.18.4 glue_1.8.1        Rcpp_1.1.1-1.1   
#> [61] systemfonts_1.3.2 xfun_0.57         tibble_3.3.1      tidyselect_1.2.1 
#> [65] knitr_1.51        nlme_3.1-169      htmltools_0.5.9   rmarkdown_2.31   
#> [69] compiler_4.6.0
```
