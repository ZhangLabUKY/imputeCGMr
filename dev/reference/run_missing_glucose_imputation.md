# Impute real missing glucose values using the CGMissingData Python workflow

Strict R entry point for the real-missing-value imputation workflow in
the Python package `CGMissingData` 0.1.6. When
`imputer_backend = "sklearn"`, the full strict path is executed in
Python through `reticulate`: pandas performs preprocessing and feature
construction, scikit-learn runs `IterativeImputer`, statsmodels runs
segmentwise ARIMA when the missing rate is low, and Python xgboost runs
the high-missingness branch. The completed pandas data frame is then
converted back to R.

The R fallback `imputer_backend = "mice"` keeps the same R-side pipeline
and uses the R package `mice` for the imputation matrix. No
iterative-ridge backend is used.

## Usage

``` r
run_missing_glucose_imputation(
  data,
  target_col,
  feature_cols = NULL,
  id_col = "USUBJID",
  time_col = "Time",
  time_format = "yyyy:mm:dd:hh:nn",
  time_unit = "minute",
  models = "auto",
  rf_n_estimators = 200,
  knn_k = 7,
  xgb_nrounds = 300,
  lgb_nrounds = 400,
  arima_order = c(4L, 1L, 0L),
  seed = 42,
  lag_k = c(1L, 2L, 3L),
  add_rollmean = TRUE,
  roll_window = 3L,
  interval_minutes = 5L,
  use_arima_if_missing_leq = 0.05,
  arima_min_history = 20L,
  imputer_backend = c("mice", "sklearn"),
  prefer_cgmanalyzer_equal_interval = FALSE,
  export = FALSE
)
```

## Arguments

- data:

  A data.frame, an object coercible to data.frame, or a path to a CSV
  file.

- target_col:

  Single character string: target glucose column with missing values to
  impute. Python default name is `"glucose_value"`.

- feature_cols:

  Optional character vector of base feature columns. If `NULL`, the
  Python pipeline feature set is used when available: `TimeSeries`,
  `TimeDifferenceMinutes`, `id_col`, `AGE`, `SEX`, `HBA1C`, `lag1`,
  `lag2`, `lag3`, and `rollmean`. If supplied, the listed columns are
  used together with the generated time, subject, lag, and rolling-mean
  columns that exist in the data.

- id_col:

  Character string: subject identifier column. Python default name is
  `"subjectid"`.

- time_col:

  Character string: raw timestamp column. Python default name is
  `"timestamp"`.

- time_format:

  Retained for compatibility with the old R function. The Python-engine
  path uses pandas timestamp parsing.

- time_unit:

  Retained for compatibility with the old R function and not used by the
  strict Python-engine path.

- models:

  Retained for compatibility. The strict Python workflow auto-selects
  between `MICE+ARIMA` and `MICE+XGBoost` from the missing-rate
  threshold; RF, kNN, LightGBM, and MICE-only are not used.

- rf_n_estimators, knn_k, lgb_nrounds:

  Retained for compatibility and ignored by the strict Python workflow.

- xgb_nrounds:

  Integer: number of XGBoost boosting rounds. Python's `n_estimators`
  default is 300.

- arima_order:

  Integer vector of length 3. Python default is `c(4L, 1L, 0L)`.

- seed:

  Integer seed for scikit-learn and XGBoost. Python default is 42.

- lag_k:

  Integer vector of target lags to compute. Python default is
  `c(1L, 2L, 3L)`.

- add_rollmean:

  Logical: add rolling mean of prior target values. Python always adds
  this; setting `FALSE` is allowed only for compatibility.

- roll_window:

  Integer rolling mean window. Python default is 3.

- interval_minutes:

  Equal interval in minutes. In the Python-engine path, elapsed minutes
  are computed by subject when `TimeSeries` is not already present.

- use_arima_if_missing_leq:

  Numeric missing-rate threshold. If the target missing rate is less
  than or equal to this value, segmentwise ARIMA is used; otherwise
  XGBoost is used. Python default is 0.05.

- arima_min_history:

  Minimum number of prior observations required before fitting ARIMA for
  a missing segment. Python default is 20.

- imputer_backend:

  One of `"mice"` or `"sklearn"`. `"mice"` uses the R package `mice` as
  the CRAN-safe R-native fallback. `"sklearn"` uses Python modules
  through `reticulate` for the full strict workflow and gives the
  closest agreement with the Python package.

- prefer_cgmanalyzer_equal_interval:

  Retained for compatibility. The Python-engine path uses pandas
  elapsed-minute construction unless an existing non-empty `TimeSeries`
  column is supplied.

- export:

  Logical; if `TRUE`, writes the returned imputed data frame to a
  timestamped CSV file in the current working directory. Default is
  `FALSE`.

## Value

A data.frame sorted by `id_col` and `TimeSeries`, matching the Python
package output shape. The original target column is left unchanged, so
rows that were originally missing remain `NA` in `target_col`.
`imputed_glucose_value` contains the completed target values,
`imputation_method` is either `"MICE+ARIMA"` or `"MICE+XGBoost"`, and
`missing_rate` is the original target missing rate. Generated lag and
rolling-mean feature columns are used internally and removed before
return.

## Details

For closest Python-package parity, use `imputer_backend = "sklearn"`
with a Python environment containing `numpy`, `pandas`, `scikit-learn`,
`statsmodels`, and `xgboost`. The sklearn path intentionally calls those
modules directly rather than wrapping the Python package.

The ARIMA branch is segmentwise, matching the Python package: within
each subject, contiguous missing blocks are detected, ARIMA is fit only
to the MICE-completed history before the block, and forecasts replace
the MICE values only when there are at least `arima_min_history` finite
historical values and the ARIMA fit succeeds. Otherwise, the MICE value
is retained.

## Examples

``` r
data("CGMExampleData")
out <- run_missing_glucose_imputation(
  CGMExampleData,
  target_col = "LBORRES",
  feature_cols = c("AGE", "hba1c"),
  id_col = "USUBJID",
  time_col = "Time",
  imputer_backend = "mice"
)
#> Warning: Number of logged events: 35
head(out[, c("LBORRES", "imputed_glucose_value", "imputation_method")])
#>   LBORRES imputed_glucose_value imputation_method
#> 1     150                   150      MICE+XGBoost
#> 2     134                   134      MICE+XGBoost
#> 3     125                   125      MICE+XGBoost
#> 4     132                   132      MICE+XGBoost
#> 5     132                   132      MICE+XGBoost
#> 6     132                   132      MICE+XGBoost
```
