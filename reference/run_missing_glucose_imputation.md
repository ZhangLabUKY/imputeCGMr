# Impute missing glucose values using Mice and ARIMA/XGBoost

Imputes missing glucose values in continuous glucose monitoring (CGM)
data. The function handles both explicit missing glucose values already
coded as `NA` and implicit missing readings caused by timestamp gaps.
Before imputation, each subject is regularized to an equal
`interval_minutes` timestamp grid; missing timestamp gaps are converted
into explicit rows with `target_col = NA`, then imputed using the
selected backend.

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

  Expected spacing, in minutes, between consecutive CGM readings. The
  default is `5`. The function uses this value to regularize each
  subject's timestamps to an equal-interval grid before imputation.

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

A data.frame containing the original user-supplied columns plus
`imputed_glucose_value`, the completed glucose column. The original
target column is left unchanged, so values that were originally missing
or created from timestamp gaps remain `NA` in `target_col`, while their
completed values are stored in `imputed_glucose_value`.

## Details

The imputation workflow first parses and sorts timestamps within each
subject. Each subject is regularized to an equal `interval_minutes`
grid. If a reading is missing because the timestamp is absent from the
input data, a new row is inserted and the target glucose value is set to
`NA`. These inserted missing values are then imputed using the same
workflow as explicit `NA` values.

Internally, the function creates time features, lag features, and
rolling-mean features to support imputation. These engineered columns
are used only during model fitting and are removed from the returned
data frame.

`imputed_glucose_value` is returned as a continuous numeric model
estimate. Users who require whole-number glucose values for reporting
can round this column after imputation.

## Examples

``` r
data("CGMExmplDat10Pct")
out <- run_missing_glucose_imputation(
  CGMExmplDat10Pct,
  target_col = "LBORRES",
  feature_cols = c("AGE", "hba1c"),
  id_col = "USUBJID",
  time_col = "Time",
  imputer_backend = "mice"
)
#> Warning: The data have already had equal intervals between any two consecutive points. No adjustment!
#> Warning: The data have already had equal intervals between any two consecutive points. No adjustment!
#> Warning: The data have already had equal intervals between any two consecutive points. No adjustment!
#> Warning: The data have already had equal intervals between any two consecutive points. No adjustment!
#> Warning: The data have already had equal intervals between any two consecutive points. No adjustment!
#> Warning: Number of logged events: 36
head(subset(out, is.na("LBORRES")))
#> [1] USUBJID               LBORRES               Time                 
#> [4] AGE                   hba1c                 imputed_glucose_value
#> <0 rows> (or 0-length row.names)
```
