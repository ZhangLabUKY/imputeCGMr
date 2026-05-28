# Impute missing glucose values using selectable MICE-based methods

Imputes missing glucose values in continuous glucose monitoring (CGM)
data. The function handles both explicit missing glucose values already
coded as `NA` and implicit missing readings caused by timestamp gaps.
Before imputation, each subject is regularized to an equal
`interval_minutes` timestamp grid; missing timestamp gaps are converted
into explicit rows with `target_col = NA`, then imputed using the
selected backend and final imputation method.

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
  missing_warning_threshold = 0.2,
  study_start = NULL,
  study_end = NULL,
  use_arima_if_missing_leq = 0.05,
  arima_min_history = 20L,
  imputer_backend = c("mice", "sklearn"),
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

  Final real-imputation method selector. Use `NULL` or `"auto"` to keep
  the default missing-rate rule: `MICE+ARIMA` when the target missing
  rate is less than or equal to `use_arima_if_missing_leq`, otherwise
  `MICE+XGBoost`. Use exactly one of `"arima"`, `"xgboost"`, `"rf"`,
  `"knn"`, or `"lightgbm"` to force a specific method regardless of
  missing rate.

- rf_n_estimators:

  Integer number of Random Forest trees. Used when `models = "rf"`.

- knn_k:

  Integer number of nearest neighbors. Used when `models = "knn"`.

- xgb_nrounds:

  Integer number of XGBoost boosting rounds. Used when
  `models = "xgboost"` and may be used by `models = "auto"` when the
  missing-rate rule selects XGBoost.

- lgb_nrounds:

  Integer number of LightGBM boosting rounds. Used when
  `models = "lightgbm"`.

- arima_order:

  Integer vector of length 3. Python default is `c(4L, 1L, 0L)`.

- seed:

  Integer seed for reproducible MICE, tree-based models, and the
  Python-compatible backend. Default is 42.

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

- missing_warning_threshold:

  Numeric value between 0 and 1. If the missingness rate in `target_col`
  after timestamp-gap regularization exceeds this threshold, a warning
  is issued. Default is `0.20`.

- study_start:

  Optional study start timestamp. If supplied, the function reports
  subjects whose first observed CGM timestamp occurs after this time.
  Leading study time is not imputed.

- study_end:

  Optional study end timestamp. If supplied, the function reports
  subjects whose last observed CGM timestamp occurs before this time.
  Trailing study time is not imputed.

- use_arima_if_missing_leq:

  Numeric missing-rate threshold used only when `models` is `NULL` or
  `"auto"`. If the target missing rate is less than or equal to this
  value, segmentwise ARIMA is used; otherwise XGBoost is used. Default
  is 0.05.

- arima_min_history:

  Minimum number of prior observations required before fitting ARIMA for
  a missing segment. Python default is 20.

- imputer_backend:

  One of `"mice"` or `"sklearn"`. `"mice"` uses the R package `mice` as
  the CRAN-safe R-native backend. `"sklearn"` uses Python modules
  through `reticulate` for a Python-compatible workflow.

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
workflow as explicit `NA` values. The deterministic interval grid is
controlled by this package; `CGManalyzer`'s equal-interval helper is
called internally for workflow consistency.

Internally, the function creates time features, lag features, and
rolling-mean features to support imputation. MICE first completes the
target and feature matrix. The selected final method then fills the
missing glucose positions in `imputed_glucose_value`: either by
segmentwise ARIMA or by a supervised model trained on observed glucose
values and the MICE-completed feature matrix. These engineered columns
are used only during model fitting and are removed from the returned
data frame.

`imputed_glucose_value` is returned as a continuous numeric model
estimate. Users who require whole-number glucose values for reporting
can round this column after imputation.

Missingness warnings are based on the data after timestamp-gap
regularization, so both explicit `NA` glucose values and rows created
from timestamp gaps contribute to the reported missingness rate. The
function also warns when long contiguous missing blocks of at least 12
or 24 hours are detected. If `study_start` or `study_end` is supplied,
leading or trailing study-period coverage gaps are reported but are not
imputed.

## Examples

``` r
data("CGMExmplDat5Pct")
out <- run_missing_glucose_imputation(
  CGMExmplDat5Pct,
  target_col = "LBORRES",
  feature_cols = c("AGE", "hba1c"),
  id_col = "USUBJID",
  time_col = "Time",
  imputer_backend = "mice"
)
#> Warning: Number of logged events: 41
head(subset(out, is.na(LBORRES)))
#>     USUBJID SEX LBORRES                Time AGE hba1c imputed_glucose_value
#> 10       11   0      NA 2020-01-16 00:45:00  34   6.4             124.76679
#> 31       11   0      NA 2020-01-16 02:30:00  34   6.4              83.82781
#> 32       11   0      NA 2020-01-16 02:35:00  34   6.4              82.37366
#> 55       11   0      NA 2020-01-16 04:30:00  34   6.4              78.75699
#> 90       11   0      NA 2020-01-16 07:25:00  34   6.4             113.84458
#> 146      11   0      NA 2020-01-16 12:05:00  34   6.4             129.06611
```
