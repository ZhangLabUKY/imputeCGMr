# Run missingness benchmark (target-masking with LAG features)

**\[deprecated\]**

This function is deprecated. Use
[`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/imputeCGMR/reference/run_missing_glucose_imputation.md)
for real missing glucose values.

This function implements missingness benchmarking by masking the target
column at various rates and evaluating imputation and predictive
performance of MICE, Random Forest, and KNN methods. Additionally, it
includes LAG features of the target variable to assess their impact on
imputation and prediction. The function returns a data.frame summarizing
the Mask Rate, Method, MRD (Mean Relative Difference), and Masked Count
for each method and mask rate.

## Usage

``` r
run_missingness_benchmark(
  data,
  target_col,
  feature_cols = NULL,
  id_col = "USUBJID",
  time_col = "TimeSeries",
  mask_rates = c(0.05, 0.1, 0.2, 0.3, 0.4),
  mask_type = c("random", "block"),
  rf_n_estimators = 400,
  knn_k = 7,
  seed = NULL,
  lag_k = c(1, 2, 3),
  add_rollmean = TRUE,
  roll_window = 3
)
```

## Arguments

- data:

  A data.frame (or object coercible to data.frame), OR a path to a CSV
  file.

- target_col:

  Single character string: name of the outcome column to mask/impute
  (e.g., "LBORRES", "Glucose").

- feature_cols:

  Character vector of base feature columns (excluding the target). If
  NULL, uses all columns except `target_col`.

- id_col:

  Character string: subject identifier column used for LAG features
  (default "USUBJID").

- time_col:

  Character string: time-ordering column used for LAG features (default
  "TimeSeries").

- mask_rates:

  Numeric vector in (0, 1): fraction of rows to mask (default 0.05,
  0.10, 0.20, 0.30, 0.40).

- mask_type:

  One of `"random"` or `"block"`.

- rf_n_estimators:

  Integer: number of trees for random forest (default 400).

- knn_k:

  Integer: number of neighbors for kNN (default 7).

- seed:

  Optional integer seed used for masking, MICE, and models. The default
  `NULL` does not set the user's random-number generator state.

- lag_k:

  Integer vector of lags to compute on the target (default c(1,2,3)).

- add_rollmean:

  Logical: add rolling mean feature of prior target values (default
  TRUE).

- roll_window:

  Integer: rolling window length for rollmean (default 3).

## Value

A data.frame with columns: MaskRate, Method, MRD, MaskedCount.

## Details

LAG features are computed using
[`data.table::shift()`](https://rdrr.io/pkg/data.table/man/shift.html)
(fast lag/lead). The rolling mean is computed with
[`data.table::frollmean()`](https://rdrr.io/pkg/data.table/man/froll.html)
using `align="right"` and `fill=NA`.
