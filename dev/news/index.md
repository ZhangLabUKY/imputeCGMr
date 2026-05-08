# Changelog

## CGMissingDataR 0.0.2

### Major changes

- Refocused the package documentation around real missing glucose
  imputation with
  [`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/CGMissingDataR/dev/reference/run_missing_glucose_imputation.md).

- Updated
  [`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/CGMissingDataR/dev/reference/run_missing_glucose_imputation.md)
  to return a single completed data frame rather than nested
  model-specific output objects.

- Added the completed glucose column `imputed_glucose_value` while
  preserving the original glucose column unchanged. Rows that were
  originally missing in the target column remain `NA` in that original
  column.

- Added method labels through `imputation_method`, currently either
  `"MICE+ARIMA"` or `"MICE+XGBoost"`.

- Added `missing_rate`, the observed missingness rate in the target
  glucose column before imputation.

- Added automatic workflow selection based on the observed target
  missing rate: `MICE+ARIMA` is used when the missing rate is less than
  or equal to 5%, and `MICE+XGBoost` is used when the missing rate is
  greater than 5%.

- Added internal lag and rolling-mean feature construction for
  imputation and modeling. These engineered columns are used internally
  and removed from the returned data frame.

- Added support for common timestamp inputs, including colon-separated
  timestamps, ISO-style timestamps, slash-separated timestamps, and
  `POSIXct` values.

- Added an optional Python-compatible backend through
  `imputer_backend = "sklearn"`. This path uses `reticulate` to call
  Python modules directly, including `pandas`, `scikit-learn`,
  `statsmodels`, and Python `xgboost`.

- Kept `imputer_backend = "mice"` as the CRAN-safe default backend.

- Added optional Python-engine tests that are skipped by default and
  only run when explicitly enabled with the `CGMISSINGDATAR_TEST_PYTHON`
  environment variable.

- Updated README and vignette documentation to describe the imputation
  workflow, output columns, timestamp handling, backend options, and
  optional reticulate setup.

### Documentation

- Updated examples to focus on real missing glucose imputation with
  `CGMExampleData`.

- Documented the difference between the default R-native MICE backend
  and the optional Python-compatible sklearn backend.

- Documented the expected output structure: `TimeSeries`,
  `imputed_glucose_value`, `imputation_method`, and `missing_rate`.

- Documented CRAN-safe usage of the optional Python backend.

### Package maintenance

- Updated package version from `0.0.1.9000` to `0.0.2`.

- Moved `reticulate` to optional usage for the Python-compatible
  backend.

- Added and updated tests for the imputation return shape, timestamp
  parsing, internal feature cleanup, and optional Python backend
  behavior.

## CGMissingDataR 0.0.1

CRAN release: 2026-02-03

- Initial package creation preparing for CRAN submission.
