# Changelog

## CGMissingDataR 0.0.2

CRAN release: 2026-05-30

### Major changes

- [`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/CGMmissingDataR/reference/run_missing_glucose_imputation.md)
  now handles both explicit missing glucose values and missing readings
  implied by timestamp gaps. When timestamps skip expected CGM
  intervals, the function regularizes each subject to the expected
  interval and imputes the newly created missing glucose rows.

- The returned data frame is now simpler. It contains the user’s
  original columns plus `imputed_glucose_value`. Internal columns used
  for timestamp regularization, lag features, rolling means, model
  fitting, and missingness tracking are no longer returned.

- The original glucose column is still preserved. Values that were
  originally missing, or created from timestamp gaps, remain `NA` in the
  original target column, while completed values are stored in
  `imputed_glucose_value`.

- `imputed_glucose_value` is returned as a continuous numeric model
  estimate. Users who need whole-number glucose values for reporting can
  round this column after imputation.

- [`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/CGMmissingDataR/reference/run_missing_glucose_imputation.md)
  now supports selectable real-imputation methods through the existing
  `models` argument. The default `models = "auto"` keeps the
  missing-rate rule, using `MICE+ARIMA` when missingness is at or below
  the configured threshold and `MICE+XGBoost` otherwise.

- Users can now force one final real-imputation method with
  `models = "arima"`, `"xgboost"`, `"rf"`, `"knn"`, or `"lightgbm"`.
  Random Forest, kNN, and LightGBM use the same lag-feature workflow as
  the existing ARIMA and XGBoost real-imputation paths.

- Real-imputation model engines now use `n_threads = 1` by default for
  CRAN-friendly and shared-system-friendly CPU use. Users can increase
  `n_threads` for faster local XGBoost, Random Forest, or LightGBM runs.

- Added a bundled Shiny app for interactive missing glucose imputation.
  The app lets users upload a CSV file or load example data, choose the
  relevant columns, select the final imputation method, run imputation,
  preview results, and download the completed data.

- Added built-in example data for demonstrating both explicit missing
  glucose values and timestamp-gap handling.

- The optional Python-compatible backend remains available with
  `imputer_backend = "sklearn"`. The default backend remains
  `imputer_backend = "mice"` for standard R usage. Both backends support
  the selectable final imputation methods, with Python LightGBM
  available when the optional Python `lightgbm` module is installed.

- Updated README and vignettes to describe timestamp-gap handling, the
  simplified output structure, selectable final imputation methods, the
  bundled Shiny app, backend options, and post-imputation rounding.

## CGMissingDataR 0.0.1

CRAN release: 2026-02-03

- Initial package creation preparing for CRAN submission.
