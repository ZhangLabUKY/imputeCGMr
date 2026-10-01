# Using the imputeCGM Shiny App

## Overview

imputeCGM includes an optional Shiny app for interactive missing glucose
imputation. The app is a point-and-click interface around the main
package function:

``` r

run_missing_glucose_imputation()
```

The app is useful when users want to:

- upload a CSV file without writing R code;
- choose the target glucose, subject ID, timestamp, and feature columns
  from a user interface;
- load built-in example data sets for demonstration;
- inspect observed missingness before running imputation;
- run the imputation workflow;
- preview rows where glucose was missing and then imputed;
- download the completed data as a CSV file.

The Shiny app does not implement a separate imputation algorithm. It
calls
[`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/imputeCGMr/dev/reference/run_missing_glucose_imputation.md)
internally and returns the same type of completed data frame as the
command-line workflow.

The imputation workflow handles both explicit missing glucose values
coded as `NA` and missing readings implied by timestamp gaps. During
imputation, each subject is regularized to the expected
`interval_minutes` timestamp grid, so the returned data can contain more
rows than the uploaded data when timestamps are missing.

## Installation

Install imputeCGM from CRAN with:

``` r

install.packages("imputeCGM")
```

The app requires the optional R package `shiny`. If Shiny is not already
installed, install it with:

``` r

install.packages("shiny")
```

Then load the package:

``` r

library(imputeCGM)
```

## Launching the app

Launch the app with:

``` r

run_app()
```

## Input options

The app provides two ways to load data.

### Upload a CSV file

Use the **Browse** button to upload a CSV file containing CGM data. The
file should contain, at minimum, columns corresponding to:

| Role                  | Example column | App selector          |
|-----------------------|---------------:|-----------------------|
| Subject identifier    |      `USUBJID` | Subject ID column     |
| Glucose value         |      `LBORRES` | Target glucose column |
| Timestamp             |         `Time` | Timestamp column      |
| Additional predictors | `AGE`, `hba1c` | Feature columns       |

After the file is uploaded, the app displays a preview of the uploaded
data and populates the column-selection controls.

### Load built-in example data

The app can also load built-in example data sets for demonstration.
These are useful for quickly showing how the workflow behaves without
requiring users to upload their own data.

The example data sets are intended to include:

| Example data | Description |
|----|----|
| `CGMExmplDat5Pct` | Example CGM data with about 5% explicit missing glucose values. |
| `CGMExmplDat10Pct` | Example CGM data with about 10% explicit missing glucose values. |

After selecting an example data set and clicking **Load example data**,
the app uses that data set exactly as if it had been uploaded by the
user.

## Selecting columns

Once data are loaded, select the columns that map to the imputation
function.

### Target glucose column

Choose the glucose column with missing values to impute. In the included
example data, this is usually:

``` r

LBORRES
```

The original target column is preserved in the returned data. Values
that were originally missing, or created from timestamp gaps during
regularization, remain `NA` in this original column. Completed glucose
values are written to a new column named:

``` r

imputed_glucose_value
```

### Subject ID column

Choose the column identifying each subject or participant. In the
example data, this is usually:

``` r

USUBJID
```

The subject ID is used for sorting, timestamp regularization, lag
feature creation, rolling-mean feature creation, and subject-level time
handling.

### Timestamp column

Choose the raw timestamp column. In the example data, this is usually:

``` r

Time
```

The imputation function uses this timestamp column to regularize each
subject to an equal `interval_minutes` CGM grid before imputation.
Common timestamp formats are supported, including colon-separated,
hyphen-separated, slash-separated, ISO-style, and `POSIXct` values.

### Feature columns

Choose additional predictor columns. In the example data, these commonly
include:

``` r

SEX
AGE
hba1c
```

Feature columns can be numeric or categorical. Use the type controls to
override automatic inference, including numeric-coded categories.

Use `feature_cols` to select any numeric or categorical metadata
columns. The package infers types;
`feature_types = c(group_code = "categorical")` marks numeric category
codes explicitly. Categorical predictors use indicator columns with a
deterministic reference category. Missing labels use a separate
missing-category indicator rather than an estimated demographic value.
Both the R-native and Python backends use the same predictor
preparation. Original names, labels, and blanks are preserved in
returned data.

## Missingness summary card

The app includes a missingness summary card beside the uploaded data
preview. After a target glucose column is selected, this card shows the
observed missingness in the loaded data before imputation:

- the percentage of explicit missing values in the selected target
  column;
- the number of explicit missing rows;
- the total number of uploaded rows;
- a warning style when missingness is greater than the chosen threshold,
  such as 20%.

This card is intended as a quick data-quality check before running the
imputation workflow. Timestamp gaps are handled during imputation, so
the final number of rows imputed can be larger than the explicit missing
count shown in this pre-imputation summary.

## Timestamp-gap handling

When imputation runs, the underlying function regularizes each subject
to the expected `interval_minutes` grid. For example, if readings jump
from `00:05` to `00:30`, the function internally creates the missing
rows at `00:10`, `00:15`, `00:20`, and `00:25`, sets the target glucose
value to `NA`, and then imputes those values.

This means the downloaded data can contain more rows than the uploaded
data when there are timestamp gaps.

## Backend selection

The app supports the same backends as
[`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/imputeCGMr/dev/reference/run_missing_glucose_imputation.md).

| Backend | Description | Recommended use |
|----|----|----|
| `mice` | R-native backend using the R package `mice`. | Default, CRAN-safe workflow. |
| `sklearn` | Optional Python-compatible backend through `reticulate`. | Closest agreement with the Python reference workflow. |

### MICE backend

The default backend is:

``` r

imputer_backend = "mice"
```

This backend does not require Python and is the safest choice for most
users. It is also the backend used in CRAN-safe examples and tests.

## Method selection

The **Final imputation method** control mirrors the `models` argument in
[`run_missing_glucose_imputation()`](https://zhanglabuky.github.io/imputeCGMr/dev/reference/run_missing_glucose_imputation.md).
The default **Automatic by missing rate** option uses `MICE+ARIMA` when
missingness is at or below the selected threshold and `MICE+XGBoost`
otherwise.

Users can also force exactly one final method:

- `MICE+ARIMA`;
- `MICE+XGBoost`;
- `MICE+Random Forest`;
- `MICE+kNN`;
- `MICE+LightGBM`.

The app shows only the tuning controls relevant to the selected method.
For example, Random Forest shows the tree count, kNN shows the neighbor
count, and LightGBM shows boosting rounds.

The **Model threads** control maps to `n_threads`. It defaults to `1`
for CRAN-friendly and shared-system-friendly CPU use. Increase it for
faster local XGBoost, Random Forest, or LightGBM runs.

### Optional sklearn backend

The optional Python-compatible backend is:

``` r

imputer_backend = "sklearn"
```

This path sends the data frame to Python through `reticulate`. Python
then uses:

- `pandas` for data-frame operations;
- `scikit-learn` for `IterativeImputer`;
- `statsmodels` for ARIMA;
- Python `xgboost` for XGBoost regression;
- Python `lightgbm` when forcing LightGBM.

imputeCGM requires `reticulate >= 1.41` and declares its core Python
dependencies in `.onLoad()`, adding LightGBM when requested. This does
not start Python. When sklearn runs, reticulate uses its normal
environment selection and can provision a managed environment if no
existing environment takes precedence. Install dependencies during setup
for user- or deployment-selected environments.

The Python backend is optional. It is not required for installing or
loading the package.

## Running imputation

After loading data and selecting columns, click **Run imputation**.

Internally, the app calls code equivalent to:

``` r

out <- run_missing_glucose_imputation(
  data = uploaded_data,
  target_col = selected_target_col,
  feature_cols = selected_feature_cols,
  id_col = selected_id_col,
  time_col = selected_time_col,
  imputer_backend = selected_backend,
  models = selected_method,
  use_arima_if_missing_leq = selected_threshold,
  xgb_nrounds = selected_xgb_rounds,
  rf_n_estimators = selected_rf_trees,
  knn_k = selected_knn_neighbors,
  lgb_nrounds = selected_lightgbm_rounds,
  n_threads = selected_threads,
  seed = selected_seed,
  export_path = NULL
)
```

The returned object is a data frame containing the original input
columns plus:

| Column                  | Meaning                                    |
|-------------------------|--------------------------------------------|
| `imputed_glucose_value` | Completed glucose values after imputation. |

The original target glucose column is left unchanged. Internal time
features, lag features, rolling means, model labels, and
missingness-tracking flags are used during imputation but are not
included in the returned or downloaded data.

## Previewing results

After imputation, the app displays a preview of rows where the target
glucose value is missing in the returned data. This includes explicit
missing glucose values and, when timestamp gaps exist, rows inserted
during timestamp regularization.

For example, the preview is based on logic like:

``` r

imputed_rows <- out[is.na(out[[target_col]]), , drop = FALSE]
head(imputed_rows, 15)
```

The full completed data frame remains available for download.

## Downloading results

Use the **Download imputed CSV** button to save the completed data set.
The CSV is intentionally minimal and contains:

- the original uploaded columns;
- any rows inserted from timestamp gaps;
- `imputed_glucose_value`.

`imputed_glucose_value` is returned as a continuous numeric model
estimate. If whole-number glucose values are needed for reporting, users
can round the column after download.

## Troubleshooting

### The app does not launch

If you see an error saying that Shiny is not installed, run:

``` r

install.packages("shiny")
```

Then restart R and try:

``` r

run_app()
```

### No column choices appear

Column choices appear only after data are loaded. Upload a CSV file or
load one of the built-in example data sets.

### Imputation fails because a timestamp cannot be parsed

Check the timestamp column selected in the app. The values should be
parseable dates or datetimes, for example:

``` r

"2020:01:16:00:00"
"2020-01-16 00:00:00"
"2020/01/16 00:00:00"
"2020-01-16T00:00:00"
```

If the wrong column was selected as the timestamp column, select the
correct column and rerun imputation.

### Downloaded data have more rows than the uploaded file

This can be expected. The imputation workflow creates missing expected
CGM rows from timestamp gaps before imputing glucose values.

### Python backend fails because a Python module is missing

The package declares its requirements automatically. In an externally
managed environment, install missing dependencies during environment
setup or deployment, then restart R and launch the app again. The
repository’s `requirements.txt` lists the direct Python dependencies,
including optional LightGBM.

### Downloaded data contain `NA` in the original glucose column

This is expected. The original target column is intentionally preserved.
The completed values are stored in:

``` r

imputed_glucose_value
```
