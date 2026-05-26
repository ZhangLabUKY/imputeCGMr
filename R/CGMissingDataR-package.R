#' CGMmissingDataR: Impute Missing Glucose Values in CGM Data
#'
#' Imputes missing glucose values in repeated-measures continuous glucose
#' monitoring (CGM) data. Workflows create time-series features from raw
#' timestamps, support model selection, and return model-specific completed
#' data sets for glucose values that are already missing in user data.
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(":=", "..lag_cols", "LB_rollmean_3"))

## usethis namespace: start
## usethis namespace: end
NULL
