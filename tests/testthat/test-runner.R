test_that("public example dataset has expected missing glucose shape", {
  data("CGMExampleData", package = "CGMissingDataR")

  expect_equal(nrow(CGMExampleData), 500L)
  expect_equal(ncol(CGMExampleData), 5L)
  expect_equal(length(unique(CGMExampleData$USUBJID)), 5L)
  expect_equal(sum(is.na(CGMExampleData$LBORRES)), 50L)
  expect_false("TimeSeries" %in% names(CGMExampleData))
  expect_false("TimeDifferenceMinutes" %in% names(CGMExampleData))
})

expect_strict_imputation_output <- function(out, target_col = "LBORRES") {
  expect_s3_class(out, "data.frame")
  expect_true(all(
    c(
      target_col,
      "TimeSeries",
      "imputed_glucose_value",
      "imputation_method",
      "missing_rate"
    ) %in%
      names(out)
  ))
  expect_false(any(grepl("^lag[0-9]+$", names(out))))
  expect_false("rollmean" %in% names(out))
  expect_true(all(out$imputation_method %in% c("MICE+ARIMA", "MICE+XGBoost")))
  expect_true(all(is.finite(out$missing_rate)))
}

.test_imputation_data <- function(time_values) {
  n <- length(time_values)
  glucose <- 120 + seq_len(n)
  glucose[c(5L, 18L)] <- NA_real_
  data.frame(
    USUBJID = rep(c(1, 2), each = n / 2L),
    LBORRES = glucose,
    Time = time_values,
    AGE = rep(c(42, 55), each = n / 2L),
    hba1c = rep(c(6.4, 7.1), each = n / 2L)
  )
}

.run_mice_imputation <- function(dat) {
  .suppress_expected_imputation_warnings(
    run_missing_glucose_imputation(
      dat,
      target_col = "LBORRES",
      feature_cols = c("AGE", "hba1c"),
      id_col = "USUBJID",
      time_col = "Time",
      imputer_backend = "mice",
      prefer_cgmanalyzer_equal_interval = FALSE
    )
  )
}
.suppress_expected_imputation_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)

      if (
        grepl("^Number of logged events:", msg) ||
          grepl("already had equal intervals", msg)
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
test_that("real missing glucose imputation returns strict-port data frame", {
  skip_if_not_installed("mice")
  skip_if_not_installed("data.table")

  data("CGMExampleData", package = "CGMissingDataR")

  out <- .suppress_expected_imputation_warnings(
    run_missing_glucose_imputation(
      CGMExampleData,
      target_col = "LBORRES",
      feature_cols = c("AGE", "hba1c"),
      id_col = "USUBJID",
      time_col = "Time",
      imputer_backend = "mice",
      xgb_nrounds = 5,
      prefer_cgmanalyzer_equal_interval = FALSE
    )
  )

  expect_strict_imputation_output(out)
  expect_equal(nrow(out), nrow(CGMExampleData))
  expect_equal(sum(is.na(out$LBORRES)), 50L)
  expect_false(anyNA(out$imputed_glucose_value))
  expect_equal(unique(out$missing_rate), 50 / 500)
})

test_that("legacy model argument is ignored with a compatibility warning", {
  skip_if_not_installed("mice")
  skip_if_not_installed("data.table")

  data("CGMExampleData", package = "CGMissingDataR")

  expect_warning(
    out <- .suppress_expected_imputation_warnings(
      run_missing_glucose_imputation(
        CGMExampleData,
        target_col = "LBORRES",
        feature_cols = c("AGE", "hba1c"),
        id_col = "USUBJID",
        time_col = "Time",
        imputer_backend = "mice",
        models = "not_a_model",
        prefer_cgmanalyzer_equal_interval = FALSE
      )
    ),
    regexp = "Strict Python-port mode ignores"
  )
  expect_strict_imputation_output(out)
})

test_that("automatic timestamp parsing accepts common character formats", {
  skip_if_not_installed("mice")
  skip_if_not_installed("data.table")

  base_time <- as.POSIXct("2020-01-16 00:00:00", tz = "UTC") +
    seq(0, by = 300, length.out = 24)
  time_formats <- list(
    colon = format(base_time, "%Y:%m:%d:%H:%M"),
    iso_minutes = format(base_time, "%Y-%m-%d %H:%M"),
    iso_seconds = format(base_time, "%Y-%m-%d %H:%M:%S"),
    slash_seconds = format(base_time, "%Y/%m/%d %H:%M:%S"),
    us_slash_minutes = format(base_time, "%m/%d/%Y %H:%M"),
    iso_t = format(base_time, "%Y-%m-%dT%H:%M:%S")
  )

  for (time_values in time_formats) {
    out <- .run_mice_imputation(.test_imputation_data(time_values))
    expect_strict_imputation_output(out)
    expect_false(anyNA(out$TimeSeries))
  }
})

test_that("automatic timestamp parsing accepts POSIXct timestamps", {
  skip_if_not_installed("mice")
  skip_if_not_installed("data.table")

  base_time <- as.POSIXct("2020-01-16 00:00:00", tz = "UTC") +
    seq(0, by = 300, length.out = 24)

  out <- .run_mice_imputation(.test_imputation_data(base_time))

  expect_strict_imputation_output(out)
  expect_false(anyNA(out$TimeSeries))
})

test_that("automatic timestamp parsing reports unparseable timestamps", {
  skip_if_not_installed("mice")
  skip_if_not_installed("data.table")

  bad_data <- .test_imputation_data(rep("not a timestamp", 24))

  expect_error(
    .run_mice_imputation(bad_data),
    "Some timestamp values could not be parsed"
  )
})

test_that("sklearn Python engine works when explicitly enabled", {
  skip_on_cran()
  if (!identical(Sys.getenv("CGMISSINGDATAR_TEST_PYTHON"), "true")) {
    skip(
      "Set CGMISSINGDATAR_TEST_PYTHON=true to run optional Python-engine tests"
    )
  }
  skip_if_not_installed("reticulate")

  reticulate::py_require(c(
    "numpy",
    "pandas",
    "scikit-learn",
    "statsmodels",
    "xgboost"
  ))

  data("CGMExampleData", package = "CGMissingDataR")

  out <- run_missing_glucose_imputation(
    CGMExampleData,
    target_col = "LBORRES",
    feature_cols = c("AGE", "hba1c"),
    id_col = "USUBJID",
    time_col = "Time",
    imputer_backend = "sklearn",
    xgb_nrounds = 5
  )

  expect_strict_imputation_output(out)
  expect_equal(nrow(out), nrow(CGMExampleData))
  expect_false(anyNA(out$imputed_glucose_value))
})
