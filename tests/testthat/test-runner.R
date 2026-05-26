test_that("public example dataset has expected missing glucose shape", {
  data("CGMExmplDat10Pct", package = "CGMissingDataR")

  expect_equal(nrow(CGMExmplDat10Pct), 1440L)
  expect_equal(ncol(CGMExmplDat10Pct), 6L)
  expect_equal(length(unique(CGMExmplDat10Pct$USUBJID)), 5L)
  expect_equal(sum(is.na(CGMExmplDat10Pct$LBORRES)), 144L)
  expect_false("TimeSeries" %in% names(CGMExmplDat10Pct))
  expect_false("TimeDifferenceMinutes" %in% names(CGMExmplDat10Pct))
})

expect_strict_imputation_output <- function(
  out,
  target_col = "LBORRES",
  input_cols = NULL
) {
  expect_s3_class(out, "data.frame")

  expect_true(target_col %in% names(out))
  expect_true("imputed_glucose_value" %in% names(out))

  if (!is.null(input_cols)) {
    expect_true(all(input_cols %in% names(out)))
    expect_equal(
      setdiff(names(out), input_cols),
      "imputed_glucose_value"
    )
  }

  internal_cols <- c(
    "TimeSeries",
    "TimeDifferenceMinutes",
    "imputation_method",
    "missing_rate",
    "inserted_timestamp_gap",
    "explicit_missing_glucose",
    "missing_source",
    "rollmean"
  )

  expect_false(any(internal_cols %in% names(out)))
  expect_false(any(grepl("^lag[0-9]+$", names(out))))
  expect_false(anyNA(out$imputed_glucose_value))
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

.run_mice_imputation <- function(dat, models = "auto") {
  .suppress_expected_imputation_warnings(
    run_missing_glucose_imputation(
      dat,
      target_col = "LBORRES",
      feature_cols = c("AGE", "hba1c"),
      id_col = "USUBJID",
      time_col = "Time",
      imputer_backend = "mice",
      models = models,
      xgb_nrounds = 5,
      rf_n_estimators = 25,
      lgb_nrounds = 25,
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

  data("CGMExmplDat10Pct", package = "CGMissingDataR")

  out <- .suppress_expected_imputation_warnings(
    run_missing_glucose_imputation(
      CGMExmplDat10Pct,
      target_col = "LBORRES",
      feature_cols = c("AGE", "hba1c"),
      id_col = "USUBJID",
      time_col = "Time",
      imputer_backend = "mice",
      xgb_nrounds = 5,
      prefer_cgmanalyzer_equal_interval = FALSE
    )
  )

  expect_strict_imputation_output(
    out,
    target_col = "LBORRES",
    input_cols = names(CGMExmplDat10Pct)
  )

  expect_equal(nrow(out), nrow(CGMExmplDat10Pct))
  expect_equal(sum(is.na(out$LBORRES)), 144L)
  expect_false(anyNA(out$imputed_glucose_value))
})

test_that("models = NULL works like automatic method selection", {
  skip_if_not_installed("mice")
  skip_if_not_installed("data.table")

  data("CGMExmplDat10Pct", package = "CGMissingDataR")

  out <- .suppress_expected_imputation_warnings(
    run_missing_glucose_imputation(
      CGMExmplDat10Pct,
      target_col = "LBORRES",
      feature_cols = c("AGE", "hba1c"),
      id_col = "USUBJID",
      time_col = "Time",
      imputer_backend = "mice",
      models = NULL,
      xgb_nrounds = 5,
      prefer_cgmanalyzer_equal_interval = FALSE
    )
  )
  expect_strict_imputation_output(
    out,
    target_col = "LBORRES",
    input_cols = names(CGMExmplDat10Pct)
  )
})

test_that("invalid real-imputation model selections error clearly", {
  expect_error(
    run_missing_glucose_imputation(
      data.frame(USUBJID = 1, LBORRES = NA_real_, Time = "2020-01-01"),
      target_col = "LBORRES",
      id_col = "USUBJID",
      time_col = "Time",
      models = "all"
    ),
    "not supported"
  )
  expect_error(
    run_missing_glucose_imputation(
      data.frame(USUBJID = 1, LBORRES = NA_real_, Time = "2020-01-01"),
      target_col = "LBORRES",
      id_col = "USUBJID",
      time_col = "Time",
      models = "mice_only"
    ),
    "not supported"
  )
  expect_error(
    run_missing_glucose_imputation(
      data.frame(USUBJID = 1, LBORRES = NA_real_, Time = "2020-01-01"),
      target_col = "LBORRES",
      id_col = "USUBJID",
      time_col = "Time",
      models = c("rf", "knn")
    ),
    "exactly one"
  )
  expect_error(
    run_missing_glucose_imputation(
      data.frame(USUBJID = 1, LBORRES = NA_real_, Time = "2020-01-01"),
      target_col = "LBORRES",
      id_col = "USUBJID",
      time_col = "Time",
      models = "not_a_model"
    ),
    "Invalid models value"
  )
})

test_that("R backend supports forced real-imputation methods", {
  skip_if_not_installed("mice")
  skip_if_not_installed("data.table")

  base_time <- as.POSIXct("2020-01-16 00:00:00", tz = "UTC") +
    seq(0, by = 300, length.out = 24)
  dat <- .test_imputation_data(base_time)

  method_requirements <- list(
    arima = character(0),
    xgboost = "xgboost",
    rf = "ranger",
    knn = "FNN",
    lightgbm = "lightgbm"
  )

  for (model in names(method_requirements)) {
    for (pkg in method_requirements[[model]]) {
      skip_if_not_installed(pkg)
    }

    out <- .run_mice_imputation(dat, models = model)
    expect_strict_imputation_output(
      out,
      target_col = "LBORRES",
      input_cols = names(dat)
    )
    expect_equal(nrow(out), nrow(dat))
    expect_equal(sum(is.na(out$LBORRES)), 2L)
  }
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
    dat <- .test_imputation_data(time_values)
    out <- .run_mice_imputation(dat)

    expect_strict_imputation_output(
      out,
      target_col = "LBORRES",
      input_cols = names(dat)
    )

    expect_equal(nrow(out), nrow(dat))
    expect_equal(sum(is.na(out$LBORRES)), 2L)
  }
})

test_that("automatic timestamp parsing accepts POSIXct timestamps", {
  skip_if_not_installed("mice")
  skip_if_not_installed("data.table")

  base_time <- as.POSIXct("2020-01-16 00:00:00", tz = "UTC") +
    seq(0, by = 300, length.out = 24)

  dat <- .test_imputation_data(base_time)
  out <- .run_mice_imputation(dat)

  expect_strict_imputation_output(
    out,
    target_col = "LBORRES",
    input_cols = names(dat)
  )

  expect_equal(nrow(out), nrow(dat))
  expect_equal(sum(is.na(out$LBORRES)), 2L)
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

  data("CGMExmplDat10Pct", package = "CGMissingDataR")

  out <- run_missing_glucose_imputation(
    CGMExmplDat10Pct,
    target_col = "LBORRES",
    feature_cols = c("AGE", "hba1c"),
    id_col = "USUBJID",
    time_col = "Time",
    imputer_backend = "sklearn",
    xgb_nrounds = 5
  )

  expect_strict_imputation_output(
    out,
    target_col = "LBORRES",
    input_cols = names(CGMExmplDat10Pct)
  )

  expect_equal(nrow(out), nrow(CGMExmplDat10Pct))
  expect_false(anyNA(out$imputed_glucose_value))
})

test_that("sklearn Python engine supports forced real-imputation methods", {
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

  base_time <- as.POSIXct("2020-01-16 00:00:00", tz = "UTC") +
    seq(0, by = 300, length.out = 24)
  dat <- .test_imputation_data(base_time)

  forced_models <- c("arima", "xgboost", "rf", "knn")
  if (reticulate::py_module_available("lightgbm")) {
    forced_models <- c(forced_models, "lightgbm")
  }

  for (model in forced_models) {
    out <- run_missing_glucose_imputation(
      dat,
      target_col = "LBORRES",
      feature_cols = c("AGE", "hba1c"),
      id_col = "USUBJID",
      time_col = "Time",
      imputer_backend = "sklearn",
      models = model,
      xgb_nrounds = 5,
      rf_n_estimators = 25,
      lgb_nrounds = 25
    )

    expect_strict_imputation_output(
      out,
      target_col = "LBORRES",
      input_cols = names(dat)
    )
    expect_equal(nrow(out), nrow(dat))
    expect_false(anyNA(out$imputed_glucose_value))
  }
})

test_that("Shiny app exposes and maps selectable imputation methods", {
  skip_if_not_installed("shiny")

  app_path <- system.file(
    "shiny",
    "cgm_imputation_app",
    "app.R",
    package = "CGMissingDataR"
  )
  if (identical(app_path, "")) {
    app_path <- file.path(
      getwd(),
      "inst",
      "shiny",
      "cgm_imputation_app",
      "app.R"
    )
  }

  app_env <- new.env(parent = globalenv())
  source(app_path, local = app_env)

  expect_equal(
    unname(app_env$.imputation_method_choices),
    c("auto", "arima", "xgboost", "rf", "knn", "lightgbm")
  )

  dat <- .test_imputation_data(as.POSIXct("2020-01-16 00:00:00", tz = "UTC") +
    seq(0, by = 300, length.out = 24))

  for (model in c("auto", "arima", "xgboost", "rf", "knn", "lightgbm")) {
    args <- app_env$.cgmd_shiny_imputation_args(
      data = dat,
      target_col = "LBORRES",
      feature_cols = c("AGE", "hba1c"),
      id_col = "USUBJID",
      time_col = "Time",
      imputer_backend = "mice",
      models = model,
      use_arima_if_missing_leq = 0.05,
      xgb_nrounds = 5,
      rf_n_estimators = 25,
      knn_k = 3,
      lgb_nrounds = 25,
      seed = 42,
      prefer_cgmanalyzer_equal_interval = FALSE
    )

    expect_identical(args$models, model)
    expect_identical(args$rf_n_estimators, 25)
    expect_identical(args$knn_k, 3)
    expect_identical(args$lgb_nrounds, 25)
    expect_false(args$export)
  }
})
