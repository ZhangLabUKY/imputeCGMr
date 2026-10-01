feature_test_data <- function() {
  data.frame(USUBJID = rep(1L, 90),
    Time = as.POSIXct("2026-01-01", tz = "UTC") + (0:89) * 300,
    glucose = replace(100 + sin(1:90) * 20, c(20, 40, 60), NA_real_),
    custom_label = rep(c("Alpha", "Beta", NA_character_), 30),
    sex = "F", age = "42", code = rep(c(1, 2, 3), 30),
    blank = NA_character_, stringsAsFactors = FALSE)
}

test_that("arbitrary categorical predictors preserve original labels and report eligibility", {
  dat <- feature_test_data()
  result <- suppressWarnings(run_missing_glucose_imputation(dat, "glucose",
    feature_cols = c("custom_label", "sex", "age", "blank", "code"),
    feature_types = c(code = "categorical"), models = "knn", seed = 42))
  diagnostics <- attr(result, "feature_diagnostics")
  expect_equal(names(result), c(names(dat), "imputed_glucose_value"))
  expect_identical(result$custom_label, dat$custom_label)
  expect_identical(result$sex, dat$sex)
  expect_identical(result$age, dat$age)
  expect_equal(diagnostics$status, c("used", "constant", "constant", "all_missing", "used"))
  expect_equal(diagnostics$type, c("categorical", "categorical", "numeric", "categorical", "categorical"))
  expect_true(all(is.finite(result$imputed_glucose_value)))
  observed <- !is.na(dat$glucose)
  expect_equal(result$imputed_glucose_value[observed], dat$glucose[observed])
})

test_that("feature overrides validate named numeric columns without obscuring errors", {
  dat <- feature_test_data()
  expect_error(run_missing_glucose_imputation(dat, "glucose", feature_cols = "custom_label",
    feature_types = c(custom_label = "numeric")), "custom_label")
  expect_error(run_missing_glucose_imputation(dat, "glucose", feature_cols = "age",
    feature_types = c(unknown = "categorical")), "feature_types")
  expect_error(run_missing_glucose_imputation(dat, "glucose", feature_cols = "unknown"), "unknown")
  expect_error(run_missing_glucose_imputation(dat, "glucose", feature_cols = "age",
    feature_types = "numeric"), "feature_types")
})

test_that("categorical missingness uses exact indicators including a separate missing level", {
  dat <- feature_test_data()
  prepared <- .cgmd_prepare_features(dat, "custom_label")
  expect_equal(ncol(prepared$data), 2L)
  expect_false(anyNA(prepared$data))
  expect_true(all(as.matrix(prepared$data) %in% c(0, 1)))
  expect_equal(unname(rowSums(prepared$data)[is.na(dat$custom_label)]), rep(1, 30))
  dat$custom_label <- factor(dat$custom_label)
  expect_equal(.cgmd_prepare_features(dat, "custom_label")$diagnostics$type, "categorical")
})

test_that("logical labels and internal-looking column names are handled safely", {
  dat <- feature_test_data()
  dat[["Treatment label"]] <- rep(c(TRUE, FALSE, NA), 30)
  dat[[".cgmd_feature_1_2"]] <- 123
  prepared <- .cgmd_prepare_features(dat, "Treatment label")
  expect_equal(prepared$diagnostics$type, "categorical")
  expect_false(any(names(prepared$data) %in% names(dat)))
  result <- suppressWarnings(run_missing_glucose_imputation(dat, "glucose",
    feature_cols = "Treatment label", models = "knn", seed = 42))
  expect_identical(result[["Treatment label"]], dat[["Treatment label"]])
  expect_true(all(is.finite(result$imputed_glucose_value)))
})

test_that("selected varying predictors are used and exclusions change predictions", {
  set.seed(11)
  dat <- feature_test_data()
  dat$signal <- sample(c(-1, 1), nrow(dat), replace = TRUE)
  dat$glucose <- replace(150 + 60 * dat$signal, seq(10, 90, 10), NA_real_)
  with_feature <- suppressWarnings(run_missing_glucose_imputation(dat, "glucose",
    feature_cols = "signal", models = "knn", lag_k = integer(), add_rollmean = FALSE, seed = 42))
  without_feature <- suppressWarnings(run_missing_glucose_imputation(dat, "glucose",
    feature_cols = character(), models = "knn", lag_k = integer(), add_rollmean = FALSE, seed = 42))
  missing <- is.na(dat$glucose)
  expect_false(isTRUE(all.equal(with_feature$imputed_glucose_value[missing], without_feature$imputed_glucose_value[missing])))
  expect_equal(attr(with_feature, "feature_diagnostics")$status, "used")
  expect_equal(nrow(attr(without_feature, "feature_diagnostics")), 0L)
})

test_that("categorical predictors and metadata survive generated rows and input sorting", {
  dat <- feature_test_data()[c(4, 1, 2), ]
  dat$sex <- "F"
  result <- suppressWarnings(run_missing_glucose_imputation(dat, "glucose",
    feature_cols = c("sex", "custom_label"), models = "knn", lag_k = integer(), add_rollmean = FALSE, seed = 42))
  expect_equal(nrow(result), 4L)
  expect_equal(result$sex, rep("F", 4))
  expect_true(all(is.finite(result$imputed_glucose_value)))
  for (i in seq_len(nrow(dat))) {
    row <- match(dat$Time[[i]], result$Time)
    expect_equal(result$glucose[[row]], dat$glucose[[i]])
    expect_equal(result$custom_label[[row]], dat$custom_label[[i]])
  }
})

test_that("short records omit entirely missing engineered predictors", {
  dat <- feature_test_data()[1:2, ]
  dat$glucose[[2]] <- NA_real_
  result <- suppressWarnings(run_missing_glucose_imputation(dat, "glucose",
    feature_cols = "sex", models = "knn", seed = 42))
  expect_true(all(is.finite(result$imputed_glucose_value)))
})

test_that("MICE supports general categories for all real-imputation models", {
  dat <- feature_test_data()
  for (model in c("auto", "arima", "xgboost", "rf", "knn", "lightgbm")) {
    result <- suppressWarnings(run_missing_glucose_imputation(dat, "glucose",
      feature_cols = c("custom_label", "sex", "code"), feature_types = c(code = "categorical"),
      models = model, imputer_backend = "mice", seed = 42, xgb_nrounds = 5L, rf_n_estimators = 10L, lgb_nrounds = 5L))
    expect_true(all(is.finite(result$imputed_glucose_value)), info = model)
    expect_identical(result$custom_label, dat$custom_label)
    expect_identical(result$sex, dat$sex)
    expect_equal(result$imputed_glucose_value[!is.na(dat$glucose)], dat$glucose[!is.na(dat$glucose)])
  }
})
