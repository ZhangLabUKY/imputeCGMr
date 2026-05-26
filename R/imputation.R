#' Run comprehensive imputation benchmark
#'
#' @description
#' Benchmarks target imputation under random, contiguous block, or
#' gap-distribution block masking. The workflow imputes the masked target with
#' MICE, recomputes lag features from the completed target series, then evaluates
#' MICE-only, Random Forest, kNN, XGBoost, LightGBM, and ARIMA predictions on the
#' full target series.
#'
#' @param data A data.frame, an object coercible to data.frame, or a path to a
#'   CSV file.
#' @param target_col Single character string: target column to mask and impute.
#' @param feature_cols Character vector of base feature columns excluding
#'   `target_col`. If `NULL`, all columns except `target_col`, `time_col`, and
#'   generated time features are used.
#' @param id_col Character string: subject identifier column used for lag
#'   features.
#' @param time_col Character string: raw timestamp column to convert into
#'   `TimeSeries`.
#' @param time_format Advanced character string passed to
#'   `CGManalyzer::timeSeqConversion.fn()`. The default automatically handles
#'   common timestamp inputs, so most users only need to provide `time_col`.
#' @param time_unit Character string passed to
#'   `CGManalyzer::timeSeqConversion.fn()`. Use `"minute"` or `"second"`.
#' @param mask_rates Numeric vector in (0, 1): target-row masking rates.
#' @param mask_type One of `"random"`, `"block"`, or `"gap_block"`.
#' @param models Character vector of models to return. Use `"mice_only"`,
#'   `"rf"`, `"knn"`, `"xgboost"`, `"lightgbm"`, `"arima"`, or `"all"`.
#'   MICE is always run internally because the other models depend on the
#'   MICE-completed target.
#' @param rf_n_estimators Integer: number of random forest trees.
#'   Only used when `models` includes `"rf"` or `"all"`.
#' @param knn_k Integer: number of kNN neighbors. Only used when `models`
#'   includes `"knn"` or `"all"`.
#' @param xgb_nrounds Integer: number of XGBoost boosting rounds. Only used
#'   when `models` includes `"xgboost"` or `"all"`.
#' @param lgb_nrounds Integer: number of LightGBM boosting rounds. Only used
#'   when `models` includes `"lightgbm"` or `"all"`.
#' @param arima_order Integer vector of length 3 for `forecast::Arima()`. Only
#'   used when `models` includes `"arima"` or `"all"`.
#' @param seed Integer seed for masking, MICE, and model reproducibility.
#' @param lag_k Integer vector of target lags to compute.
#' @param add_rollmean Logical: add rolling mean of prior target values.
#' @param roll_window Integer rolling mean window.
#' @param gap_bins List of length-2 vectors defining gap-block size bins.
#' @param gap_probs Numeric probabilities for `gap_bins`.
#' @param open_cap Numeric cap used for the open-ended gap bin.
#'
#' @return A list containing `results`, a data.frame with columns
#'   `MaskRate`, `MaskType`, `Method`, `MAPE`, `R2`, `MRD`, and `MaskedCount`;
#'   and `imputed_data`, a named list of model-specific completed data.frames.
#'
#' @details
#' The returned `imputed_data` object is a named list with one data.frame per
#' selected model. Each data.frame stacks rows across mask rates. For unmasked
#' rows, `ImputedValue` equals `ObservedValue`; for masked rows, `ImputedValue`
#' is the method-specific imputed or predicted target value.
#'
#' `MRD` follows the comprehensive script convention:
#' `sum(abs(true - pred) / abs(true)) / length(true)`, with zero true values
#' excluded from the numerator but retained in the denominator. `MAPE` is
#' `MRD * 100`. `R2` is computed on the same full-length prediction vector.
#'
#' @importFrom FNN knn.reg
#' @importFrom ranger ranger
#' @importFrom mice mice complete
#' @importFrom data.table as.data.table setorderv shift frollmean
#' @importFrom stats complete.cases median predict
#' @importFrom CGManalyzer timeSeqConversion.fn
#'
#' @keywords internal
#' @noRd
run_comprehensive_imputation_benchmark <- function(
  data,
  target_col,
  feature_cols = NULL,
  id_col = "USUBJID",
  time_col = "Time",
  time_format = "yyyy:mm:dd:hh:nn",
  time_unit = "minute",
  mask_rates = c(0.05, 0.10, 0.20, 0.30, 0.40),
  mask_type = c("random", "block", "gap_block"),
  models = "mice_only",
  rf_n_estimators = 200,
  knn_k = 7,
  xgb_nrounds = 300,
  lgb_nrounds = 400,
  arima_order = c(4L, 1L, 0L),
  seed = 42,
  lag_k = c(1, 2, 3),
  add_rollmean = TRUE,
  roll_window = 3,
  gap_bins = list(c(1, 5), c(6, 35), c(36, NA)),
  gap_probs = c(0.5923, 0.2569, 0.1509),
  open_cap = 0.50
) {
  mask_type <- match.arg(mask_type)
  selected_models <- .cgmd_normalize_models(models)

  if (is.character(data) && length(data) == 1L && file.exists(data)) {
    df <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else {
    df <- as.data.frame(data)
  }
  df$.RowID <- seq_len(nrow(df))

  if (!is.character(target_col) || length(target_col) != 1L) {
    stop("target_col must be a single character string.")
  }
  if (!is.character(id_col) || length(id_col) != 1L) {
    stop("id_col must be a single character string.")
  }
  if (!is.character(time_col) || length(time_col) != 1L) {
    stop("time_col must be a single character string.")
  }
  if (!is.character(time_format) || length(time_format) != 1L) {
    stop("time_format must be a single character string.")
  }
  if (!is.character(time_unit) || length(time_unit) != 1L) {
    stop("time_unit must be a single character string.")
  }
  if (!time_unit %in% c("minute", "second")) {
    stop("time_unit must be either 'minute' or 'second'.")
  }

  time_series_col <- "TimeSeries"
  time_diff_col <- "TimeDifferenceMinutes"
  generated_time_cols <- c(time_series_col, time_diff_col)

  if (is.null(feature_cols)) {
    feature_cols <- setdiff(
      names(df),
      c(target_col, time_col, generated_time_cols, ".RowID")
    )
  } else {
    feature_cols <- setdiff(
      unique(feature_cols),
      c(target_col, time_col, generated_time_cols, ".RowID")
    )
  }

  needed_cols <- unique(c(target_col, feature_cols, id_col, time_col))
  missing_cols <- setdiff(needed_cols, names(df))
  if (length(missing_cols) > 0L) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  df <- .cgmd_add_time_features(
    df = df,
    raw_time_col = time_col,
    id_col = id_col,
    time_format = time_format,
    time_unit = time_unit,
    time_series_col = time_series_col,
    time_diff_col = time_diff_col
  )
  feature_cols <- unique(c(feature_cols, generated_time_cols))

  coerce_numeric_strict <- function(x, nm) {
    if (is.numeric(x) || is.integer(x)) {
      return(as.double(x))
    }
    if (is.factor(x)) {
      x <- as.character(x)
    }
    if (is.character(x)) {
      num <- suppressWarnings(as.numeric(x))
      bad <- is.na(num) & !is.na(x) & nzchar(x)
      if (any(bad)) {
        stop(
          "Column '",
          nm,
          "' contains non-numeric values; recode it before benchmarking."
        )
      }
      return(num)
    }
    stop("Column '", nm, "' has unsupported type for numeric coercion.")
  }

  numeric_needed_cols <- unique(c(target_col, feature_cols))
  for (nm in numeric_needed_cols) {
    df[[nm]] <- coerce_numeric_strict(df[[nm]], nm)
  }

  complete_needed_cols <- unique(c(numeric_needed_cols, id_col))
  df <- df[
    stats::complete.cases(df[, complete_needed_cols, drop = FALSE]),
    ,
    drop = FALSE
  ]
  if (nrow(df) < 10L) {
    stop("Not enough complete rows after baseline cleaning.")
  }

  if (is.factor(mask_rates)) {
    mask_rates <- as.character(mask_rates)
  }
  if (is.character(mask_rates)) {
    mask_rates <- suppressWarnings(as.numeric(mask_rates))
  }
  if (!is.numeric(mask_rates) || any(!is.finite(mask_rates))) {
    stop("mask_rates must be a numeric vector in (0,1).")
  }
  if (any(mask_rates <= 0 | mask_rates >= 1)) {
    stop("mask_rates must be in (0,1).")
  }

  if (length(arima_order) != 3L || any(!is.finite(arima_order))) {
    stop("arima_order must be a finite numeric vector of length 3.")
  }
  arima_order <- as.integer(arima_order)

  df_base <- .cgmd_sort_by_id_time(df, id_col, time_series_col)
  y_true_full <- as.numeric(df_base[[target_col]])
  n_total <- nrow(df_base)

  eng_cols <- paste0(target_col, "_lag", lag_k)
  if (isTRUE(add_rollmean)) {
    eng_cols <- c(eng_cols, paste0(target_col, "_rollmean_", roll_window))
  }
  model_feature_cols <- unique(c(feature_cols, eng_cols))
  model_base_cols <- unique(c(
    id_col,
    time_series_col,
    target_col,
    feature_cols
  ))

  all_rows <- list()
  imputed_rows <- .cgmd_empty_model_rows(selected_models)
  ml_models <- c("rf", "knn", "xgboost", "lightgbm")
  needs_ml_models <- any(ml_models %in% selected_models)

  for (rate in mask_rates) {
    rate_label <- paste0(as.integer(rate * 100), "%")
    mask_seed <- seed + as.integer(rate * 100)
    mask_pos <- .cgmd_make_mask_pos(
      n = n_total,
      rate = rate,
      mask_type = mask_type,
      seed = mask_seed,
      gap_bins = gap_bins,
      gap_probs = gap_probs,
      open_cap = open_cap
    )
    test_idx <- which(mask_pos)
    train_idx <- which(!mask_pos)

    df_after <- df_base[, model_base_cols, drop = FALSE]
    df_after[[target_col]][test_idx] <- NA_real_

    imp_df <- df_after[, unique(c(target_col, feature_cols)), drop = FALSE]
    mice_method <- mice::make.method(imp_df)
    mice_method[] <- ""
    mice_method[target_col] <- "norm"

    pred_matrix <- mice::make.predictorMatrix(imp_df)
    pred_matrix[,] <- 0
    pred_matrix[target_col, setdiff(colnames(imp_df), target_col)] <- 1

    set.seed(seed)
    imp_obj <- mice::mice(
      imp_df,
      m = 1,
      maxit = 10,
      method = mice_method,
      predictorMatrix = pred_matrix,
      ridge = 1e-5,
      printFlag = FALSE,
      seed = seed
    )
    imp_mat <- mice::complete(imp_obj, 1)
    y_imp <- as.numeric(imp_mat[[target_col]])
    if (any(!is.finite(y_imp))) {
      stop("MICE returned non-finite values for ", target_col, ".")
    }

    df_model <- df_after
    df_model[[target_col]] <- y_imp
    df_model <- .cgmd_compute_lag_features(
      df = df_model,
      target_col = target_col,
      id_col = id_col,
      time_col = time_series_col,
      lag_k = lag_k,
      add_rollmean = add_rollmean,
      roll_window = roll_window
    )

    if (needs_ml_models) {
      X_imp <- as.matrix(df_model[, model_feature_cols, drop = FALSE])
      storage.mode(X_imp) <- "double"
      X_train <- X_imp[train_idx, , drop = FALSE]
      y_train <- y_true_full[train_idx]
      X_test <- X_imp[test_idx, , drop = FALSE]

      filled <- .cgmd_fill_missing_with_train_medians(
        train_mat = X_train,
        test_mat = X_test,
        cols = eng_cols
      )
      X_train <- filled$train
      X_test <- filled$test

      .cgmd_assert_all_finite_matrix(X_train, "X_train")
      .cgmd_assert_all_finite_matrix(X_test, "X_test")
    }

    p_mice <- y_true_full
    p_mice[test_idx] <- y_imp[test_idx]
    mice_method_label <- paste0(
      "MICE-only (base features; impute ",
      target_col,
      ")"
    )
    if ("mice_only" %in% selected_models) {
      all_rows[[length(all_rows) + 1L]] <- .cgmd_metric_row(
        rate = rate,
        rate_label = rate_label,
        mask_type = mask_type,
        method = mice_method_label,
        y_true = y_true_full,
        y_pred = p_mice,
        masked_count = length(test_idx)
      )
      imputed_rows$mice_only[[length(imputed_rows$mice_only) + 1L]] <-
        .cgmd_imputed_data_rows(
          rate = rate,
          rate_label = rate_label,
          mask_type = mask_type,
          method = mice_method_label,
          df_base = df_base,
          df_model = df_model,
          target_col = target_col,
          mask_pos = mask_pos,
          y_pred = p_mice
        )
    }

    if ("rf" %in% selected_models) {
      rf_model <- ranger::ranger(
        x = X_train,
        y = y_train,
        num.trees = rf_n_estimators,
        mtry = ncol(X_train),
        min.node.size = 1,
        replace = TRUE,
        sample.fraction = 1.0,
        seed = seed,
        num.threads = 1
      )
      y_rf <- stats::predict(rf_model, data = X_test)$predictions
      p_rf <- y_true_full
      p_rf[test_idx] <- y_rf
      rf_method_label <- "MICE + RF (engineered lag features)"
      all_rows[[length(all_rows) + 1L]] <- .cgmd_metric_row(
        rate = rate,
        rate_label = rate_label,
        mask_type = mask_type,
        method = rf_method_label,
        y_true = y_true_full,
        y_pred = p_rf,
        masked_count = length(test_idx)
      )
      imputed_rows$rf[[length(imputed_rows$rf) + 1L]] <-
        .cgmd_imputed_data_rows(
          rate = rate,
          rate_label = rate_label,
          mask_type = mask_type,
          method = rf_method_label,
          df_base = df_base,
          df_model = df_model,
          target_col = target_col,
          mask_pos = mask_pos,
          y_pred = p_rf
        )
    }

    if ("knn" %in% selected_models) {
      scaler <- .cgmd_fit_scaler(X_train)
      X_train_sc <- .cgmd_transform_scaler(X_train, scaler)
      X_test_sc <- .cgmd_transform_scaler(X_test, scaler)
      .cgmd_assert_all_finite_matrix(X_train_sc, "X_train_sc")
      .cgmd_assert_all_finite_matrix(X_test_sc, "X_test_sc")

      y_knn <- FNN::knn.reg(
        train = X_train_sc,
        test = X_test_sc,
        y = y_train,
        k = knn_k
      )$pred
      p_knn <- y_true_full
      p_knn[test_idx] <- y_knn
      knn_method_label <- "MICE + KNN (engineered lag features)"
      all_rows[[length(all_rows) + 1L]] <- .cgmd_metric_row(
        rate = rate,
        rate_label = rate_label,
        mask_type = mask_type,
        method = knn_method_label,
        y_true = y_true_full,
        y_pred = p_knn,
        masked_count = length(test_idx)
      )
      imputed_rows$knn[[length(imputed_rows$knn) + 1L]] <-
        .cgmd_imputed_data_rows(
          rate = rate,
          rate_label = rate_label,
          mask_type = mask_type,
          method = knn_method_label,
          df_base = df_base,
          df_model = df_model,
          target_col = target_col,
          mask_pos = mask_pos,
          y_pred = p_knn
        )
    }

    if ("xgboost" %in% selected_models) {
      dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
      xgb_model <- xgboost::xgb.train(
        params = list(
          objective = "reg:squarederror",
          eta = 0.05,
          max_depth = 6,
          subsample = 0.8,
          colsample_bytree = 0.8,
          lambda = 1.0,
          eval_metric = "rmse",
          nthread = -1,
          seed = seed
        ),
        data = dtrain,
        nrounds = xgb_nrounds,
        verbose = 0
      )
      y_xgb <- stats::predict(
        xgb_model,
        xgboost::xgb.DMatrix(data = X_test)
      )
      p_xgb <- y_true_full
      p_xgb[test_idx] <- y_xgb
      xgb_method_label <- "MICE + XGBoost (engineered lag features)"
      all_rows[[length(all_rows) + 1L]] <- .cgmd_metric_row(
        rate = rate,
        rate_label = rate_label,
        mask_type = mask_type,
        method = xgb_method_label,
        y_true = y_true_full,
        y_pred = p_xgb,
        masked_count = length(test_idx)
      )
      imputed_rows$xgboost[[length(imputed_rows$xgboost) + 1L]] <-
        .cgmd_imputed_data_rows(
          rate = rate,
          rate_label = rate_label,
          mask_type = mask_type,
          method = xgb_method_label,
          df_base = df_base,
          df_model = df_model,
          target_col = target_col,
          mask_pos = mask_pos,
          y_pred = p_xgb
        )
    }

    if ("lightgbm" %in% selected_models) {
      lgb_train <- lightgbm::lgb.Dataset(data = X_train, label = y_train)
      lgb_model <- lightgbm::lgb.train(
        params = list(
          objective = "regression",
          learning_rate = 0.05,
          num_leaves = 31L,
          bagging_fraction = 0.8,
          feature_fraction = 0.8,
          seed = seed,
          verbose = -1
        ),
        data = lgb_train,
        nrounds = lgb_nrounds
      )
      y_lgb <- stats::predict(lgb_model, X_test)
      p_lgb <- y_true_full
      p_lgb[test_idx] <- y_lgb
      lgb_method_label <- "MICE + LightGBM (engineered lag features)"
      all_rows[[length(all_rows) + 1L]] <- .cgmd_metric_row(
        rate = rate,
        rate_label = rate_label,
        mask_type = mask_type,
        method = lgb_method_label,
        y_true = y_true_full,
        y_pred = p_lgb,
        masked_count = length(test_idx)
      )
      imputed_rows$lightgbm[[length(imputed_rows$lightgbm) + 1L]] <-
        .cgmd_imputed_data_rows(
          rate = rate,
          rate_label = rate_label,
          mask_type = mask_type,
          method = lgb_method_label,
          df_base = df_base,
          df_model = df_model,
          target_col = target_col,
          mask_pos = mask_pos,
          y_pred = p_lgb
        )
    }

    if ("arima" %in% selected_models) {
      arima_model <- forecast::Arima(y_imp, order = arima_order)
      y_arima <- as.numeric(
        forecast::forecast(arima_model, h = length(test_idx))$mean
      )
      p_arima <- y_true_full
      p_arima[test_idx] <- y_arima
      arima_method_label <- paste0(
        "ARIMA(",
        paste(arima_order, collapse = ","),
        ") on MICE-completed target"
      )
      all_rows[[length(all_rows) + 1L]] <- .cgmd_metric_row(
        rate = rate,
        rate_label = rate_label,
        mask_type = mask_type,
        method = arima_method_label,
        y_true = y_true_full,
        y_pred = p_arima,
        masked_count = length(test_idx)
      )
      imputed_rows$arima[[length(imputed_rows$arima) + 1L]] <-
        .cgmd_imputed_data_rows(
          rate = rate,
          rate_label = rate_label,
          mask_type = mask_type,
          method = arima_method_label,
          df_base = df_base,
          df_model = df_model,
          target_col = target_col,
          mask_pos = mask_pos,
          y_pred = p_arima
        )
    }
  }

  results <- do.call(rbind, all_rows)
  results <- results[order(results$MaskRateNum, results$Method), ]
  results$MaskRateNum <- NULL
  rownames(results) <- NULL

  imputed_data <- lapply(imputed_rows, .cgmd_bind_imputed_model_rows)

  list(results = results, imputed_data = imputed_data)
}


#' Impute missing glucose values using Mice and ARIMA/XGBoost
#'
#' @description
#' Imputes missing glucose values in continuous glucose monitoring (CGM) data.
#' The function handles both explicit missing glucose values already coded as
#' `NA` and implicit missing readings caused by timestamp gaps. Before
#' imputation, each subject is regularized to an equal `interval_minutes`
#' timestamp grid; missing timestamp gaps are converted into explicit rows with
#' `target_col = NA`, then imputed using the selected backend.
#'
#' @param data A data.frame, an object coercible to data.frame, or a path to a
#'   CSV file.
#' @param target_col Single character string: target glucose column with
#'   missing values to impute. Python default name is `"glucose_value"`.
#' @param feature_cols Optional character vector of base feature columns. If
#'   `NULL`, the Python pipeline feature set is used when available:
#'   `TimeSeries`, `TimeDifferenceMinutes`, `id_col`, `AGE`, `SEX`, `HBA1C`,
#'   `lag1`, `lag2`, `lag3`, and `rollmean`. If supplied, the listed columns are
#'   used together with the generated time, subject, lag, and rolling-mean
#'   columns that exist in the data.
#' @param id_col Character string: subject identifier column. Python default
#'   name is `"subjectid"`.
#' @param time_col Character string: raw timestamp column. Python default name
#'   is `"timestamp"`.
#' @param time_format Retained for compatibility with the old R function. The
#'   Python-engine path uses pandas timestamp parsing.
#' @param time_unit Retained for compatibility with the old R function and not
#'   used by the strict Python-engine path.
#' @param models Real-imputation method selector. Use `NULL` or `"auto"` to
#'   keep the default missing-rate rule: `MICE+ARIMA` when the target missing
#'   rate is less than or equal to `use_arima_if_missing_leq`, otherwise
#'   `MICE+XGBoost`. Use one of `"arima"`, `"xgboost"`, `"rf"`, `"knn"`, or
#'   `"lightgbm"` to force a specific method regardless of missing rate.
#' @param rf_n_estimators Integer: number of Random Forest trees. Only used
#'   when `models = "rf"`.
#' @param knn_k Integer: number of nearest neighbors. Only used when
#'   `models = "knn"`.
#' @param lgb_nrounds Integer: number of LightGBM boosting rounds. Only used
#'   when `models = "lightgbm"`.
#' @param xgb_nrounds Integer: number of XGBoost boosting rounds. Python's
#'   `n_estimators` default is 300.
#' @param arima_order Integer vector of length 3. Python default is
#'   `c(4L, 1L, 0L)`.
#' @param seed Integer seed for scikit-learn and XGBoost. Python default is 42.
#' @param lag_k Integer vector of target lags to compute. Python default is
#'   `c(1L, 2L, 3L)`.
#' @param add_rollmean Logical: add rolling mean of prior target values. Python
#'   always adds this; setting `FALSE` is allowed only for compatibility.
#' @param roll_window Integer rolling mean window. Python default is 3.
#' @param interval_minutes Expected spacing, in minutes, between consecutive CGM
#'   readings. The default is `5`. The function uses this value to regularize
#'   each subject's timestamps to an equal-interval grid before imputation.
#' @param missing_warning_threshold Numeric value between 0 and 1. If the
#'   missingness rate in `target_col` after timestamp-gap regularization exceeds
#'   this threshold, a warning is issued. Default is `0.20`.
#' @param study_start Optional study start timestamp. If supplied, the function
#'   reports subjects whose first observed CGM timestamp occurs after this time.
#'   Leading study time is not imputed.
#' @param study_end Optional study end timestamp. If supplied, the function
#'   reports subjects whose last observed CGM timestamp occurs before this time.
#'   Trailing study time is not imputed.
#' @param use_arima_if_missing_leq Numeric missing-rate threshold. If the target
#'   missing rate is less than or equal to this value, segmentwise ARIMA is used;
#'   otherwise XGBoost is used. Python default is 0.05.
#' @param arima_min_history Minimum number of prior observations required before
#'   fitting ARIMA for a missing segment. Python default is 20.
#' @param imputer_backend One of `"mice"` or `"sklearn"`. `"mice"` uses the
#'   R package `mice` as the CRAN-safe R-native fallback. `"sklearn"` uses
#'   Python modules through `reticulate` for the full strict workflow and gives
#'   the closest agreement with the Python package.
#' @param prefer_cgmanalyzer_equal_interval Retained for compatibility. The
#'   Python-engine path uses pandas elapsed-minute construction unless an
#'   existing non-empty `TimeSeries` column is supplied.
#' @param export Logical; if `TRUE`, writes the returned imputed data frame to a
#'   timestamped CSV file in the current working directory. Default is `FALSE`.
#'
#' @return A data.frame containing the original user-supplied columns plus
#'   `imputed_glucose_value`, the completed glucose column. The original target
#'   column is left unchanged, so values that were originally missing or created
#'   from timestamp gaps remain `NA` in `target_col`, while their completed
#'   values are stored in `imputed_glucose_value`.
#'
#' @details
#' The imputation workflow first parses and sorts timestamps within each subject.
#' Each subject is regularized to an equal `interval_minutes` grid. If a reading
#' is missing because the timestamp is absent from the input data, a new row is
#' inserted and the target glucose value is set to `NA`. These inserted missing
#' values are then imputed using the same workflow as explicit `NA` values.
#'
#' Internally, the function creates time features, lag features, and rolling-mean
#' features to support imputation. These engineered columns are used only during
#' model fitting and are removed from the returned data frame.
#'
#' `imputed_glucose_value` is returned as a continuous numeric model estimate.
#' Users who require whole-number glucose values for reporting can round this
#' column after imputation.
#'
#' Missingness warnings are based on the data after timestamp-gap
#' regularization, so both explicit `NA` glucose values and rows created from
#' timestamp gaps contribute to the reported missingness rate. The function also
#' warns when long contiguous missing blocks of at least 12 or 24 hours are
#' detected. If `study_start` or `study_end` is supplied, leading or trailing
#' study-period coverage gaps are reported but are not imputed.
#' @examples
#' data("CGMExmplDat10Pct")
#' out <- run_missing_glucose_imputation(
#'   CGMExmplDat10Pct,
#'   target_col = "LBORRES",
#'   feature_cols = c("AGE", "hba1c"),
#'   id_col = "USUBJID",
#'   time_col = "Time",
#'   imputer_backend = "mice"
#' )
#' head(subset(out, is.na(LBORRES)))
#'
#' @importFrom utils read.csv write.csv
#' @importFrom CGManalyzer equalInterval.fn
#' @export
run_missing_glucose_imputation <- function(
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
  missing_warning_threshold = 0.20,
  study_start = NULL,
  study_end = NULL,
  use_arima_if_missing_leq = 0.05,
  arima_min_history = 20L,
  imputer_backend = c("mice", "sklearn"),
  prefer_cgmanalyzer_equal_interval = FALSE,
  export = FALSE
) {
  imputer_backend <- match.arg(imputer_backend)

  if (!is.character(target_col) || length(target_col) != 1L) {
    stop("target_col must be a single character string.")
  }
  if (!is.character(id_col) || length(id_col) != 1L) {
    stop("id_col must be a single character string.")
  }
  if (!is.character(time_col) || length(time_col) != 1L) {
    stop("time_col must be a single character string.")
  }
  if (length(arima_order) != 3L || any(!is.finite(arima_order))) {
    stop("arima_order must be a finite numeric vector of length 3.")
  }
  if (
    !is.numeric(missing_warning_threshold) ||
      length(missing_warning_threshold) != 1L ||
      is.na(missing_warning_threshold) ||
      missing_warning_threshold < 0 ||
      missing_warning_threshold > 1
  ) {
    stop(
      "missing_warning_threshold must be a single numeric value between 0 and 1.",
      call. = FALSE
    )
  }
  arima_order <- as.integer(arima_order)
  lag_k <- as.integer(lag_k)
  real_imputation_model <- .cgmd_normalize_real_imputation_model(models)

  if (!is.null(feature_cols)) {
    if (is.factor(feature_cols)) {
      feature_cols <- as.character(feature_cols)
    }
    if (!is.character(feature_cols)) {
      stop("feature_cols must be NULL or a character vector.")
    }
  }

  ignored <- character(0)
  if (!identical(time_format, "yyyy:mm:dd:hh:nn")) {
    ignored <- c(ignored, "time_format")
  }
  if (!identical(time_unit, "minute")) {
    ignored <- c(ignored, "time_unit")
  }
  if (length(ignored) > 0L) {
    warning(
      "Strict Python-port mode ignores these old R-only controls: ",
      paste(unique(ignored), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  out <- .cgmd_py_read_data(data)
  original_output_cols <- names(out)

  if (!target_col %in% names(out)) {
    stop("Missing required target_col: ", target_col)
  }
  if (!id_col %in% names(out)) {
    stop("Missing required id_col: ", id_col)
  }
  if (!time_col %in% names(out)) {
    stop("Missing required time_col: ", time_col)
  }

  out <- .cgmd_py_regularize_timestamp_gaps(
    df = out,
    id_col = id_col,
    time_col = time_col,
    target_col = target_col,
    interval_minutes = interval_minutes
  )

  missingness_diagnostics <- .cgmd_py_warn_missingness(
    df = out,
    id_col = id_col,
    time_col = time_col,
    target_col = target_col,
    interval_minutes = interval_minutes,
    missing_warning_threshold = missing_warning_threshold,
    study_start = study_start,
    study_end = study_end
  )

  if (identical(imputer_backend, "sklearn")) {
    result <- .cgmd_py_run_python_engine(
      df = out,
      timestamp_col = time_col,
      subjectid_col = id_col,
      glucose_col = target_col,
      feature_cols = feature_cols,
      interval_minutes = interval_minutes,
      use_arima_if_missing_leq = use_arima_if_missing_leq,
      seed = seed,
      lag_k = lag_k,
      roll_window = roll_window,
      add_rollmean = add_rollmean,
      arima_order = arima_order,
      arima_min_history = arima_min_history,
      xgb_nrounds = xgb_nrounds,
      rf_n_estimators = rf_n_estimators,
      knn_k = knn_k,
      lgb_nrounds = lgb_nrounds,
      models = real_imputation_model,
      drop_internal_cols = TRUE
    )
    result <- .cgmd_py_keep_user_output_cols(
      df = result,
      original_cols = original_output_cols
    )

    result <- .cgmd_py_export_if_requested(result, export)
    return(result)
  }

  out <- .cgmd_py_add_timeseries_column(
    df = out,
    ts_col = time_col,
    id_col = id_col,
    interval_minutes = interval_minutes,
    prefer_cgmanalyzer = prefer_cgmanalyzer_equal_interval
  )

  out <- .cgmd_py_encode_sex(out, "SEX")

  numeric_cols <- c(
    target_col,
    "TimeSeries",
    "TimeDifferenceMinutes",
    id_col,
    "AGE",
    "HBA1C",
    "SEX"
  )
  for (nm in intersect(numeric_cols, names(out))) {
    out[[nm]] <- .cgmd_py_to_numeric(out[[nm]])
  }

  out <- .cgmd_py_add_lag_features(
    df = out,
    target_col = target_col,
    id_col = id_col,
    time_col = "TimeSeries",
    lag_k = lag_k,
    roll_window = roll_window,
    add_rollmean = add_rollmean
  )

  lag_cols <- paste0("lag", lag_k)
  roll_cols <- if (isTRUE(add_rollmean)) "rollmean" else character(0)

  if (is.null(feature_cols)) {
    selected_feature_cols <- c(
      "TimeSeries",
      "TimeDifferenceMinutes",
      id_col,
      "AGE",
      "SEX",
      "HBA1C",
      lag_cols,
      roll_cols
    )
  } else {
    selected_feature_cols <- c(
      feature_cols,
      "TimeSeries",
      "TimeDifferenceMinutes",
      id_col,
      lag_cols,
      roll_cols
    )
  }
  selected_feature_cols <- unique(setdiff(selected_feature_cols, target_col))
  selected_feature_cols <- intersect(selected_feature_cols, names(out))

  result <- .cgmd_py_impute_values(
    df = out,
    id_col = id_col,
    time_col = "TimeSeries",
    target_col = target_col,
    feature_cols = selected_feature_cols,
    use_arima_if_missing_leq = use_arima_if_missing_leq,
    seed = seed,
    imputer_backend = "mice",
    arima_order = arima_order,
    arima_min_history = arima_min_history,
    xgb_nrounds = xgb_nrounds,
    rf_n_estimators = rf_n_estimators,
    knn_k = knn_k,
    lgb_nrounds = lgb_nrounds,
    models = real_imputation_model
  )
  result <- .cgmd_py_keep_user_output_cols(
    df = result,
    original_cols = original_output_cols
  )

  result <- .cgmd_py_export_if_requested(result, export)
  result
}

.cgmd_py_export_if_requested <- function(out, export = FALSE) {
  if (isTRUE(export)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("imputed_cgm_data_", timestamp, ".csv")
    if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::fwrite(out, file = filename)
    } else {
      utils::write.csv(out, file = filename, row.names = FALSE)
    }
    message("Imputed data has been exported to: ", filename)
  }
  out
}

.cgmd_py_ensure_python_engine <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "imputer_backend = 'sklearn' requires the optional R package 'reticulate'. ",
      "Install it with install.packages('reticulate'), or use imputer_backend = 'mice'.",
      call. = FALSE
    )
  }

  required_py_packages <- c(
    "numpy",
    "pandas",
    "scikit-learn",
    "statsmodels",
    "xgboost"
  )

  if ("py_require" %in% getNamespaceExports("reticulate")) {
    reticulate::py_require(required_py_packages)
  }

  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "imputer_backend = 'sklearn' requires an available Python installation. ",
      "Use imputer_backend = 'mice' for an R-native fallback, or configure Python with reticulate.",
      call. = FALSE
    )
  }

  missing_modules <- character(0)
  for (mod in c("numpy", "pandas", "sklearn", "statsmodels", "xgboost")) {
    if (!reticulate::py_module_available(mod)) {
      missing_modules <- c(missing_modules, mod)
    }
  }

  if (length(missing_modules) > 0L) {
    stop(
      "The sklearn Python engine requires Python modules: numpy, pandas, scikit-learn, statsmodels, xgboost. ",
      "Missing import names: ",
      paste(missing_modules, collapse = ", "),
      ". ",
      "Install them with reticulate::py_install(c('numpy', 'pandas', 'scikit-learn', 'statsmodels', 'xgboost'), pip = TRUE), ",
      "then restart R.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.cgmd_py_define_python_engine <- function() {
  .cgmd_py_ensure_python_engine()
  code <- paste(
    c(
      "import numpy as np",
      "import pandas as pd",
      "from sklearn.experimental import enable_iterative_imputer  # noqa: F401",
      "from sklearn.impute import IterativeImputer",
      "from sklearn.ensemble import RandomForestRegressor",
      "from sklearn.neighbors import KNeighborsRegressor",
      "from sklearn.preprocessing import StandardScaler",
      "from statsmodels.tsa.arima.model import ARIMA",
      "import xgboost as xgb",
      "import re",
      "",
      "def _cgmd_encode_sex(df, col='SEX'):",
      "    out = df.copy()",
      "    if col in out.columns:",
      "        s = out[col].astype(str).str.strip().str.upper()",
      "        out[col] = s.map({'M': 1, 'MALE': 1, '1': 1, 'F': 0, 'FEMALE': 0, '0': 0})",
      "    return out",
      "",
      "def _cgmd_add_timeseries_column(df, ts_col='timestamp', id_col='subjectid', interval_minutes=5):",
      "    if ts_col not in df.columns:",
      "        raise ValueError(f\"Column '{ts_col}' was not found.\")",
      "    out = df.copy()",
      "    if 'TimeSeries' in out.columns and out['TimeSeries'].notna().any():",
      "        return out",
      "    raw_ts = out[ts_col].astype(str).str.strip()",
      "    out['_ts_parsed'] = pd.to_datetime(raw_ts, errors='coerce')",
      "",
      "    needs_parse = out['_ts_parsed'].isna()",
      "    if needs_parse.any():",
      "        parsed_alt = pd.to_datetime(",
      "            raw_ts[needs_parse],",
      "            format='%Y:%m:%d:%H:%M',",
      "            errors='coerce'",
      "        )",
      "        out.loc[needs_parse, '_ts_parsed'] = parsed_alt",
      "",
      "    needs_parse = out['_ts_parsed'].isna()",
      "    if needs_parse.any():",
      "        parsed_alt = pd.to_datetime(",
      "            raw_ts[needs_parse],",
      "            format='%Y:%m:%d:%H:%M:%S',",
      "            errors='coerce'",
      "        )",
      "        out.loc[needs_parse, '_ts_parsed'] = parsed_alt",
      "",
      "    if out['_ts_parsed'].isna().any():",
      "        bad_examples = raw_ts[out['_ts_parsed'].isna()].head(5).tolist()",
      "        raise ValueError(",
      "            'Some timestamp values could not be parsed. Examples: ' +",
      "            ', '.join(map(str, bad_examples))",
      "        )",
      "    if id_col in out.columns:",
      "        out = out.sort_values([id_col, '_ts_parsed']).reset_index(drop=True)",
      "        out['TimeSeries'] = out.groupby(id_col)['_ts_parsed'].transform(lambda s: (s - s.min()).dt.total_seconds() / 60.0)",
      "    else:",
      "        out = out.sort_values('_ts_parsed').reset_index(drop=True)",
      "        out['TimeSeries'] = (out['_ts_parsed'] - out['_ts_parsed'].min()).dt.total_seconds() / 60.0",
      "    return out.drop(columns=['_ts_parsed'])",
      "",
      "def _cgmd_add_lag_features(df, target_col='glucose_value', id_col='subjectid', time_col='TimeSeries', lag_k=(1, 2, 3), roll_window=3, add_rollmean=True):",
      "    out = df.sort_values([id_col, time_col]).reset_index(drop=True).copy()",
      "    for k in lag_k:",
      "        out[f'lag{int(k)}'] = out.groupby(id_col)[target_col].shift(int(k))",
      "    if add_rollmean:",
      "        s = out.groupby(id_col)[target_col].shift(1)",
      "        out['rollmean'] = s.groupby(out[id_col]).rolling(int(roll_window)).mean().reset_index(level=0, drop=True)",
      "    return out",
      "",
      "def _cgmd_segments(mask):",
      "    segs = []",
      "    i = 0",
      "    n = len(mask)",
      "    while i < n:",
      "        if bool(mask[i]):",
      "            j = i",
      "            while j + 1 < n and bool(mask[j + 1]):",
      "                j += 1",
      "            segs.append((i, j))",
      "            i = j + 1",
      "        else:",
      "            i += 1",
      "    return segs",
      "",
      "def _cgmd_arima_segmentwise_on_mice(df_sorted, id_col, y_mice_full, mask_pos, order=(4, 1, 0), min_history=20):",
      "    pred_full = np.asarray(y_mice_full, dtype=float).copy()",
      "    mask_pos = np.asarray(mask_pos, dtype=bool)",
      "    for _, grp in df_sorted.groupby(id_col, sort=False):",
      "        idx = grp.index.to_numpy()",
      "        local_mask = mask_pos[idx]",
      "        segs = _cgmd_segments(local_mask)",
      "        if not segs:",
      "            continue",
      "        local_series = pred_full[idx]",
      "        for s, e in segs:",
      "            block_len = e - s + 1",
      "            history = local_series[:s]",
      "            if len(history) < int(min_history):",
      "                continue",
      "            if not np.all(np.isfinite(history)):",
      "                continue",
      "            try:",
      "                fit = ARIMA(history, order=tuple(int(x) for x in order)).fit()",
      "                fcst = fit.forecast(steps=block_len)",
      "                pred_full[idx[s:e+1]] = np.asarray(fcst, dtype=float)",
      "            except Exception:",
      "                continue",
      "    return pred_full",
      "",
      "def _cgmd_fit_xgb_predict_missing(X_train, y_train, X_missing, seed=42, nrounds=300):",
      "    if X_missing.shape[0] == 0:",
      "        return np.asarray([], dtype=float)",
      "    model = xgb.XGBRegressor(",
      "        n_estimators=int(nrounds),",
      "        learning_rate=0.05,",
      "        max_depth=6,",
      "        subsample=0.8,",
      "        colsample_bytree=0.8,",
      "        reg_lambda=1.0,",
      "        n_jobs=-1,",
      "        random_state=int(seed),",
      "        tree_method='hist',",
      "        eval_metric='rmse',",
      "    )",
      "    model.fit(X_train, y_train, verbose=False)",
      "    return model.predict(X_missing)",
      "",
      "def _cgmd_fit_rf_predict_missing(X_train, y_train, X_missing, seed=42, n_estimators=200):",
      "    if X_missing.shape[0] == 0:",
      "        return np.asarray([], dtype=float)",
      "    model = RandomForestRegressor(",
      "        n_estimators=int(n_estimators),",
      "        max_features=1.0,",
      "        min_samples_leaf=1,",
      "        bootstrap=True,",
      "        random_state=int(seed),",
      "        n_jobs=1,",
      "    )",
      "    model.fit(X_train, y_train)",
      "    return model.predict(X_missing)",
      "",
      "def _cgmd_fit_knn_predict_missing(X_train, y_train, X_missing, k=7):",
      "    if X_missing.shape[0] == 0:",
      "        return np.asarray([], dtype=float)",
      "    scaler = StandardScaler()",
      "    X_train_sc = scaler.fit_transform(X_train)",
      "    X_missing_sc = scaler.transform(X_missing)",
      "    n_neighbors = max(1, min(int(k), X_train.shape[0]))",
      "    model = KNeighborsRegressor(n_neighbors=n_neighbors)",
      "    model.fit(X_train_sc, y_train)",
      "    return model.predict(X_missing_sc)",
      "",
      "def _cgmd_fit_lgb_predict_missing(X_train, y_train, X_missing, seed=42, nrounds=400):",
      "    if X_missing.shape[0] == 0:",
      "        return np.asarray([], dtype=float)",
      "    try:",
      "        import lightgbm as lgb",
      "    except Exception as exc:",
      "        raise ImportError(\"models='lightgbm' with imputer_backend='sklearn' requires the Python module lightgbm. Install it with reticulate::py_install('lightgbm', pip = TRUE), then restart R.\") from exc",
      "    model = lgb.LGBMRegressor(",
      "        objective='regression',",
      "        n_estimators=int(nrounds),",
      "        learning_rate=0.05,",
      "        num_leaves=31,",
      "        subsample=0.8,",
      "        colsample_bytree=0.8,",
      "        random_state=int(seed),",
      "        verbosity=-1,",
      "    )",
      "    model.fit(X_train, y_train)",
      "    return model.predict(X_missing)",
      "",
      "def _cgmd_resolve_model(models, miss_rate, use_arima_if_missing_leq):",
      "    key = 'auto' if models is None else str(models).strip().lower()",
      "    if key == 'auto':",
      "        key = 'arima' if miss_rate <= float(use_arima_if_missing_leq) else 'xgboost'",
      "    labels = {",
      "        'arima': 'MICE+ARIMA',",
      "        'xgboost': 'MICE+XGBoost',",
      "        'rf': 'MICE+RF',",
      "        'knn': 'MICE+kNN',",
      "        'lightgbm': 'MICE+LightGBM',",
      "    }",
      "    if key not in labels:",
      "        raise ValueError(f\"Invalid models value for real imputation: {models}\")",
      "    return key, labels[key]",
      "",
      "def _cgmd_drop_internal_cols(df):",
      "    drop_cols = [c for c in df.columns if re.match(r'^lag[0-9]+$', str(c))]",
      "    if 'rollmean' in df.columns:",
      "        drop_cols.append('rollmean')",
      "    return df.drop(columns=list(dict.fromkeys(drop_cols)), errors='ignore')",
      "",
      "def cgmd_r_run_python_engine(r_df, timestamp_col='timestamp', subjectid_col='subjectid', glucose_col='glucose_value', feature_cols=None, interval_minutes=5, use_arima_if_missing_leq=0.05, seed=42, lag_k=(1,2,3), roll_window=3, add_rollmean=True, arima_order=(4,1,0), arima_min_history=20, xgb_nrounds=300, rf_n_estimators=200, knn_k=7, lgb_nrounds=400, models='auto', drop_internal_cols=True):",
      "    out = pd.DataFrame(r_df).copy()",
      "    out = _cgmd_add_timeseries_column(out, ts_col=timestamp_col, id_col=subjectid_col, interval_minutes=interval_minutes)",
      "    out = _cgmd_encode_sex(out, 'SEX')",
      "    numeric_cols = [glucose_col, 'TimeSeries', 'TimeDifferenceMinutes', subjectid_col, 'AGE', 'HBA1C', 'SEX']",
      "    for c in numeric_cols:",
      "        if c in out.columns:",
      "            out[c] = pd.to_numeric(out[c], errors='coerce')",
      "    lag_k = tuple(int(x) for x in lag_k)",
      "    out = _cgmd_add_lag_features(out, target_col=glucose_col, id_col=subjectid_col, time_col='TimeSeries', lag_k=lag_k, roll_window=roll_window, add_rollmean=bool(add_rollmean))",
      "    lag_cols = [f'lag{int(k)}' for k in lag_k]",
      "    roll_cols = ['rollmean'] if bool(add_rollmean) else []",
      "    if feature_cols is None:",
      "        selected_feature_cols = ['TimeSeries', 'TimeDifferenceMinutes', subjectid_col, 'AGE', 'SEX', 'HBA1C'] + lag_cols + roll_cols",
      "    else:",
      "        selected_feature_cols = list(feature_cols) + ['TimeSeries', 'TimeDifferenceMinutes', subjectid_col] + lag_cols + roll_cols",
      "    selected_feature_cols = [c for c in dict.fromkeys(selected_feature_cols) if c in out.columns and c != glucose_col]",
      "    out = out.sort_values([subjectid_col, 'TimeSeries']).reset_index(drop=True).copy()",
      "    mask_pos = out[glucose_col].isna().to_numpy()",
      "    miss_rate = float(mask_pos.mean())",
      "    imp_df = out[[glucose_col] + selected_feature_cols].copy()",
      "    imp_mat = IterativeImputer(random_state=int(seed), max_iter=10).fit_transform(imp_df.to_numpy(dtype=float))",
      "    y_mice_full = imp_mat[:, 0]",
      "    X_imp = imp_mat[:, 1:]",
      "    y_final = np.asarray(y_mice_full, dtype=float).copy()",
      "    model_key, method = _cgmd_resolve_model(models, miss_rate, use_arima_if_missing_leq)",
      "    if model_key == 'arima':",
      "        y_final = _cgmd_arima_segmentwise_on_mice(out, subjectid_col, y_mice_full, mask_pos, order=tuple(int(x) for x in arima_order), min_history=int(arima_min_history))",
      "    else:",
      "        train_idx = ~mask_pos",
      "        X_train = X_imp[train_idx]",
      "        y_train = out.loc[train_idx, glucose_col].to_numpy(dtype=float)",
      "        X_missing = X_imp[mask_pos]",
      "        if X_missing.shape[0] == 0:",
      "            y_pred_missing = np.asarray([], dtype=float)",
      "        elif X_train.shape[1] == 0:",
      "            raise ValueError(f\"{method} requires at least one feature column.\")",
      "        elif model_key == 'xgboost':",
      "            y_pred_missing = _cgmd_fit_xgb_predict_missing(X_train, y_train, X_missing, seed=seed, nrounds=xgb_nrounds)",
      "        elif model_key == 'rf':",
      "            y_pred_missing = _cgmd_fit_rf_predict_missing(X_train, y_train, X_missing, seed=seed, n_estimators=rf_n_estimators)",
      "        elif model_key == 'knn':",
      "            y_pred_missing = _cgmd_fit_knn_predict_missing(X_train, y_train, X_missing, k=knn_k)",
      "        elif model_key == 'lightgbm':",
      "            y_pred_missing = _cgmd_fit_lgb_predict_missing(X_train, y_train, X_missing, seed=seed, nrounds=lgb_nrounds)",
      "        else:",
      "            raise ValueError(f\"Unsupported real-imputation model: {model_key}\")",
      "        y_final[mask_pos] = y_pred_missing",
      "    out['imputed_glucose_value'] = y_final",
      "    out['imputation_method'] = method",
      "    out['missing_rate'] = miss_rate",
      "    if drop_internal_cols:",
      "        out = _cgmd_drop_internal_cols(out)",
      "    return out",
      ""
    ),
    collapse = "\n"
  )
  reticulate::py_run_string(code)
  invisible(TRUE)
}

.cgmd_py_run_python_engine <- function(
  df,
  timestamp_col,
  subjectid_col,
  glucose_col,
  feature_cols = NULL,
  interval_minutes = 5L,
  use_arima_if_missing_leq = 0.05,
  seed = 42L,
  lag_k = c(1L, 2L, 3L),
  roll_window = 3L,
  add_rollmean = TRUE,
  arima_order = c(4L, 1L, 0L),
  arima_min_history = 20L,
  xgb_nrounds = 300L,
  rf_n_estimators = 200L,
  knn_k = 7L,
  lgb_nrounds = 400L,
  models = "auto",
  drop_internal_cols = TRUE
) {
  .cgmd_py_define_python_engine()

  py_df <- reticulate::r_to_py(as.data.frame(df, stringsAsFactors = FALSE))
  py_feature_cols <- if (is.null(feature_cols)) NULL else as.list(feature_cols)

  ans <- reticulate::py$cgmd_r_run_python_engine(
    py_df,
    timestamp_col = timestamp_col,
    subjectid_col = subjectid_col,
    glucose_col = glucose_col,
    feature_cols = py_feature_cols,
    interval_minutes = as.integer(interval_minutes),
    use_arima_if_missing_leq = as.numeric(use_arima_if_missing_leq),
    seed = as.integer(seed),
    lag_k = as.list(as.integer(lag_k)),
    roll_window = as.integer(roll_window),
    add_rollmean = isTRUE(add_rollmean),
    arima_order = as.list(as.integer(arima_order)),
    arima_min_history = as.integer(arima_min_history),
    xgb_nrounds = as.integer(xgb_nrounds),
    rf_n_estimators = as.integer(rf_n_estimators),
    knn_k = as.integer(knn_k),
    lgb_nrounds = as.integer(lgb_nrounds),
    models = models,
    drop_internal_cols = isTRUE(drop_internal_cols)
  )

  out <- reticulate::py_to_r(ans)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

.cgmd_py_read_data <- function(data) {
  if (is.character(data) && length(data) == 1L && file.exists(data)) {
    utils::read.csv(data, stringsAsFactors = FALSE)
  } else {
    as.data.frame(data, stringsAsFactors = FALSE)
  }
}


.cgmd_py_regularize_timestamp_gaps <- function(
  df,
  id_col,
  time_col,
  target_col,
  interval_minutes = 5L
) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)

  if (!id_col %in% names(out)) {
    stop("Missing required id_col: ", id_col, call. = FALSE)
  }
  if (!time_col %in% names(out)) {
    stop("Missing required time_col: ", time_col, call. = FALSE)
  }
  if (!target_col %in% names(out)) {
    stop("Missing required target_col: ", target_col, call. = FALSE)
  }

  parsed <- .cgmd_py_parse_timestamp(out[[time_col]])
  if (any(is.na(parsed))) {
    stop("Some timestamp values could not be parsed.", call. = FALSE)
  }

  out[[time_col]] <- parsed
  out[["inserted_timestamp_gap"]] <- FALSE
  out[["explicit_missing_glucose"]] <- is.na(out[[target_col]])

  id_values <- unique(out[[id_col]])
  id_values <- id_values[!is.na(id_values)]
  regularized <- vector("list", length(id_values))

  for (i in seq_along(id_values)) {
    id_value <- id_values[[i]]
    g <- out[!is.na(out[[id_col]]) & out[[id_col]] == id_value, , drop = FALSE]
    g <- g[order(g[[time_col]]), , drop = FALSE]

    start_time <- min(g[[time_col]], na.rm = TRUE)
    interval_minutes_num <- as.numeric(interval_minutes)

    # Snap observed timestamps to the nearest interval grid anchored at the
    # subject's first timestamp. This prevents off-grid readings such as 11:56
    # from being preserved separately from generated grid rows such as 11:55.
    x_minutes_raw <- as.numeric(difftime(
      g[[time_col]],
      start_time,
      units = "mins"
    ))

    x_minutes_snapped <- round(x_minutes_raw / interval_minutes_num) *
      interval_minutes_num

    g[[time_col]] <- start_time + x_minutes_snapped * 60

    # If snapping creates duplicate subject-time rows, keep the row with a
    # non-missing target value first. This avoids losing observed glucose values
    # when an explicit NA and an observed value map to the same grid time.
    g <- g[order(g[[time_col]], is.na(g[[target_col]])), , drop = FALSE]

    dup_key <- paste(
      g[[id_col]],
      format(g[[time_col]], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      sep = "||"
    )

    g <- g[!duplicated(dup_key), , drop = FALSE]

    x_minutes <- as.numeric(difftime(
      g[[time_col]],
      start_time,
      units = "mins"
    ))

    grid_minutes <- .cgmd_py_equal_interval_minutes(
      x_minutes = x_minutes,
      interval_minutes = interval_minutes
    )

    grid_times <- start_time + grid_minutes * 60
    grid <- data.frame(
      .cgmd_grid_id = rep(id_value, length(grid_times)),
      .cgmd_grid_time = as.POSIXct(
        grid_times,
        origin = "1970-01-01",
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
    names(grid) <- c(id_col, time_col)

    merged <- merge(
      grid,
      g,
      by = c(id_col, time_col),
      all.x = TRUE,
      sort = TRUE
    )

    inserted <- is.na(merged[["inserted_timestamp_gap"]])
    merged[["inserted_timestamp_gap"]][inserted] <- TRUE
    merged[["inserted_timestamp_gap"]][is.na(merged[[
      "inserted_timestamp_gap"
    ]])] <- FALSE
    merged[["explicit_missing_glucose"]][inserted] <- FALSE
    merged[["explicit_missing_glucose"]][is.na(merged[[
      "explicit_missing_glucose"
    ]])] <- FALSE

    protected_cols <- c(
      id_col,
      time_col,
      target_col,
      "TimeSeries",
      "TimeDifferenceMinutes",
      "inserted_timestamp_gap",
      "explicit_missing_glucose",
      "missing_source"
    )
    fill_cols <- setdiff(names(merged), protected_cols)

    for (nm in fill_cols) {
      source_vals <- g[[nm]]
      source_vals <- source_vals[!is.na(source_vals)]
      if (length(source_vals) > 0L && length(unique(source_vals)) == 1L) {
        merged[[nm]][inserted & is.na(merged[[nm]])] <- source_vals[[1L]]
      }
    }

    merged[["missing_source"]] <- "observed"
    merged[["missing_source"]][merged[[
      "inserted_timestamp_gap"
    ]]] <- "timestamp_gap"
    merged[["missing_source"]][
      !merged[["inserted_timestamp_gap"]] & is.na(merged[[target_col]])
    ] <- "explicit_na"

    merged <- merged[order(merged[[time_col]]), , drop = FALSE]
    merged[["TimeSeries"]] <- as.numeric(
      difftime(merged[[time_col]], start_time, units = "mins")
    )
    merged[["TimeDifferenceMinutes"]] <- c(
      NA_real_,
      as.numeric(diff(merged[[time_col]]), units = "mins")
    )

    regularized[[i]] <- merged
  }

  missing_id_rows <- out[is.na(out[[id_col]]), , drop = FALSE]
  if (nrow(missing_id_rows) > 0L) {
    missing_id_rows[["missing_source"]] <- ifelse(
      is.na(missing_id_rows[[target_col]]),
      "explicit_na",
      "observed"
    )
    missing_id_rows[["TimeSeries"]] <- NA_real_
    missing_id_rows[["TimeDifferenceMinutes"]] <- NA_real_
    regularized[[length(regularized) + 1L]] <- missing_id_rows
  }

  ans <- do.call(rbind, regularized)
  ans <- ans[order(ans[[id_col]], ans[[time_col]]), , drop = FALSE]
  rownames(ans) <- NULL
  ans
}
.cgmd_py_warn_missingness <- function(
  df,
  id_col,
  time_col,
  target_col,
  interval_minutes = 5L,
  missing_warning_threshold = 0.20,
  study_start = NULL,
  study_end = NULL
) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)

  if (!id_col %in% names(out)) {
    stop("Missing required id_col: ", id_col, call. = FALSE)
  }
  if (!time_col %in% names(out)) {
    stop("Missing required time_col: ", time_col, call. = FALSE)
  }
  if (!target_col %in% names(out)) {
    stop("Missing required target_col: ", target_col, call. = FALSE)
  }

  interval_minutes <- as.numeric(interval_minutes)

  missing_pos <- is.na(out[[target_col]])
  missing_n <- sum(missing_pos)
  total_n <- nrow(out)
  missing_rate <- if (total_n > 0L) missing_n / total_n else NA_real_

  if (is.finite(missing_rate) && missing_rate > missing_warning_threshold) {
    warning(
      sprintf(
        "High missingness after timestamp-gap regularization: %.1f%% of '%s' values are missing (%s of %s rows). Imputed values may be less reliable when missingness is high.",
        missing_rate * 100,
        target_col,
        missing_n,
        total_n
      ),
      call. = FALSE
    )
  }

  half_day_rows <- ceiling((12 * 60) / interval_minutes)
  full_day_rows <- ceiling((24 * 60) / interval_minutes)

  id_values <- unique(out[[id_col]])
  id_values <- id_values[!is.na(id_values)]

  long_gap_messages <- character(0)

  for (id_value in id_values) {
    g <- out[!is.na(out[[id_col]]) & out[[id_col]] == id_value, , drop = FALSE]
    g <- g[order(g[[time_col]]), , drop = FALSE]

    miss <- is.na(g[[target_col]])
    if (!any(miss)) {
      next
    }

    rr <- rle(miss)
    ends <- cumsum(rr$lengths)
    starts <- ends - rr$lengths + 1L
    missing_runs <- which(rr$values)

    if (length(missing_runs) == 0L) {
      next
    }

    run_lengths <- rr$lengths[missing_runs]
    max_run_idx <- missing_runs[which.max(run_lengths)]
    max_run_len <- rr$lengths[max_run_idx]

    if (max_run_len >= half_day_rows) {
      start_i <- starts[max_run_idx]
      end_i <- ends[max_run_idx]

      gap_start <- g[[time_col]][start_i]
      gap_end <- g[[time_col]][end_i]
      gap_hours <- max_run_len * interval_minutes / 60

      severity <- if (max_run_len >= full_day_rows) {
        "at least one full day"
      } else {
        "at least one half day"
      }

      long_gap_messages <- c(
        long_gap_messages,
        sprintf(
          "Subject %s has a contiguous missing block of %s (%.1f hours), from %s to %s.",
          as.character(id_value),
          severity,
          gap_hours,
          format(gap_start, "%Y-%m-%d %H:%M:%S"),
          format(gap_end, "%Y-%m-%d %H:%M:%S")
        )
      )
    }
  }

  if (length(long_gap_messages) > 0L) {
    warning(
      paste(
        c(
          "Long contiguous missing glucose blocks were detected after timestamp-gap regularization:",
          long_gap_messages
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  study_start_parsed <- .cgmd_py_parse_boundary_time(study_start, "study_start")
  study_end_parsed <- .cgmd_py_parse_boundary_time(study_end, "study_end")

  boundary_messages <- character(0)

  if (!is.null(study_start_parsed) || !is.null(study_end_parsed)) {
    for (id_value in id_values) {
      g <- out[
        !is.na(out[[id_col]]) & out[[id_col]] == id_value,
        ,
        drop = FALSE
      ]
      g <- g[order(g[[time_col]]), , drop = FALSE]

      first_obs <- min(g[[time_col]], na.rm = TRUE)
      last_obs <- max(g[[time_col]], na.rm = TRUE)

      if (!is.null(study_start_parsed)) {
        leading_minutes <- as.numeric(
          difftime(first_obs, study_start_parsed, units = "mins")
        )

        if (is.finite(leading_minutes) && leading_minutes >= interval_minutes) {
          boundary_messages <- c(
            boundary_messages,
            sprintf(
              "Subject %s starts %.1f hours after study_start. Leading study time is not imputed.",
              as.character(id_value),
              leading_minutes / 60
            )
          )
        }
      }

      if (!is.null(study_end_parsed)) {
        trailing_minutes <- as.numeric(
          difftime(study_end_parsed, last_obs, units = "mins")
        )

        if (
          is.finite(trailing_minutes) && trailing_minutes >= interval_minutes
        ) {
          boundary_messages <- c(
            boundary_messages,
            sprintf(
              "Subject %s ends %.1f hours before study_end. Trailing study time is not imputed.",
              as.character(id_value),
              trailing_minutes / 60
            )
          )
        }
      }
    }
  }

  if (length(boundary_messages) > 0L) {
    message(
      paste(
        c(
          "Study boundary coverage note:",
          boundary_messages
        ),
        collapse = "\n"
      )
    )
  }

  invisible(list(
    missing_n = missing_n,
    total_n = total_n,
    missing_rate = missing_rate,
    missing_warning_threshold = missing_warning_threshold
  ))
}

.cgmd_py_parse_boundary_time <- function(x, nm) {
  if (is.null(x)) {
    return(NULL)
  }

  if (length(x) != 1L) {
    stop(nm, " must be NULL or a single timestamp value.", call. = FALSE)
  }

  parsed <- .cgmd_py_parse_timestamp(x)

  if (length(parsed) != 1L || is.na(parsed)) {
    stop(nm, " could not be parsed as a timestamp.", call. = FALSE)
  }

  parsed
}

.cgmd_py_equal_interval_minutes <- function(x_minutes, interval_minutes = 5L) {
  x_minutes <- sort(unique(as.numeric(x_minutes)))
  x_minutes <- x_minutes[is.finite(x_minutes)]

  if (length(x_minutes) == 0L) {
    return(numeric(0))
  }

  interval_minutes <- as.numeric(interval_minutes)

  base_grid <- seq(
    from = min(x_minutes),
    to = max(x_minutes),
    by = interval_minutes
  )

  # Call CGManalyzer for consistency with the package workflow, but do not use
  # arbitrary returned times to expand the grid. For imputation, we need a
  # deterministic interval grid so timestamp gaps become explicit NA rows.
  invisible(
    tryCatch(
      {
        CGManalyzer::equalInterval.fn(
          x = x_minutes,
          y = seq_along(x_minutes),
          Interval = interval_minutes
        )
      },
      error = function(e) NULL
    )
  )

  base_grid
}

.cgmd_py_add_timeseries_column <- function(
  df,
  ts_col = "timestamp",
  id_col = "subjectid",
  interval_minutes = 5L,
  prefer_cgmanalyzer = TRUE
) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!ts_col %in% names(out)) {
    stop(sprintf("Column '%s' was not found.", ts_col), call. = FALSE)
  }

  # Python returns unchanged if a non-empty TimeSeries column is already present.
  if ("TimeSeries" %in% names(out) && any(!is.na(out[["TimeSeries"]]))) {
    return(out)
  }

  parsed <- .cgmd_py_parse_timestamp(out[[ts_col]])
  if (any(is.na(parsed))) {
    stop("Some timestamp values could not be parsed.", call. = FALSE)
  }

  if (
    isTRUE(prefer_cgmanalyzer) &&
      requireNamespace("CGManalyzer", quietly = TRUE)
  ) {
    ans <- tryCatch(
      .cgmd_py_add_timeseries_equal_interval(
        out = out,
        parsed = parsed,
        id_col = id_col,
        interval_minutes = interval_minutes
      ),
      error = function(e) NULL
    )
    if (!is.null(ans)) {
      return(ans)
    }
  }

  .cgmd_py_timeseries_fallback(out = out, parsed = parsed, id_col = id_col)
}

.cgmd_py_encode_sex <- function(df, col = "SEX") {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  if (col %in% names(out)) {
    s <- toupper(trimws(as.character(out[[col]])))
    mapped <- rep(NA_real_, length(s))
    mapped[s %in% c("M", "MALE", "1")] <- 1
    mapped[s %in% c("F", "FEMALE", "0")] <- 0
    out[[col]] <- mapped
  }
  out
}

.cgmd_py_add_lag_features <- function(
  df,
  target_col = "glucose_value",
  id_col = "subjectid",
  time_col = "TimeSeries",
  lag_k = c(1L, 2L, 3L),
  roll_window = 3L,
  add_rollmean = TRUE
) {
  out <- .cgmd_py_sort_reset(df, c(id_col, time_col))
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package 'data.table' is required for lag feature creation.",
      call. = FALSE
    )
  }
  if (!target_col %in% names(out)) {
    stop(sprintf("Column '%s' was not found.", target_col), call. = FALSE)
  }

  dt <- data.table::as.data.table(out)
  lag_k <- as.integer(lag_k)

  for (k in lag_k) {
    nm <- paste0("lag", k)
    dt[, (nm) := NA_real_]
    dt[
      !is.na(get(id_col)),
      (nm) := data.table::shift(get(target_col), n = k, type = "lag"),
      by = id_col
    ]
  }

  if (isTRUE(add_rollmean)) {
    roll_col <- "rollmean"
    dt[, (roll_col) := NA_real_]
    dt[
      !is.na(get(id_col)),
      (roll_col) := data.table::frollmean(
        data.table::shift(get(target_col), n = 1L, type = "lag"),
        n = as.integer(roll_window),
        fill = NA_real_,
        align = "right"
      ),
      by = id_col
    ]
  }

  as.data.frame(dt)
}

.cgmd_py_impute_values <- function(
  df,
  id_col = "subjectid",
  time_col = "TimeSeries",
  target_col = "glucose_value",
  feature_cols = NULL,
  use_arima_if_missing_leq = 0.05,
  seed = 42L,
  imputer_backend = c("mice"),
  arima_order = c(4L, 1L, 0L),
  arima_min_history = 20L,
  xgb_nrounds = 300L,
  rf_n_estimators = 200L,
  knn_k = 7L,
  lgb_nrounds = 400L,
  models = "auto"
) {
  imputer_backend <- match.arg(imputer_backend)
  out <- .cgmd_py_sort_reset(df, c(id_col, time_col))

  if (is.null(feature_cols)) {
    feature_cols <- setdiff(names(out), target_col)
  }
  feature_cols <- unique(setdiff(feature_cols, target_col))
  feature_cols <- intersect(feature_cols, names(out))

  required_cols <- c(target_col, feature_cols)
  missing_cols <- setdiff(required_cols, names(out))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  mask_pos <- is.na(out[[target_col]])
  miss_rate <- mean(mask_pos)

  imp_df <- out[, c(target_col, feature_cols), drop = FALSE]
  imp_mat_in <- as.matrix(data.frame(
    lapply(imp_df, .cgmd_py_to_numeric),
    check.names = FALSE
  ))
  storage.mode(imp_mat_in) <- "double"

  if (ncol(imp_mat_in) < 1L) {
    stop(
      "The imputation matrix must contain at least the target column.",
      call. = FALSE
    )
  }
  if (all(is.na(imp_mat_in[, 1L]))) {
    stop(
      "All target values are missing; at least one observed target value is required.",
      call. = FALSE
    )
  }

  imp_mat <- .cgmd_py_impute_matrix(
    imp_mat_in,
    seed = seed,
    backend = imputer_backend,
    max_iter = 10L
  )

  y_mice_full <- as.numeric(imp_mat[, 1L])
  X_imp <- imp_mat[, -1L, drop = FALSE]
  y_final <- y_mice_full
  model_info <- .cgmd_resolve_real_imputation_model(
    models = models,
    missing_rate = miss_rate,
    use_arima_if_missing_leq = use_arima_if_missing_leq
  )
  model_key <- model_info$key
  method <- model_info$label

  if (identical(model_key, "arima")) {
    y_final <- .cgmd_py_arima_segmentwise_on_mice(
      df_sorted = out,
      id_col = id_col,
      y_mice_full = y_mice_full,
      mask_pos = mask_pos,
      order = arima_order,
      min_history = arima_min_history
    )
  } else {
    train_idx <- !mask_pos
    if (!any(train_idx)) {
      stop(
        "No observed target values are available for model training.",
        call. = FALSE
      )
    }
    X_train <- X_imp[train_idx, , drop = FALSE]
    y_train <- as.numeric(out[[target_col]][train_idx])
    X_missing <- X_imp[mask_pos, , drop = FALSE]
    filled <- .cgmd_fill_missing_with_train_medians(
      train_mat = X_train,
      test_mat = X_missing,
      cols = colnames(X_train)
    )
    X_train <- filled$train
    X_missing <- filled$test

    y_pred_missing <- switch(
      model_key,
      xgboost = .cgmd_py_fit_xgb_predict_missing(
        X_train = X_train,
        y_train = y_train,
        X_missing = X_missing,
        seed = seed,
        nrounds = xgb_nrounds
      ),
      rf = .cgmd_py_fit_rf_predict_missing(
        X_train = X_train,
        y_train = y_train,
        X_missing = X_missing,
        seed = seed,
        n_estimators = rf_n_estimators
      ),
      knn = .cgmd_py_fit_knn_predict_missing(
        X_train = X_train,
        y_train = y_train,
        X_missing = X_missing,
        k = knn_k
      ),
      lightgbm = .cgmd_py_fit_lgb_predict_missing(
        X_train = X_train,
        y_train = y_train,
        X_missing = X_missing,
        seed = seed,
        nrounds = lgb_nrounds
      ),
      stop("Unsupported real-imputation model: ", model_key, call. = FALSE)
    )
    y_final[mask_pos] <- y_pred_missing
  }

  out[["imputed_glucose_value"]] <- as.numeric(y_final)
  out[["imputation_method"]] <- method
  out[["missing_rate"]] <- as.numeric(miss_rate)
  out <- .cgmd_py_drop_internal_return_cols(out)
  rownames(out) <- NULL

  out
}

.cgmd_py_drop_internal_return_cols <- function(df) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)

  # Internal engineered features used for imputation/modeling but omitted from
  # the returned data frame. Add any future return-only exclusions to this
  # vector, for example: "SomeTemporaryColumn" or "AnotherInternalFeature".
  drop_cols <- c(
    grep("^lag[0-9]+$", names(out), value = TRUE),
    "rollmean"
  )

  drop_cols <- intersect(unique(drop_cols), names(out))
  if (length(drop_cols) > 0L) {
    out[drop_cols] <- NULL
  }

  out
}
.cgmd_py_keep_user_output_cols <- function(
  df,
  original_cols,
  imputed_col = "imputed_glucose_value"
) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)

  keep_cols <- unique(c(
    intersect(original_cols, names(out)),
    imputed_col
  ))

  keep_cols <- intersect(keep_cols, names(out))

  out <- out[, keep_cols, drop = FALSE]
  rownames(out) <- NULL
  out
}
.cgmd_py_to_numeric <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  suppressWarnings(as.numeric(x))
}

.cgmd_py_sort_reset <- function(df, cols) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in cols) {
    if (!nm %in% names(out)) {
      stop(sprintf("Column '%s' was not found.", nm), call. = FALSE)
    }
  }

  ord_exprs <- vector("list", length(cols) * 2L)
  for (i in seq_along(cols)) {
    v <- out[[cols[[i]]]]
    ord_exprs[[2L * i - 1L]] <- is.na(v)
    ord_exprs[[2L * i]] <- v
  }
  ord <- do.call(order, c(ord_exprs, list(na.last = TRUE, method = "radix")))
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.cgmd_py_parse_timestamp <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%OS",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%Y:%m:%d:%H:%M:%S",
    "%Y:%m:%d:%H:%M",
    "%m/%d/%Y %H:%M:%OS",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%m/%d/%Y"
  )

  parsed <- as.POSIXct(rep(NA_character_, length(x_chr)), tz = "UTC")
  remaining <- !is.na(x_chr)

  for (fmt in formats) {
    if (!any(remaining)) {
      break
    }
    candidate <- as.POSIXct(
      strptime(x_chr[remaining], format = fmt, tz = "UTC"),
      tz = "UTC"
    )
    ok <- !is.na(candidate)
    if (any(ok)) {
      remaining_idx <- which(remaining)
      parsed[remaining_idx[ok]] <- candidate[ok]
      remaining[remaining_idx[ok]] <- FALSE
    }
  }

  parsed
}

.cgmd_py_add_timeseries_equal_interval <- function(
  out,
  parsed,
  id_col,
  interval_minutes
) {
  out[["TimeSeries"]] <- NA_real_

  if (id_col %in% names(out)) {
    ids <- out[[id_col]]
    id_values <- unique(ids)
    id_values <- id_values[!is.na(id_values)]
    groups <- lapply(id_values, function(idv) which(!is.na(ids) & ids == idv))
  } else {
    groups <- list(seq_len(nrow(out)))
  }

  for (idx in groups) {
    if (length(idx) == 0L) {
      next
    }
    idx_sorted <- idx[order(parsed[idx], na.last = TRUE)]
    first_time <- min(parsed[idx_sorted], na.rm = TRUE)
    x_minutes <- as.numeric(difftime(
      parsed[idx_sorted],
      first_time,
      units = "mins"
    ))
    dummy_y <- seq_along(idx_sorted) - 1L

    r_times <- tryCatch(
      {
        r_result <- CGManalyzer::equalInterval.fn(
          x = x_minutes,
          y = dummy_y,
          Interval = as.integer(interval_minutes)
        )
        as.numeric(as.data.frame(r_result)[[1L]])
      },
      error = function(e) numeric(0)
    )

    if (length(r_times) >= length(idx_sorted)) {
      out[["TimeSeries"]][idx_sorted] <- r_times[seq_along(idx_sorted)]
    } else {
      out[["TimeSeries"]][idx_sorted] <- x_minutes
    }
  }

  out
}

.cgmd_py_timeseries_fallback <- function(out, parsed, id_col) {
  out[[".cgmd_ts_parsed"]] <- parsed

  if (id_col %in% names(out)) {
    out <- .cgmd_py_sort_reset(out, c(id_col, ".cgmd_ts_parsed"))
    out[["TimeSeries"]] <- NA_real_
    ids <- out[[id_col]]
    id_values <- unique(ids)
    id_values <- id_values[!is.na(id_values)]
    for (idv in id_values) {
      idx <- which(!is.na(ids) & ids == idv)
      first_time <- min(out[[".cgmd_ts_parsed"]][idx], na.rm = TRUE)
      out[["TimeSeries"]][idx] <- as.numeric(
        difftime(out[[".cgmd_ts_parsed"]][idx], first_time, units = "mins")
      )
    }
  } else {
    out <- .cgmd_py_sort_reset(out, c(".cgmd_ts_parsed"))
    first_time <- min(out[[".cgmd_ts_parsed"]], na.rm = TRUE)
    out[["TimeSeries"]] <- as.numeric(difftime(
      out[[".cgmd_ts_parsed"]],
      first_time,
      units = "mins"
    ))
  }

  out[[".cgmd_ts_parsed"]] <- NULL
  rownames(out) <- NULL
  out
}

.cgmd_py_impute_matrix <- function(mat, seed, backend, max_iter = 10L) {
  mat <- as.matrix(mat)
  storage.mode(mat) <- "double"
  mat[!is.finite(mat)] <- NA_real_

  if (!anyNA(mat)) {
    return(mat)
  }

  if (identical(backend, "mice")) {
    return(.cgmd_py_mice_imputer(mat, seed = seed, max_iter = max_iter))
  }

  stop("Unsupported imputer_backend.", call. = FALSE)
}

.cgmd_py_mice_imputer <- function(mat, seed, max_iter = 10L) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop(
      "Package 'mice' is required for imputer_backend = 'mice'.",
      call. = FALSE
    )
  }
  dat <- as.data.frame(mat)
  names(dat) <- paste0("V", seq_len(ncol(dat)))

  all_missing <- vapply(dat, function(x) all(is.na(x)), logical(1))
  if (any(all_missing)) {
    stop(
      "Cannot impute columns that are entirely missing with imputer_backend = 'mice': ",
      paste(names(dat)[all_missing], collapse = ", "),
      call. = FALSE
    )
  }

  method <- mice::make.method(dat)
  method[] <- ""
  has_missing <- vapply(dat, function(x) any(is.na(x)), logical(1))
  method[has_missing] <- "norm"

  set.seed(seed)
  imp <- mice::mice(
    dat,
    m = 1L,
    maxit = as.integer(max_iter),
    method = method,
    printFlag = FALSE,
    seed = as.integer(seed)
  )
  out <- as.matrix(mice::complete(imp, 1L))
  storage.mode(out) <- "double"
  out
}

.cgmd_py_segments <- function(mask) {
  mask <- as.logical(mask)
  segs <- list()
  i <- 1L
  n <- length(mask)
  while (i <= n) {
    if (isTRUE(mask[[i]])) {
      j <- i
      while (j + 1L <= n && isTRUE(mask[[j + 1L]])) {
        j <- j + 1L
      }
      segs[[length(segs) + 1L]] <- c(i, j)
      i <- j + 1L
    } else {
      i <- i + 1L
    }
  }
  segs
}

.cgmd_py_arima_segmentwise_on_mice <- function(
  df_sorted,
  id_col,
  y_mice_full,
  mask_pos,
  order = c(4L, 1L, 0L),
  min_history = 20L
) {
  pred_full <- as.numeric(y_mice_full)
  mask_pos <- as.logical(mask_pos)

  ids <- df_sorted[[id_col]]
  id_values <- unique(ids)
  id_values <- id_values[!is.na(id_values)]

  for (idv in id_values) {
    idx <- which(!is.na(ids) & ids == idv)
    local_mask <- mask_pos[idx]
    segs <- .cgmd_py_segments(local_mask)
    if (length(segs) == 0L) {
      next
    }

    # Python copies this once from the MICE-completed series; ARIMA forecasts
    # for earlier blocks are not fed into later histories.
    local_series <- pred_full[idx]

    for (seg in segs) {
      s <- seg[[1L]]
      e <- seg[[2L]]
      block_len <- e - s + 1L
      history <- if (s <= 1L) numeric(0) else local_series[seq_len(s - 1L)]

      if (length(history) < as.integer(min_history)) {
        next
      }
      if (!all(is.finite(history))) {
        next
      }

      fcst <- tryCatch(
        {
          if (requireNamespace("forecast", quietly = TRUE)) {
            fit <- forecast::Arima(history, order = as.integer(order))
            as.numeric(forecast::forecast(fit, h = block_len)$mean)
          } else {
            fit <- stats::arima(history, order = as.integer(order))
            as.numeric(stats::predict(fit, n.ahead = block_len)$pred)
          }
        },
        error = function(e) NULL
      )

      if (!is.null(fcst) && length(fcst) == block_len && all(is.finite(fcst))) {
        pred_full[idx[s:e]] <- fcst
      }
    }
  }

  pred_full
}

.cgmd_py_fit_xgb_predict_missing <- function(
  X_train,
  y_train,
  X_missing,
  seed = 42L,
  nrounds = 300L
) {
  if (nrow(X_missing) == 0L) {
    return(numeric(0))
  }
  if (ncol(X_train) == 0L) {
    stop("XGBoost branch requires at least one feature column.", call. = FALSE)
  }

  params <- list(
    objective = "reg:squarederror",
    eta = 0.05,
    max_depth = 6L,
    subsample = 0.8,
    colsample_bytree = 0.8,
    lambda = 1.0,
    nthread = -1L,
    seed = as.integer(seed),
    tree_method = "hist",
    eval_metric = "rmse"
  )

  set.seed(seed)
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = as.integer(nrounds),
    verbose = 0
  )
  as.numeric(stats::predict(model, xgboost::xgb.DMatrix(data = X_missing)))
}

.cgmd_py_fit_rf_predict_missing <- function(
  X_train,
  y_train,
  X_missing,
  seed = 42L,
  n_estimators = 200L
) {
  if (nrow(X_missing) == 0L) {
    return(numeric(0))
  }
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop(
      "Package 'ranger' is required when models = 'rf'.",
      call. = FALSE
    )
  }
  if (ncol(X_train) == 0L) {
    stop("MICE+RF requires at least one feature column.", call. = FALSE)
  }

  .cgmd_assert_all_finite_matrix(X_train, "X_train")
  .cgmd_assert_all_finite_matrix(X_missing, "X_missing")

  set.seed(seed)
  rf_model <- ranger::ranger(
    x = X_train,
    y = y_train,
    num.trees = as.integer(n_estimators),
    mtry = ncol(X_train),
    min.node.size = 1L,
    replace = TRUE,
    sample.fraction = 1.0,
    seed = as.integer(seed),
    num.threads = 1L
  )
  as.numeric(stats::predict(rf_model, data = X_missing)$predictions)
}

.cgmd_py_fit_knn_predict_missing <- function(
  X_train,
  y_train,
  X_missing,
  k = 7L
) {
  if (nrow(X_missing) == 0L) {
    return(numeric(0))
  }
  if (!requireNamespace("FNN", quietly = TRUE)) {
    stop(
      "Package 'FNN' is required when models = 'knn'.",
      call. = FALSE
    )
  }
  if (ncol(X_train) == 0L) {
    stop("MICE+kNN requires at least one feature column.", call. = FALSE)
  }

  .cgmd_assert_all_finite_matrix(X_train, "X_train")
  .cgmd_assert_all_finite_matrix(X_missing, "X_missing")

  scaler <- .cgmd_fit_scaler(X_train)
  X_train_sc <- .cgmd_transform_scaler(X_train, scaler)
  X_missing_sc <- .cgmd_transform_scaler(X_missing, scaler)
  .cgmd_assert_all_finite_matrix(X_train_sc, "X_train_sc")
  .cgmd_assert_all_finite_matrix(X_missing_sc, "X_missing_sc")

  k <- max(1L, min(as.integer(k), nrow(X_train_sc)))
  as.numeric(FNN::knn.reg(
    train = X_train_sc,
    test = X_missing_sc,
    y = y_train,
    k = k
  )$pred)
}

.cgmd_py_fit_lgb_predict_missing <- function(
  X_train,
  y_train,
  X_missing,
  seed = 42L,
  nrounds = 400L
) {
  if (nrow(X_missing) == 0L) {
    return(numeric(0))
  }
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop(
      "Package 'lightgbm' is required when models = 'lightgbm'.",
      call. = FALSE
    )
  }
  if (ncol(X_train) == 0L) {
    stop("MICE+LightGBM requires at least one feature column.", call. = FALSE)
  }

  .cgmd_assert_all_finite_matrix(X_train, "X_train")
  .cgmd_assert_all_finite_matrix(X_missing, "X_missing")

  set.seed(seed)
  lgb_train <- lightgbm::lgb.Dataset(data = X_train, label = y_train)
  lgb_model <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      learning_rate = 0.05,
      num_leaves = 31L,
      bagging_fraction = 0.8,
      feature_fraction = 0.8,
      seed = as.integer(seed),
      verbose = -1L
    ),
    data = lgb_train,
    nrounds = as.integer(nrounds)
  )
  as.numeric(stats::predict(lgb_model, X_missing))
}

.cgmd_real_imputation_model_keys <- function() {
  c("arima", "xgboost", "rf", "knn", "lightgbm")
}

.cgmd_normalize_real_imputation_model <- function(models) {
  if (is.null(models)) {
    return("auto")
  }
  if (is.factor(models)) {
    models <- as.character(models)
  }
  if (!is.character(models) || length(models) != 1L) {
    stop(
      "models must be NULL, 'auto', or exactly one of: ",
      paste(.cgmd_real_imputation_model_keys(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  model <- tolower(trimws(models))
  if (is.na(model) || identical(model, "")) {
    stop("models cannot be NA or empty.", call. = FALSE)
  }
  if (identical(model, "auto")) {
    return(model)
  }
  if (model %in% c("all", "mice_only")) {
    stop(
      "models = '",
      model,
      "' is not supported by run_missing_glucose_imputation() because it returns one imputed column. ",
      "Use one of: auto, ",
      paste(.cgmd_real_imputation_model_keys(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!model %in% .cgmd_real_imputation_model_keys()) {
    stop(
      "Invalid models value: '",
      model,
      "'. Use NULL, 'auto', or exactly one of: ",
      paste(.cgmd_real_imputation_model_keys(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  model
}

.cgmd_resolve_real_imputation_model <- function(
  models,
  missing_rate,
  use_arima_if_missing_leq = 0.05
) {
  model <- .cgmd_normalize_real_imputation_model(models)
  if (identical(model, "auto")) {
    model <- if (missing_rate <= use_arima_if_missing_leq) {
      "arima"
    } else {
      "xgboost"
    }
  }

  labels <- c(
    arima = "MICE+ARIMA",
    xgboost = "MICE+XGBoost",
    rf = "MICE+RF",
    knn = "MICE+kNN",
    lightgbm = "MICE+LightGBM"
  )

  list(key = model, label = unname(labels[[model]]))
}

.cgmd_model_keys <- function() {
  c("mice_only", "rf", "knn", "xgboost", "lightgbm", "arima")
}

.cgmd_normalize_models <- function(models) {
  if (is.factor(models)) {
    models <- as.character(models)
  }
  if (!is.character(models) || length(models) == 0L) {
    stop("models must be a character vector.")
  }

  models <- tolower(trimws(models))
  if (any(is.na(models)) || any(models == "")) {
    stop("models cannot contain NA or empty values.")
  }

  valid_models <- c(.cgmd_model_keys(), "all")
  invalid_models <- setdiff(models, valid_models)
  if (length(invalid_models) > 0L) {
    stop(
      "Invalid models: ",
      paste(invalid_models, collapse = ", "),
      ". Valid values are: ",
      paste(valid_models, collapse = ", "),
      "."
    )
  }

  if ("all" %in% models) {
    return(.cgmd_model_keys())
  }

  unique(models)
}

.cgmd_empty_model_rows <- function(models) {
  rows <- vector("list", length(models))
  names(rows) <- models
  for (model in models) {
    rows[[model]] <- list()
  }
  rows
}

.cgmd_bind_imputed_model_rows <- function(rows) {
  out <- do.call(rbind, rows)
  if ("MaskRateNum" %in% names(out)) {
    out <- out[order(out$MaskRateNum, out$.RowID), ]
    out$MaskRateNum <- NULL
  } else {
    out <- out[order(out$.RowID), ]
  }
  out$.RowID <- NULL
  rownames(out) <- NULL
  out
}

.cgmd_imputed_data_rows <- function(
  rate,
  rate_label,
  mask_type,
  method,
  df_base,
  df_model,
  target_col,
  mask_pos,
  y_pred
) {
  engineered_cols <- setdiff(names(df_model), names(df_base))
  engineered_df <- df_model[, engineered_cols, drop = FALSE]
  observed <- as.numeric(df_base[[target_col]])

  out <- data.frame(
    MaskRateNum = rate,
    MaskRate = rate_label,
    MaskType = mask_type,
    Method = method,
    .RowID = df_base$.RowID,
    .Masked = as.logical(mask_pos),
    stringsAsFactors = FALSE
  )
  out <- cbind(
    out,
    df_base[, setdiff(names(df_base), ".RowID"), drop = FALSE],
    engineered_df
  )
  out$ObservedValue <- observed
  out$ImputedValue <- as.numeric(y_pred)
  out
}

.cgmd_real_imputed_data_rows <- function(
  method,
  df_base,
  df_model,
  target_col,
  missing_pos,
  y_pred
) {
  engineered_cols <- setdiff(names(df_model), names(df_base))
  engineered_df <- df_model[, engineered_cols, drop = FALSE]
  observed <- as.numeric(df_base[[target_col]])

  out <- data.frame(
    Method = method,
    .RowID = df_base$.RowID,
    .Missing = as.logical(missing_pos),
    stringsAsFactors = FALSE
  )
  out <- cbind(
    out,
    df_base[, setdiff(names(df_base), ".RowID"), drop = FALSE],
    engineered_df
  )
  out$ObservedValue <- observed
  out$ImputedValue <- as.numeric(y_pred)
  out
}

.cgmd_coerce_numeric_strict <- function(x, nm) {
  if (is.numeric(x) || is.integer(x)) {
    return(as.double(x))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    num <- suppressWarnings(as.numeric(x))
    bad <- is.na(num) & !is.na(x) & nzchar(x)
    if (any(bad)) {
      stop(
        "Column '",
        nm,
        "' contains non-numeric values; recode it before imputation."
      )
    }
    return(num)
  }
  stop("Column '", nm, "' has unsupported type for numeric coercion.")
}

.cgmd_add_time_features <- function(
  df,
  raw_time_col,
  id_col,
  time_format,
  time_unit,
  time_series_col,
  time_diff_col
) {
  prepared_time <- .cgmd_prepare_time_stamps(
    x = df[[raw_time_col]],
    time_format = time_format
  )
  time_mat <- CGManalyzer::timeSeqConversion.fn(
    time.stamp = prepared_time,
    time.format = time_format,
    timeUnit = time_unit
  )
  if (ncol(time_mat) < 1L) {
    stop("CGManalyzer::timeSeqConversion.fn() did not return a time series.")
  }

  df[[time_series_col]] <- as.numeric(time_mat[, 1])
  if (any(!is.finite(df[[time_series_col]]))) {
    stop("Converted TimeSeries contains non-finite values.")
  }

  dt <- data.table::as.data.table(df)
  data.table::setorderv(dt, c(id_col, time_series_col))
  diff_divisor <- if (identical(time_unit, "second")) 60 else 1
  dt[,
    (time_diff_col) := {
      ts <- get(time_series_col)
      c(0, diff(ts) / diff_divisor)
    },
    by = id_col
  ]

  as.data.frame(dt)
}

.cgmd_prepare_time_stamps <- function(x, time_format) {
  default_time_format <- "yyyy:mm:dd:hh:nn"
  if (!identical(time_format, default_time_format)) {
    return(as.character(x))
  }

  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y:%m:%d:%H:%M"))
  }

  if (inherits(x, "Date")) {
    return(format(as.POSIXct(x, tz = "UTC"), "%Y:%m:%d:%H:%M"))
  }

  x_chr <- trimws(as.character(x))
  missing_time <- is.na(x_chr) | x_chr == ""
  if (any(missing_time)) {
    stop("time_col contains missing or blank timestamps.")
  }

  if (all(grepl("^\\d{4}:\\d{2}:\\d{2}:\\d{2}:\\d{2}$", x_chr))) {
    return(x_chr)
  }

  parsed <- .cgmd_parse_common_time_stamps(x_chr)
  failed <- is.na(parsed)
  if (any(failed)) {
    examples <- paste(
      "2020:01:16:00:00",
      "2020-01-16 00:00",
      "2020-01-16 00:00:00",
      "2020/01/16 00:00",
      "2020/01/16 00:00:00",
      "2020-01-16T00:00:00",
      "01/16/2020 00:00",
      sep = ", "
    )
    stop(
      "Unable to parse time_col timestamps automatically. ",
      "Use a common timestamp format such as: ",
      examples,
      ". For advanced CGManalyzer-specific formats, provide time_format."
    )
  }

  format(parsed, "%Y:%m:%d:%H:%M")
}

.cgmd_parse_common_time_stamps <- function(x) {
  formats <- c(
    "%Y:%m:%d:%H:%M:%S",
    "%Y:%m:%d:%H:%M",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m/%d/%Y"
  )

  parsed <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")
  remaining <- rep(TRUE, length(x))

  for (fmt in formats) {
    if (!any(remaining)) {
      break
    }
    candidate <- as.POSIXct(
      strptime(x[remaining], format = fmt, tz = "UTC"),
      tz = "UTC"
    )
    ok <- !is.na(candidate)
    if (any(ok)) {
      remaining_idx <- which(remaining)
      parsed[remaining_idx[ok]] <- candidate[ok]
      remaining[remaining_idx[ok]] <- FALSE
    }
  }

  parsed
}

.cgmd_sort_by_id_time <- function(df, id_col, time_col) {
  dt <- data.table::as.data.table(df)
  data.table::setorderv(dt, c(id_col, time_col))
  as.data.frame(dt)
}

.cgmd_compute_lag_features <- function(
  df,
  target_col,
  id_col,
  time_col,
  lag_k,
  add_rollmean,
  roll_window
) {
  dt <- data.table::as.data.table(df)
  data.table::setorderv(dt, c(id_col, time_col))

  for (k in lag_k) {
    nm <- paste0(target_col, "_lag", k)
    dt[,
      (nm) := data.table::shift(get(target_col), n = k, type = "lag"),
      by = id_col
    ]
  }

  if (isTRUE(add_rollmean)) {
    rc <- paste0(target_col, "_rollmean_", roll_window)
    dt[,
      (rc) := data.table::frollmean(
        data.table::shift(get(target_col), n = 1, type = "lag"),
        n = as.integer(roll_window),
        fill = NA_real_,
        align = "right"
      ),
      by = id_col
    ]
  }

  as.data.frame(dt)
}

.cgmd_make_mask_pos <- function(
  n,
  rate,
  mask_type,
  seed,
  gap_bins,
  gap_probs,
  open_cap
) {
  n_mask <- as.integer(ceiling(rate * n))
  if (n_mask <= 0L) {
    return(rep(FALSE, n))
  }
  if (n_mask >= n) {
    stop("Mask size >= number of rows; reduce mask rate or provide more rows.")
  }

  set.seed(seed)

  if (mask_type == "random") {
    idx <- sample.int(n, size = n_mask, replace = FALSE)
  } else if (mask_type == "block") {
    start <- sample.int(n - n_mask + 1L, size = 1L)
    idx <- start:(start + n_mask - 1L)
  } else {
    idx <- .cgmd_gap_block_indices(
      n = n,
      n_mask = n_mask,
      gap_bins = gap_bins,
      gap_probs = gap_probs,
      open_cap = open_cap
    )
  }

  mask_pos <- rep(FALSE, n)
  mask_pos[idx] <- TRUE
  mask_pos
}

.cgmd_gap_block_indices <- function(n, n_mask, gap_bins, gap_probs, open_cap) {
  if (length(gap_bins) == 0L) {
    stop("gap_bins must contain at least one bin.")
  }
  if (length(gap_bins) != length(gap_probs)) {
    stop("gap_bins and gap_probs must have the same length.")
  }
  if (any(!is.finite(gap_probs)) || sum(gap_probs) <= 0) {
    stop("gap_probs must be finite and have a positive sum.")
  }

  probs <- gap_probs / sum(gap_probs)
  bins_eff <- lapply(gap_bins, function(b) {
    if (length(b) != 2L) {
      stop("Each gap bin must have length 2.")
    }
    lo <- as.integer(b[1])
    hi <- b[2]
    hi_eff <- if (is.na(hi)) {
      as.integer(max(lo, floor(n_mask * open_cap)))
    } else {
      as.integer(hi)
    }
    if (!is.finite(lo) || !is.finite(hi_eff) || lo < 1L || hi_eff < lo) {
      stop("Invalid gap bin: ", paste(b, collapse = ", "))
    }
    c(lo, hi_eff)
  })

  block_sizes <- integer(0)
  total <- 0L
  while (total < n_mask) {
    remaining <- n_mask - total
    b_idx <- sample.int(length(bins_eff), 1L, prob = probs)
    lo <- bins_eff[[b_idx]][1]
    hi <- bins_eff[[b_idx]][2]
    len <- if (lo > remaining) {
      remaining
    } else {
      sample.int(min(hi, remaining) - lo + 1L, 1L) + lo - 1L
    }
    block_sizes <- c(block_sizes, len)
    total <- total + len
  }

  k <- length(block_sizes)
  n_free <- n - n_mask
  splits <- sort(sample.int(n_free + 1L, size = k, replace = (k > n_free)) - 1L)
  gaps <- diff(c(0L, splits, n_free))

  masked <- integer(0)
  pos <- 0L
  for (i in seq_along(block_sizes)) {
    pos <- pos + gaps[i]
    masked <- c(masked, pos:(pos + block_sizes[i] - 1L))
    pos <- pos + block_sizes[i]
  }
  masked <- sort(unique(masked))
  masked <- masked[masked < n]
  masked + 1L
}

.cgmd_fill_missing_with_train_medians <- function(train_mat, test_mat, cols) {
  cols <- intersect(cols, colnames(train_mat))
  if (length(cols) == 0L) {
    return(list(train = train_mat, test = test_mat, fill_vals = numeric(0)))
  }

  fill_vals <- vapply(
    cols,
    function(col) stats::median(train_mat[, col], na.rm = TRUE),
    numeric(1)
  )
  bad_cols <- names(fill_vals)[!is.finite(fill_vals)]
  if (length(bad_cols) > 0L) {
    stop(
      sprintf(
        "Cannot compute finite training medians for columns: %s",
        paste(bad_cols, collapse = ", ")
      )
    )
  }

  for (col in cols) {
    train_bad <- !is.finite(train_mat[, col])
    test_bad <- !is.finite(test_mat[, col])
    if (any(train_bad)) {
      train_mat[train_bad, col] <- fill_vals[[col]]
    }
    if (any(test_bad)) {
      test_mat[test_bad, col] <- fill_vals[[col]]
    }
  }

  list(train = train_mat, test = test_mat, fill_vals = fill_vals)
}

.cgmd_assert_all_finite_matrix <- function(x, name) {
  bad_counts <- colSums(!is.finite(x))
  if (any(bad_counts > 0L)) {
    stop(
      sprintf(
        "%s contains non-finite values in columns: %s",
        name,
        paste(names(bad_counts)[bad_counts > 0L], collapse = ", ")
      )
    )
  }
}

.cgmd_fit_scaler <- function(mat) {
  mat <- as.matrix(mat)
  mu <- colMeans(mat, na.rm = TRUE)
  centered <- sweep(mat, 2, mu, "-")
  sd_pop <- sqrt(colMeans(centered^2, na.rm = TRUE))
  sd_pop[!is.finite(sd_pop) | sd_pop == 0] <- 1
  list(mean = mu, scale = sd_pop)
}

.cgmd_transform_scaler <- function(mat, scaler) {
  mat <- as.matrix(mat)
  out <- sweep(mat, 2, scaler$mean, "-")
  out <- sweep(out, 2, scaler$scale, "/")
  out[!is.finite(out)] <- 0
  out
}

.cgmd_metric_row <- function(
  rate,
  rate_label,
  mask_type,
  method,
  y_true,
  y_pred,
  masked_count
) {
  mrd <- .cgmd_mrd_full(y_true, y_pred)
  data.frame(
    MaskRateNum = rate,
    MaskRate = rate_label,
    MaskType = mask_type,
    Method = method,
    MAPE = mrd * 100,
    R2 = .cgmd_r2_full(y_true, y_pred),
    MRD = mrd,
    MaskedCount = masked_count,
    stringsAsFactors = FALSE
  )
}

.cgmd_mrd_full <- function(y_true, y_pred) {
  y_true <- as.numeric(y_true)
  y_pred <- as.numeric(y_pred)
  ok <- abs(y_true) != 0 & is.finite(y_true) & is.finite(y_pred)
  if (!any(ok)) {
    return(NA_real_)
  }
  sum(abs(y_true[ok] - y_pred[ok]) / abs(y_true[ok])) / length(y_true)
}

.cgmd_r2_full <- function(y_true, y_pred) {
  y_true <- as.numeric(y_true)
  y_pred <- as.numeric(y_pred)
  ok <- is.finite(y_true) & is.finite(y_pred)
  if (!any(ok)) {
    return(NA_real_)
  }
  sst <- sum((y_true[ok] - mean(y_true[ok]))^2)
  if (!is.finite(sst) || sst == 0) {
    return(NA_real_)
  }
  1 - sum((y_true[ok] - y_pred[ok])^2) / sst
}
