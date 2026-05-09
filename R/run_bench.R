#' Run missingness benchmark (target-masking with LAG features)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use
#' `run_missing_glucose_imputation()` for real missing glucose values.
#'
#' This function implements missingness benchmarking by masking the target column at various rates and
#' evaluating imputation and predictive performance of MICE, Random Forest, and KNN methods. Additionally,
#' it includes LAG features of the target variable to assess their impact on imputation and prediction.
#' The function returns a data.frame summarizing the Mask Rate, Method, MRD (Mean Relative Difference), and
#' Masked Count for each method and mask rate.
#'
#' @param data A data.frame (or object coercible to data.frame), OR a path to a CSV file.
#' @param target_col Single character string: name of the outcome column to mask/impute (e.g., "LBORRES", "Glucose").
#' @param feature_cols Character vector of base feature columns (excluding the target).
#'   If NULL, uses all columns except `target_col`.
#' @param id_col Character string: subject identifier column used for LAG features (default "USUBJID").
#' @param time_col Character string: time-ordering column used for LAG features (default "TimeSeries").
#' @param mask_rates Numeric vector in (0, 1): fraction of rows to mask (default 0.05, 0.10, 0.20, 0.30, 0.40).
#' @param mask_type One of `"random"` or `"block"`.
#' @param rf_n_estimators Integer: number of trees for random forest (default 400).
#' @param knn_k Integer: number of neighbors for kNN (default 7).
#' @param seed Integer: random seed used for MICE and models (default 42).
#' @param lag_k Integer vector of lags to compute on the target (default c(1,2,3)).
#' @param add_rollmean Logical: add rolling mean feature of prior target values (default TRUE).
#' @param roll_window Integer: rolling window length for rollmean (default 3).
#'
#' @return A data.frame with columns: MaskRate, Method, MRD, MaskedCount.
#'
#' @details
#' LAG features are computed using `data.table::shift()` (fast lag/lead). The rolling mean
#' is computed with `data.table::frollmean()` using `align="right"` and `fill=NA`.
#'
#' @importFrom FNN knn.reg
#' @importFrom ranger ranger
#' @importFrom mice mice complete
#' @importFrom data.table as.data.table setorderv shift frollmean
#' @importFrom stats predict complete.cases
#'
#' @export
run_missingness_benchmark <- function(
  data,
  target_col,
  feature_cols = NULL,
  id_col = "USUBJID",
  time_col = "TimeSeries",
  mask_rates = c(0.05, 0.10, 0.20, 0.30, 0.40),
  mask_type = c("random", "block"),
  rf_n_estimators = 400,
  knn_k = 7,
  seed = 42,
  lag_k = c(1, 2, 3),
  add_rollmean = TRUE,
  roll_window = 3
) {
  lifecycle::deprecate_warn(
    "0.0.1.9000",
    "run_missingness_benchmark()",
    details = "Use run_missing_glucose_imputation() for real missing glucose values."
  )

  # ---------------------------------------------------------------------------
  # [A] Load / normalize input
  # ---------------------------------------------------------------------------
  if (is.character(data) && length(data) == 1L && file.exists(data)) {
    df <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else {
    df <- as.data.frame(data)
  }

  # ---------------------------------------------------------------------------
  # [B] Strict numeric coercion for required columns (Python uses dtype=float)
  # ---------------------------------------------------------------------------
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
          "' contains non-numeric values; benchmark requires numeric columns."
        )
      }
      return(num)
    }
    stop("Column '", nm, "' has unsupported type for numeric coercion.")
  }

  # feature_cols default: all except target
  if (is.null(feature_cols)) {
    feature_cols <- setdiff(names(df), target_col)
  }

  # Validate required columns exist
  base_needed <- unique(c(target_col, feature_cols, id_col, time_col))
  missing_cols <- setdiff(base_needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Coerce base columns to numeric
  for (nm in base_needed) {
    df[[nm]] <- coerce_numeric_strict(df[[nm]], nm)
  }

  # ---------------------------------------------------------------------------
  # [C] Complete-case baseline on base columns (matches Python df.dropna())
  # ---------------------------------------------------------------------------
  df <- df[
    stats::complete.cases(df[, base_needed, drop = FALSE]),
    ,
    drop = FALSE
  ]
  if (nrow(df) < 10L) {
    stop("Not enough complete rows after baseline cleaning.")
  }

  # ---------------------------------------------------------------------------
  # [D] LAG feature engineering (matches benchmark_pythonv2.txt)
  #     - sort by id/time
  #     - LB_lag1..LB_lagK
  #     - LB_rollmean_3 = rolling mean of shifted(1) over window
  #     - drop rows without full lag history (lag1..lagK complete)
  # ---------------------------------------------------------------------------
  dt <- data.table::as.data.table(df)
  data.table::setorderv(dt, c(id_col, time_col))

  # Lags of target
  lag_cols <- character(0)
  for (k in lag_k) {
    nm <- paste0("LB_lag", k)
    lag_cols <- c(lag_cols, nm)
    dt[,
      (nm) := data.table::shift(get(target_col), n = k, type = "lag"),
      by = id_col
    ]
  }

  # Rolling mean of prior values: shift(1) then rollmean(window)
  if (isTRUE(add_rollmean)) {
    dt[,
      LB_rollmean_3 := data.table::frollmean(
        data.table::shift(get(target_col), n = 1, type = "lag"),
        n = as.integer(roll_window),
        fill = NA_real_,
        align = "right"
      ),
      by = id_col
    ]
  }

  # Drop rows with insufficient history (Python drops based on lag1..lag3 only)
  keep_hist <- stats::complete.cases(dt[, ..lag_cols])
  dt <- dt[keep_hist]
  if (nrow(dt) < 10L) {
    stop("Not enough rows after LAG feature engineering (history too short).")
  }

  # Update features to include engineered columns
  engineered <- lag_cols
  if (isTRUE(add_rollmean)) {
    engineered <- c(engineered, "LB_rollmean_3")
  }
  feature_cols <- unique(c(feature_cols, engineered))

  # ---------------------------------------------------------------------------
  # [E] Helpers: MRD + sklearn-like StandardScaler (ddof=0)
  # ---------------------------------------------------------------------------
  mrd <- function(y_true, y_pred) {
    y_true <- as.numeric(y_true)
    y_pred <- as.numeric(y_pred)
    denom <- abs(y_true)
    ok <- denom != 0
    if (!any(ok)) {
      return(NA_real_)
    }
    mean(abs(y_true[ok] - y_pred[ok]) / denom[ok])
  }

  fit_scaler <- function(mat) {
    mat <- as.matrix(mat)
    mu <- colMeans(mat, na.rm = TRUE)
    centered <- sweep(mat, 2, mu, "-")
    var_pop <- colMeans(centered^2, na.rm = TRUE)
    sd_pop <- sqrt(var_pop)
    sd_pop[sd_pop == 0 | !is.finite(sd_pop)] <- 1
    list(mean = mu, scale = sd_pop)
  }

  transform_scaler <- function(mat, scaler) {
    mat <- as.matrix(mat)
    out <- sweep(mat, 2, scaler$mean, "-")
    out <- sweep(out, 2, scaler$scale, "/")
    out[!is.finite(out)] <- 0
    out
  }
  make_mask_pos <- function(
    n,
    rate,
    type = c("random", "block"),
    seed,
    block_start = NULL
  ) {
    type <- match.arg(type)
    n_mask <- as.integer(ceiling(rate * n))
    set.seed(seed)

    if (type == "random") {
      idx <- sample.int(n, size = n_mask, replace = FALSE)
    } else {
      if (n_mask >= n) {
        stop("Block size >= n; reduce rate or n.")
      }
      if (is.null(block_start)) {
        block_start <- sample.int(n - n_mask + 1L, 1L)
      }
      idx <- block_start:(block_start + n_mask - 1L)
    }

    mask_pos <- rep(FALSE, n)
    mask_pos[idx] <- TRUE
    mask_pos
  }

  # ---------------------------------------------------------------------------
  # [F] Benchmark loop (matches Python masking: rng=2024+int(rate*100), mask ceil(rate*n))
  # ---------------------------------------------------------------------------
  results_list <- list()
  y_true_full <- dt[[target_col]]
  n <- nrow(dt)

  # Ensure mask_rates numeric
  if (is.factor(mask_rates)) {
    mask_rates <- as.character(mask_rates)
  }
  if (is.character(mask_rates)) {
    mask_rates <- as.numeric(mask_rates)
  }
  if (!is.numeric(mask_rates) || any(!is.finite(mask_rates))) {
    stop("mask_rates must be a numeric vector in (0,1).")
  }

  for (rate in mask_rates) {
    if (rate <= 0 || rate >= 1) {
      stop("mask_rates must be in (0,1). Got: ", rate)
    }
    rate_label <- paste0(as.integer(rate * 100), "%")

    # Python-style seed for selecting masked rows
    set.seed(2024 + as.integer(rate * 100))
    mask_type_1 <- match.arg(mask_type) # optional but recommended

    seed_mask <- 2024 + as.integer(rate * 100)
    mask_pos <- make_mask_pos(
      n = n,
      rate = rate,
      type = mask_type_1,
      seed = seed_mask
    )

    # Build imputation frame: [target] + [features], then mask ONLY target
    imp_df <- as.data.frame(dt[, c(target_col, feature_cols), with = FALSE])
    imp_df[[target_col]][mask_pos] <- NA_real_

    # --- MICE impute all columns (target + features) ---
    imp <- mice::mice(
      imp_df,
      m = 1,
      maxit = 10,
      method = "norm",
      ridge = 1e-5,
      printFlag = FALSE,
      seed = seed
    )
    completed <- mice::complete(imp, 1)

    y_imp <- completed[[target_col]]
    X_imp <- as.matrix(completed[, feature_cols, drop = FALSE])

    # (1) MICE-only MRD on masked rows
    results_list[[length(results_list) + 1L]] <- data.frame(
      MaskRateNum = rate,
      MaskRate = rate_label,
      Method = if (isTRUE(add_rollmean)) {
        paste0("MICE-only (impute ", target_col, " with lags)")
      } else {
        paste0("MICE-only (impute ", target_col, ")")
      },
      MRD = mrd(y_true_full[mask_pos], y_imp[mask_pos]),
      MaskedCount = sum(mask_pos),
      stringsAsFactors = FALSE
    )

    # Train on unmasked rows, test on masked rows
    train_idx <- !mask_pos
    test_idx <- mask_pos

    X_train <- X_imp[train_idx, , drop = FALSE]
    y_train <- y_true_full[train_idx]
    X_test <- X_imp[test_idx, , drop = FALSE]
    y_test <- y_true_full[test_idx]

    # (2) Random Forest (ranger)
    rf_data <- data.frame(y_target = y_train, X_train)
    rf_model <- ranger::ranger(
      y_target ~ .,
      data = rf_data,
      num.trees = rf_n_estimators,
      mtry = ncol(X_train),
      min.node.size = 1,
      replace = TRUE,
      sample.fraction = 1,
      seed = seed,
      num.threads = 1
    )
    y_rf <- stats::predict(rf_model, data = as.data.frame(X_test))$predictions

    results_list[[length(results_list) + 1L]] <- data.frame(
      MaskRateNum = rate,
      MaskRate = rate_label,
      Method = if (isTRUE(add_rollmean)) {
        "MICE + RF (with lags)"
      } else {
        "MICE + RF"
      },
      MRD = mrd(y_test, y_rf),
      MaskedCount = sum(mask_pos),
      stringsAsFactors = FALSE
    )

    # (3) KNN (FNN) with sklearn-like scaling
    scaler <- fit_scaler(X_train)
    X_train_sc <- transform_scaler(X_train, scaler)
    X_test_sc <- transform_scaler(X_test, scaler)

    y_knn <- FNN::knn.reg(
      train = X_train_sc,
      test = X_test_sc,
      y = y_train,
      k = knn_k
    )$pred

    results_list[[length(results_list) + 1L]] <- data.frame(
      MaskRateNum = rate,
      MaskRate = rate_label,
      Method = if (isTRUE(add_rollmean)) {
        "MICE + KNN (with lags)"
      } else {
        "MICE + KNN"
      },
      MRD = mrd(y_test, y_knn),
      MaskedCount = sum(mask_pos),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, results_list)
  out <- out[order(out$MaskRateNum, out$Method), ]
  out$MaskRateNum <- NULL
  rownames(out) <- NULL
  out
}
