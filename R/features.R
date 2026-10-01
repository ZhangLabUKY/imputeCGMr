.cgmd_prepare_features <- function(data, feature_cols, feature_types = NULL) {
  feature_cols <- unique(feature_cols)
  if (!length(feature_types)) feature_types <- NULL
  if (anyNA(feature_cols) || any(!feature_cols %in% names(data))) {
    stop("Unknown feature columns: ", paste(setdiff(feature_cols, names(data)), collapse = ", "), call. = FALSE)
  }
  if (!is.null(feature_types)) {
    if (!is.character(feature_types) || is.null(names(feature_types)) ||
        anyNA(names(feature_types)) || any(!nzchar(names(feature_types))) ||
        anyDuplicated(names(feature_types)) || anyNA(feature_types) ||
        any(!feature_types %in% c("numeric", "categorical")) ||
        any(!names(feature_types) %in% feature_cols)) {
      stop("feature_types must be a named numeric/categorical vector for selected feature columns.", call. = FALSE)
    }
  }
  matrix_data <- data.frame(row.names = seq_len(nrow(data)))
  diagnostics <- data.frame(column = character(), type = character(), status = character(), reason = character())
  for (i in seq_along(feature_cols)) {
    column <- feature_cols[[i]]
    original <- data[[column]]
    values <- trimws(as.character(original))
    values[is.na(original) | !nzchar(values)] <- NA_character_
    numeric_values <- suppressWarnings(as.numeric(values))
    observed <- !is.na(values)
    inferred <- if (is.factor(original) || is.logical(original)) "categorical" else
      if (is.numeric(original) || (any(observed) && all(is.finite(numeric_values[observed])))) "numeric" else "categorical"
    type <- if (column %in% names(feature_types)) unname(feature_types[[column]]) else inferred
    if (type == "numeric" && any(observed & !is.finite(numeric_values))) {
      stop("Numeric predictor '", column, "' contains non-numeric or non-finite values.", call. = FALSE)
    }
    status <- "used"
    reason <- "Included in fitting matrix"
    if (!any(observed)) {
      status <- "all_missing"
      reason <- "No observed predictor values"
    } else if (length(unique(if (type == "numeric") numeric_values[observed] else values[observed])) == 1L && (type == "numeric" || all(observed))) {
      status <- "constant"
      reason <- "No predictive variation within this fit"
    }
    diagnostics <- rbind(diagnostics, data.frame(column = column, type = type, status = status, reason = reason))
    if (status != "used") next
    prefix <- utils::tail(make.unique(c(names(data), names(matrix_data), paste0(".cgmd_feature_", i))), 1L)
    if (type == "numeric") {
      matrix_data[[prefix]] <- numeric_values
    } else {
      levels <- sort(unique(values[observed]), method = "radix")
      codes <- match(values, levels)
      if (any(!observed)) codes[!observed] <- length(levels) + 1L
      count <- length(levels) + as.integer(any(!observed))
      for (level in seq.int(2L, count)) {
        indicator <- utils::tail(make.unique(c(names(data), names(matrix_data), paste0(prefix, "_", level))), 1L)
        matrix_data[[indicator]] <- as.numeric(codes == level)
      }
    }
  }
  list(data = matrix_data, diagnostics = diagnostics)
}

.cgmd_restore_feature_output <- function(result, original, row_key, diagnostics, export_path) {
  rows <- match(result[[row_key]], original[[row_key]])
  if (anyNA(rows) || anyDuplicated(rows) || length(rows) != nrow(original)) {
    stop("Imputation result could not be aligned to the original rows.", call. = FALSE)
  }
  out <- original[rows, setdiff(names(original), row_key), drop = FALSE]
  out$imputed_glucose_value <- as.numeric(result$imputed_glucose_value)
  rownames(out) <- NULL
  attr(out, "feature_diagnostics") <- diagnostics
  .cgmd_py_export_if_requested(out, export_path)
}
