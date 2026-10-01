.onLoad <- function(libname, pkgname) {
  reticulate::py_require(c(
    "numpy", "pandas", "scikit-learn", "statsmodels", "xgboost"
  ))
}
