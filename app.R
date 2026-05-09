# app.R at repository root

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "The package 'pkgload' is required to run this development app. ",
    "Add pkgload to Suggests in DESCRIPTION.",
    call. = FALSE
  )
}

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach = TRUE)

app_env <- new.env(parent = globalenv())

source(
  file = "inst/shiny/cgm_imputation_app/app.R",
  local = app_env
)$value
