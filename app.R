# app.R at repository root
# Deployment wrapper for Posit Connect Cloud.

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "The package 'pkgload' is required to run this development app. ",
    "Add pkgload to Suggests in DESCRIPTION.",
    call. = FALSE
  )
}

# Deployment installs Python dependencies from requirements.txt. The sklearn
# backend declares requirements lazily and respects the host-selected environment.

pkgload::load_all(
  ".",
  export_all = FALSE,
  helpers = FALSE,
  attach = TRUE
)

app_env <- new.env(parent = globalenv())

source(
  file = file.path("inst", "shiny", "cgm_imputation_app", "app.R"),
  local = app_env
)$value
