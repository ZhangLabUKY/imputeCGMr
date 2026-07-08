#' Launch the imputeCGM Shiny App
#'
#' @description
#' Launches a Shiny app for uploading a CGM data file, selecting the target,
#' subject, timestamp, and feature columns, running
#' [run_missing_glucose_imputation()], previewing the imputed data, and
#' downloading the completed data as a CSV file.
#'
#' @return Invisibly returns the result of [shiny::runApp()].
#'
#' @examples
#' if (interactive()) {
#' run_app()
#' }
#'
#'
#' @export
run_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The Shiny app requires the optional package 'shiny'. ",
      "Install it with install.packages('shiny').",
      call. = FALSE
    )
  }

  app_dir <- system.file(
    "shiny",
    "cgm_imputation_app",
    package = "imputeCGM"
  )

  if (identical(app_dir, "")) {
    stop("Could not find the installed Shiny app directory.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
