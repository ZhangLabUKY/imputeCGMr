library(shiny)

options(shiny.maxRequestSize = 1024^3)

.imputation_method_choices <- c(
  "Automatic by missing rate" = "auto",
  "MICE + ARIMA" = "arima",
  "MICE + XGBoost" = "xgboost",
  "MICE + Random Forest" = "rf",
  "MICE + kNN" = "knn",
  "MICE + LightGBM" = "lightgbm"
)

.cgmd_shiny_imputation_args <- function(
  data,
  target_col,
  feature_cols,
  id_col,
  time_col,
  imputer_backend,
  models,
  use_arima_if_missing_leq,
  xgb_nrounds,
  rf_n_estimators,
  knn_k,
  lgb_nrounds,
  n_threads,
  seed,
  feature_types = NULL
) {
  list(
    data = data,
    target_col = target_col,
    feature_cols = feature_cols,
    feature_types = feature_types,
    id_col = id_col,
    time_col = time_col,
    imputer_backend = imputer_backend,
    models = models,
    use_arima_if_missing_leq = use_arima_if_missing_leq,
    xgb_nrounds = xgb_nrounds,
    rf_n_estimators = rf_n_estimators,
    knn_k = knn_k,
    lgb_nrounds = lgb_nrounds,
    n_threads = n_threads,
    seed = seed,
    export_path = NULL
  )
}

ui <- fluidPage(
  titlePanel("imputeCGM: Missing Glucose Imputation"),

  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file",
        label = "Upload CGM data CSV",
        accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")
      ),
      tags$hr(),

      h5("Or load example data"),

      selectInput(
        inputId = "example_data",
        label = NULL,
        choices = c(
          "Choose example data" = "",
          "CGM example data - 5% missing" = "CGMExmplDat5Pct",
          "CGM example data - 10% missing" = "CGMExmplDat10Pct"
        ),
        selected = ""
      ),

      actionButton(
        inputId = "load_example",
        label = "Load example data",
        class = "btn-secondary"
      ),
      tags$hr(),

      uiOutput("column_selectors"),
      actionButton("features_all", "Select all predictors"),
      actionButton("features_none", "Exclude all predictors"),
      uiOutput("feature_types_ui"),

      tags$hr(),

      selectInput(
        inputId = "imputer_backend",
        label = "Imputation backend",
        choices = c(
          "R-native MICE backend" = "mice",
          "Python-compatible sklearn backend" = "sklearn"
        ),
        selected = "mice"
      ),

      selectInput(
        inputId = "models",
        label = "Final imputation method",
        choices = .imputation_method_choices,
        selected = "auto"
      ),

      conditionalPanel(
        condition = "input.models == 'auto'",
        numericInput(
        inputId = "use_arima_if_missing_leq",
        label = "Use ARIMA if missing rate is <=",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.01
      ),
      ),

      conditionalPanel(
        condition = "input.models == 'auto' || input.models == 'xgboost'",
        numericInput(
        inputId = "xgb_nrounds",
        label = "XGBoost boosting rounds",
        value = 300,
        min = 1,
        step = 1
      ),
      ),

      conditionalPanel(
        condition = "input.models == 'rf'",
        numericInput(
          inputId = "rf_n_estimators",
          label = "Random Forest trees",
          value = 200,
          min = 1,
          step = 1
        )
      ),

      conditionalPanel(
        condition = "input.models == 'knn'",
        numericInput(
          inputId = "knn_k",
          label = "kNN neighbors",
          value = 7,
          min = 1,
          step = 1
        )
      ),

      conditionalPanel(
        condition = "input.models == 'lightgbm'",
        numericInput(
          inputId = "lgb_nrounds",
          label = "LightGBM boosting rounds",
          value = 400,
          min = 1,
          step = 1
        )
      ),

      numericInput(
        inputId = "seed",
        label = "Random seed (optional)",
        value = NA,
        min = 1,
        step = 1
      ),

      numericInput(
        inputId = "n_threads",
        label = "Model threads",
        value = 1,
        min = 1,
        step = 1
      ),

      tags$hr(),

      actionButton(
        inputId = "run",
        label = "Run imputation",
        class = "btn-primary"
      ),

      tags$br(),
      tags$br(),

      downloadButton(
        outputId = "download",
        label = "Download imputed CSV"
      )
    ),

    mainPanel(
      fluidRow(
        column(
          width = 8,
          h4("Uploaded data preview"),
          tableOutput("raw_preview")
        ),
        column(
          width = 4,
          h4("Missingness"),
          uiOutput("missingness_card")
        )
      ),

      # tags$hr(),

      # h4("Imputation summary"),
      # verbatimTextOutput("summary"),

      tags$hr(),

      h4("Preview of rows imputed from originally missing values"),
      tableOutput("imputed_preview"),
      h4("Predictor diagnostics"),
      tableOutput("feature_diagnostics")
    )
  )
)
.load_example_data <- function(object_name) {
  ns <- tryCatch(asNamespace("imputeCGM"), error = function(e) NULL)

  if (!is.null(ns) && exists(object_name, envir = ns, inherits = FALSE)) {
    return(get(object_name, envir = ns, inherits = FALSE))
  }

  env <- new.env(parent = emptyenv())
  ok <- tryCatch(
    {
      data(list = object_name, package = "imputeCGM", envir = env)
      exists(object_name, envir = env, inherits = FALSE)
    },
    error = function(e) FALSE
  )

  if (isTRUE(ok)) {
    return(get(object_name, envir = env, inherits = FALSE))
  }

  stop("Could not load example dataset: ", object_name, call. = FALSE)
}

server <- function(input, output, session) {
  active_data <- reactiveVal(NULL)
  active_data_name <- reactiveVal(NULL)

  observeEvent(input$file, {
    req(input$file)

    dat <- tryCatch(
      {
        data.table::fread(
          input$file$datapath,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      },
      error = function(e) {
        showNotification(
          paste("Could not read uploaded CSV:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )

    if (!is.null(dat)) {
      active_data(dat)
      active_data_name(input$file$name)
    }
  })

  observeEvent(input$load_example, {
    req(input$example_data)
    validate(
      need(nzchar(input$example_data), "Please choose an example dataset.")
    )

    dat <- switch(
      input$example_data,
      "CGMExmplDat5Pct" = {
        .load_example_data("CGMExmplDat5Pct")
      },
      "CGMExmplDat10Pct" = {
        .load_example_data("CGMExmplDat10Pct")
      },
      NULL
    )

    if (is.null(dat)) {
      showNotification(
        "Could not load the selected example dataset.",
        type = "error",
        duration = NULL
      )
      return(NULL)
    }

    active_data(as.data.frame(dat, stringsAsFactors = FALSE))
    active_data_name(input$example_data)

    showNotification(
      paste("Loaded example dataset:", input$example_data),
      type = "message"
    )
  })

  uploaded_data <- reactive({
    dat <- active_data()
    req(dat)
    dat
  })

  output$raw_preview <- renderTable({
    dat <- uploaded_data()
    req(dat)

    preview_dat <- utils::head(dat, 10)

    # Force Time columns to exactly match the original CSV format
    for (col in names(preview_dat)) {
      if (inherits(preview_dat[[col]], c("POSIXct", "POSIXt", "Date"))) {
        preview_dat[[col]] <- format(
          preview_dat[[col]],
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )
      }
    }

    preview_dat
  })

  warning_threshold <- 20
  output$missingness_card <- renderUI({
    dat <- uploaded_data()
    req(dat)
    req(input$target_col)

    if (!nzchar(input$target_col) || !input$target_col %in% names(dat)) {
      return(
        div(
          style = paste(
            "padding: 14px;",
            "border-radius: 8px;",
            "background-color: #f5f5f5;",
            "border: 1px solid #ddd;"
          ),
          strong("Select a target glucose column"),
          br(),
          span("Missingness will appear here.")
        )
      )
    }

    missing_n <- sum(is.na(dat[[input$target_col]]))
    total_n <- nrow(dat)
    missing_pct <- 100 * missing_n / total_n

    is_high <- missing_pct > warning_threshold

    bg_color <- if (is_high) "#f8d7da" else "#d4edda"
    border_color <- if (is_high) "#f5c2c7" else "#badbcc"
    text_color <- if (is_high) "#842029" else "#0f5132"
    label <- if (is_high) "High missingness" else "Acceptable missingness"

    div(
      style = paste(
        "padding: 14px;",
        "border-radius: 8px;",
        "background-color:",
        bg_color,
        ";",
        "border: 1px solid",
        border_color,
        ";",
        "color:",
        text_color,
        ";"
      ),
      strong(label),
      br(),
      tags$span(
        style = "font-size: 28px; font-weight: 700;",
        sprintf("%.1f%%", missing_pct)
      ),
      br(),
      tags$span(
        sprintf("%s missing out of %s rows", missing_n, total_n)
      ),
      if (is_high) {
        tagList(
          br(),
          tags$small(paste(
            "Warning: missingness is above",
            warning_threshold,
            "%."
          ))
        )
      }
    )
  })
  output$column_selectors <- renderUI({
    dat <- uploaded_data()
    req(dat)

    cols <- names(dat)

    tagList(
      selectInput(
        inputId = "target_col",
        label = "Target glucose column",
        choices = c("Select a column" = "", cols),
        selected = ""
      ),

      selectInput(
        inputId = "id_col",
        label = "Subject ID column",
        choices = c("Select a column" = "", cols),
        selected = ""
      ),

      selectInput(
        inputId = "time_col",
        label = "Timestamp column",
        choices = c("Select a column" = "", cols),
        selected = ""
      ),

      selectizeInput(
        inputId = "feature_cols",
        label = "Feature columns",
        choices = cols,
        selected = cols,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    )
  })

  feature_choices <- reactive({
    setdiff(names(uploaded_data()), c(input$target_col, input$id_col, input$time_col))
  })
  observeEvent(feature_choices(), {
    available <- feature_choices()
    old <- isolate(input$feature_cols)
    updateSelectizeInput(session, "feature_cols", choices = available,
      selected = union(intersect(old, available), setdiff(available, isolate(previous_features()))))
    previous_features(available)
  })
  previous_features <- reactiveVal(character())
  observeEvent(input$features_all, {
    updateSelectizeInput(session, "feature_cols", selected = feature_choices())
  })
  observeEvent(input$features_none, {
    updateSelectizeInput(session, "feature_cols", selected = character())
  })
  output$feature_types_ui <- renderUI({
    columns <- input$feature_cols
    tagList(lapply(columns, function(column) {
      key <- paste0("feature_type_", paste(as.integer(charToRaw(enc2utf8(column))), collapse = "_"))
      selectInput(key, paste("Type for", column), choices = c("Auto" = "auto", "Numeric" = "numeric", "Categorical" = "categorical"),
        selected = if (is.null(isolate(input[[key]]))) "auto" else isolate(input[[key]]))
    }))
  })

  imputed_data <- eventReactive(input$run, {
    dat <- uploaded_data()
    req(dat)
    req(input$target_col)
    req(input$id_col)
    req(input$time_col)

    validate(
      need(nzchar(input$target_col), "Please select a target glucose column."),
      need(nzchar(input$id_col), "Please select a subject ID column."),
      need(nzchar(input$time_col), "Please select a timestamp column."),
      need(input$target_col %in% names(dat), "Target column not found."),
      need(input$id_col %in% names(dat), "Subject ID column not found."),
      need(input$time_col %in% names(dat), "Timestamp column not found.")
    )
    withProgress(message = "Running imputation...", value = 0.2, {
      tryCatch(
        {
          incProgress(0.4)

          feature_cols <- input$feature_cols
          feature_cols <- as.character(feature_cols)
          types <- stats::setNames(vapply(feature_cols, function(column) {
            key <- paste0("feature_type_", paste(as.integer(charToRaw(enc2utf8(column))), collapse = "_"))
            value <- input[[key]]
            if (is.null(value)) "auto" else value
          }, character(1)), feature_cols)
          types <- types[types != "auto"]

          call_args <- .cgmd_shiny_imputation_args(
            data = dat,
            target_col = input$target_col,
            feature_cols = feature_cols,
            feature_types = types,
            id_col = input$id_col,
            time_col = input$time_col,
            imputer_backend = input$imputer_backend,
            models = input$models,
            use_arima_if_missing_leq = input$use_arima_if_missing_leq,
            xgb_nrounds = input$xgb_nrounds,
            rf_n_estimators = input$rf_n_estimators,
            knn_k = input$knn_k,
            lgb_nrounds = input$lgb_nrounds,
            n_threads = input$n_threads,
            seed = if (is.na(input$seed)) NULL else input$seed
          )
          out <- do.call(run_missing_glucose_imputation, call_args)

          incProgress(0.4)
          out
        },
        error = function(e) {
          showNotification(
            paste("Imputation failed:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
          NULL
        }
      )
    })
  })

  output$feature_diagnostics <- renderTable({
    attr(imputed_data(), "feature_diagnostics", exact = TRUE)
  })

  output$imputed_preview <- renderTable({
    out <- imputed_data()
    req(out)
    req(input$target_col)

    imputed_rows <- out[is.na(out[[input$target_col]]), , drop = FALSE]
    preview_dat <- utils::head(imputed_rows, 15)

    # Force Time columns to exactly match the downloaded CSV format
    for (col in names(preview_dat)) {
      if (inherits(preview_dat[[col]], c("POSIXct", "POSIXt", "Date"))) {
        preview_dat[[col]] <- format(
          preview_dat[[col]],
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )
      }
    }

    preview_dat
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("imputed_cgm_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      out <- imputed_data()
      req(out)

      data.table::fwrite(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
