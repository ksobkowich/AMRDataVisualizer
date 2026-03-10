#' UI for the import tab module.
#'
#' @param id  Module ID.
#' @return    Module UI.
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("importTabUI"))
  )
}

#' Server logic for the import tab module.
#'
#' @param id  The ID of the module.
#' @return    A list containing cleaned data, bp logs, guideline used, and type.
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------

    changeLogDataMo <- change_log$server(
      "moChangeLog",
      changeLogData = mo_change_log,
      cleanedData = cleanedData,
      availableData = availableData
    )
    changeLogDataAb <- change_log$server(
      "abChangeLog",
      changeLogData = ab_change_log,
      cleanedData = cleanedData,
      availableData = availableData,
      type = "antimicrobial"
    )

    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    upload <- reactiveValues(content = NULL, file = NULL, isWideFormat = FALSE)
    formattedData <- reactiveVal(NULL)
    cleanedData <- reactiveVal(NULL)
    verifiedData <- reactiveVal(NULL)
    wideData <- reactiveVal(FALSE)
    reactiveData <- reactiveVal(NULL)
    selections <- reactiveValues(
      idCol = NULL,
      dateCol = NULL,
      yearCol = NULL,
      monthCol = NULL,
      regionCol = NULL,
      subregionCol = NULL,
      speciesCol = NULL,
      sourceCol = NULL,
      valueType = "SIR",
      moCol = NULL,
      drugCol = NULL,
      sirCol = NULL,
      micSignCol = NULL,
      micValCol = NULL,
      selectedBreakpoint = "CLSI 2025",
      additionalCols = NULL
    )
    displayCleanedData <- reactiveVal(FALSE)

    mo_change_log <- reactiveVal(NULL)
    ab_change_log <- reactiveVal(NULL)
    bp_log <- reactiveVal(NULL)
    uti_log <- reactiveVal(NULL)

    # Define Available Data
    availableData <- reactive({
      data <- formattedData()
      if (is.null(data)) {
        return(NULL)
      }

      required_cols <- c("Microorganism", "Antimicrobial")
      if (selections$valueType == "SIR") {
        required_cols <- c(required_cols, "Interpretation")
      } else {
        required_cols <- c(required_cols, "Sign", "Value")
      }

      if (selections$valueType == "SIR") {
        filteredData <- data %>%
          filter(
            !is.na(Microorganism),
            !is.na(Antimicrobial),
            !is.na(Interpretation)
          )
      } else {
        filteredData <- data %>%
          filter(
            !is.na(Antimicrobial),
            !is.na(Sign) | !is.na(Value)
          )
      }
      filteredData$InternalID <- seq_len(nrow(filteredData))

      return(filteredData)
    })

    # Set "SIR" mode as default
    valueType <- reactive({
      if (is.null(input$valueType) || input$valueType == "") {
        "SIR"
      } else {
        input$valueType
      }
    })

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    # Main Dynamic UI
    output$importTabUI <- renderUI({
      ns <- session$ns

      ## If no file is uploaded or selected -------------------------------------
      if (is.null(upload$content)) {
        wellPanel(
          icon("file-import", class = "uploadIcon"),
          h3(typed("Ready to get started?", typeSpeed = 20)),
          hr(),
          fileInput(ns("fileUploader"), "Upload your own data", accept = c(".csv", ".parquet")),
          actionButton(
            ns("resetUploader"),
            "Reset file upload",
            icon("rotate-left"),
            class = "resetButton"
          ),
          br(),
          fluidRow(
            column(4, offset = 1, hr()),
            column(2, h4("or", style = "margin-top: 0px")),
            column(4, hr())
          ),
          br(),
          selectizeInput(
            ns("dataSelect"),
            "Browse available data",
            choices = c(
              "Select a dataset",
              "2020 NAHLN Canine - National Animal Health Laboratory Network" = "narms_2020.csv"
            ),
            selected = NULL
          ),
          class = "uploadWell",
          hr(),
          actionButton(ns("submit"), "Submit", class = "submitButton")
        )
      } else {
        ## If data have been processed --------------------------------------------
        if (displayCleanedData()) {
          tagList(
            fluidRow(
              style = "display: flex; align-items: stretch;",

              column(
                width = 8,
                actionButton(ns("clear"), "Clear Data", class = "clearButton"),
                br(),
                h3("Your data are ready for use!", style = "color: #44CDC4"),
                h5("Please select a tab from the side menu to explore your AMR data."),
                h5(
                  "If you would like to select a new dataset, or adjust your columns, use the buttons above."
                ),
                h6("Summary of data processing steps"),
                h6(
                  span(icon("circle-check", style = "color: #44CDC4;")),
                  "Formatted into long-data"
                ),
                h6(
                  span(icon("circle-check", style = "color: #44CDC4;")),
                  "Created dates"
                ),
                h6(
                  span(icon("circle-check", style = "color: #44CDC4;")),
                  "Standardized microorganism names"
                ),
                h6(
                  span(icon("circle-check", style = "color: #44CDC4;")),
                  "Standardized antimicrobial names"
                ),
                h6(
                  span(icon("circle-check", style = "color: #44CDC4;")),
                  "Assigned classes to antimicrobials"
                ),
                if (valueType() == "MIC") {
                  h6(
                    span(icon("circle-check", style = "color: #44CDC4;")),
                    paste("Interpretted MIC values according to", selections$selectedBreakpoint)
                  )
                },
                actionButton(ns("reopenLog"), "View Processing Log", class = "white-btn")
              ),

              column(
                width = 4,
                style = "display: flex; flex-direction: column; justify-content: flex-end;",

                wellPanel(
                  h4("Combine another cleaned file"),
                  fileInput(
                    inputId = ns("combineUploader"),
                    label = "Upload another cleaned file",
                    accept = c(".parquet")
                  ),
                  actionButton(
                    ns("combineSubmit"),
                    "Combine",
                    class = "dataSaveButton",
                    style = "margin-top: -20px"
                  ),
                  class = "contentWell",
                  style = "margin-top: 75px"
                )
              )
            ),

            br(),
            h4("Preview of Cleaned Data"),
            wellPanel(
              div(
                h5(
                  HTML(paste(
                    "<font color = #44CDC4>",
                    format(nrow(cleanedData()), big.mark = ","),
                    "</font> rows"
                  )),
                  align = "left"
                ),
                DTOutput(ns("cleanedDataPreview"))
              ),
              class = "dataPreviewWell"
            ),
            downloadButton(
              ns("downloadCleanedData"),
              "Download Cleaned Data",
              class = "dataSaveButton"
            )
          )
        } else {
          ## If cleaned data are uploaded -------------------------------------------
          if (verifiedData()) {
            cleanedData(upload$content)
            displayCleanedData(TRUE)
          } else {
            ## If data have been uploaded but not yet cleaned -------------------------
            tagList(
              actionButton(ns("clear"), "Clear Data", class = "clearButton"),
              h4("Raw Data"),
              wellPanel(
                h5(
                  HTML(paste(
                    "Your data has <font color = #44CDC4>",
                    format(nrow(upload$content), big.mark = ","),
                    "</font> rows, previewing the first 100."
                  )),
                  align = "left"
                ),
                hr(),
                div(
                  tableOutput(ns("rawDataPreview")),
                  class = "dataPreview"
                ),
                class = "dataPreviewWell"
              ),

              div(
                span(icon("chevron-down"), class = "icon"),
                actionButton(ns("assignMenu"), "Adjust Data Columns", class = "clearButton"),
                class = "container"
              ),
              h4("Formatted Data", style = "margin-top: -25px"),
              wellPanel(
                uiOutput(ns("availableRows")),
                hr(),
                div(
                  tableOutput(ns("availableDataPreview")),
                  class = "dataPreview"
                ),
                class = "dataPreviewWell"
              ),

              actionButton(ns("processData"), "Process Data", class = "processButton"),
            )
          }
        }
      }
    })

    # Preview of Cleaned Data
    output$cleanedDataPreview <- renderDT({
      req(cleanedData())
      data <- head(cleanedData(), 100) %>%
        .remove_extra_cols()
      DT::datatable(
        data,
        options = list(
          dom = 't',
          ordering = FALSE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        class = "table"
      )
    })

    # Preview raw data
    output$rawDataPreview <- renderTable({
      rawData <- head(upload$content, 100)
      req(!is.null(rawData))
      rawData <- rawData %>%
        mutate(across(everything(), as.character))
      rawData
    })

    # Number of Rows in Available Data
    output$availableRows <- renderUI({
      req(availableData())
      h5(
        HTML(paste(
          "<font color = #44CDC4>",
          format(nrow(availableData()), big.mark = ","),
          "</font> rows are suitable for processing"
        )),
        align = "left"
      )
    })

    # Preview of Available Data (Bottom Well)
    output$availableDataPreview <- renderTable({
      req(availableData())
      availableData <- availableData()

      if ("Date" %in% names(availableData)) {
        availableData <- availableData %>%
          mutate(Date = as.character(Date))
      }

      head(availableData, 100) %>%
        .remove_extra_cols()
    })

    # View first line of data to assist with wide data column selection
    output$rawDataPreview2 <- renderDT({
      abColsVal <- input$abCols
      col_indices <- numeric(0)

      if (!is.null(abColsVal) && nchar(trimws(abColsVal)) > 0) {
        col_indices <- parseColSpec(abColsVal)
      }

      valid_cols <- seq_len(ncol(upload$content))
      col_indices <- intersect(col_indices, valid_cols)

      data_preview <- head(upload$content, 1)
      # Convert all columns to character to avoid issues with datatable
      data_preview_char <- as.data.frame(
        lapply(data_preview, as.character),
        stringsAsFactors = FALSE
      )
      data_with_colnames <- rbind(colnames(upload$content), data_preview_char)
      colnames(data_with_colnames) <- seq_len(ncol(data_with_colnames))

      dt <- datatable(
        data_with_colnames,
        options = list(
          pageLength = 5,
          dom = 't',
          ordering = FALSE,
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        )
      )

      if (length(col_indices) > 0) {
        dt <- dt %>%
          formatStyle(
            columns = col_indices,
            backgroundColor = "#44CDC4",
            fontWeight = "bold"
          )
      }
      dt
    })

    output$interpretation_log <- DT::renderDataTable({
      data <- bp_log() %>%
        select(-method, -type, -site) # Remove hidden columns
      DT::datatable(
        data,
        rownames = FALSE,
        style = 'bootstrap',
        class = 'table-bordered',
        filter = "top",
        options = list(
          dom = 't',
          paging = F,
          ordering = T,
          scrollX = TRUE
        )
      ) %>%
        DT::formatStyle(
          "Interpretation",
          target = "cell",
          fontStyle = DT::styleEqual("Could not interpret", "italic"),
          color = DT::styleEqual("Could not interpret", "#666") # optional: muted grey
        )
    })

    output$uti_log <- DT::renderDataTable({
      DT::datatable(
        uti_log(),
        rownames = FALSE,
        style = 'bootstrap',
        class = 'table-bordered',
        filter = "top",
        options = list(
          dom = 't',
          paging = F,
          ordering = T,
          scrollX = TRUE
        )
      )
    })

    # Render input for additional columns
    output$additionalColsCheckbox <- renderUI({
      req(availableData())
      allColumns <- names(upload$content)
      assignedColumns <- c(
        selections$drugCol,
        selections$idCol,
        selections$micSignCol,
        selections$micValCol,
        selections$moCol,
        selections$monthCol,
        selections$regionCol,
        selections$selectedBreakpoint,
        selections$sirCol,
        selections$sourceCol,
        selections$speciesCol,
        selections$subregionCol,
        selections$valueType,
        selections$yearCol,
        selections$dateCol
      )
      unassignedColumns <- setdiff(allColumns, assignedColumns)

      tagList(
        h5(
          "Please select any additional columns to include with your data. These columns will be available as custom filters."
        ),
        h6(em("These columns will not be cleaned or standardized.")),
        prettyCheckboxGroup(
          ns("additionalCols"),
          "",
          choices = unassignedColumns,
          selected = selections$additionalCols
        )
      )
    })

    output$valueInputs <- renderUI({
      if (selections$valueType == "SIR") {
        if (wideData() == TRUE) {
          tagList(
            selectizeInput(
              ns("drugCol"),
              "Antimicrobial",
              choices = c("Not Present", names(reactiveData())),
              selected = selections$drugCol
            ),
            selectizeInput(
              ns("sirCol"),
              "Interpretations",
              choices = c("Not Present", names(reactiveData())),
              selected = selections$sirCol
            ),
            actionButton(
              ns("adjustWideCols"),
              "Adjust Test Result Columns",
              class = "clearButton",
              style = "margin-top: 25px;"
            )
          )
        } else {
          tagList(
            selectizeInput(
              ns("drugCol"),
              "Antimicrobial",
              choices = c("Not Present", names(reactiveData())),
              selected = selections$drugCol
            ),
            selectizeInput(
              ns("sirCol"),
              "Interpretations",
              choices = c("Not Present", names(reactiveData())),
              selected = selections$sirCol
            )
          )
        }
      } else {
        tagList(
          selectizeInput(
            ns("drugCol"),
            "Antimicrobial",
            choices = c("Not Present", names(reactiveData())),
            selected = selections$drugCol
          ),
          selectizeInput(
            ns("micSignCol"),
            "MIC Sign",
            choices = c("Not Present", names(reactiveData())),
            selected = selections$micSignCol
          ),
          selectizeInput(
            ns("micValCol"),
            "MIC Value",
            choices = c("Not Present", names(reactiveData())),
            selected = selections$micValCol
          ),
          selectizeInput(
            ns("selectedBreakpoint"),
            "Breakpoint",
            choices = unique(AMR::clinical_breakpoints$guideline),
            selected = selections$selectedBreakpoint
          )
        )
      }
    })

    # Wide-data modal logic
    output$abColsUI <- renderUI({
      if (is.null(input$dataFormat)) {
        return(NULL)

        # If long-data selected ---------------------------------------------------
      } else if (input$dataFormat == "Long") {
        tagList(
          h5("Thank you. You may close this dialog."),
          hr(),
          div(
            actionButton(ns("closeAbCols"), "Close", class = "clearButton"),
            style = "text-align: right;"
          )
        )

        # If wide-data selected ---------------------------------------------------
      } else {
        tagList(
          div(
            style = "width: 100%;",
            uiOutput(ns("abColWarning")),
            textInput(
              inputId = ns("abCols"),
              label = "Select columns containing your test results (e.g. 1,3,5,6-12)",
              value = NULL
            )
          ),
          br(),
          p(
            "Confirm that only your susceptibility test result columns are highlighted below. Adjust the column range above if needed."
          ),
          div(
            DTOutput(ns("rawDataPreview2")),
            style = "overflow-x: auto;"
          ),
          hr(),
          div(
            actionButton(
              ns("submitAbCols"),
              "Confirm columns",
              class = "submitButton"
            ),
            style = "text-align: right;"
          )
        )
      }
    })

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------

    #' Remove extra columns from data previews
    #'
    #' @note The "host" and "UTI" columns are used elsewhere so cannot be removed
    #' but we do not want them to show in data previews.
    #'
    #' @param data  Data frame to remove extra columns from
    #' @return      Data frame with extra columns removed
    .remove_extra_cols <- function(data) {
      if (length(data) == 0) {
        return(data)
      }
      data <- data %>%
        select(-any_of(c("InternalID", "host", "UTI")))
      return(data)
    }

    #' Get the modal dialog for wide-format data.
    #'
    #' @param ns  Namespace function
    #' @return    Modal dialog UI
    wideFormatModal <- function(ns) {
      modalDialog(
        title = "Attention",
        h4(
          "The uploaded file contains many columns, which suggests your data may be in wide format."
        ),
        h3("What is Wide-Format Data?"),
        h5(
          "Wide-format data has a single row for each sample submitted for testing, with each antimicrobial tested represented as a separate column on the same row."
        ),
        h3("What is Long-Format Data?"),
        h5(
          "Long-format data organizes each antimicrobial test result as a separate row, meaning a single sample may appear multiple times in the dataset—once for each antimicrobial tested."
        ),
        hr(),
        radioGroupButtons(
          inputId = ns("dataFormat"),
          label = "Which format of data are you using?",
          choices = c("Long", "Wide"),
          selected = character(0),
          justified = TRUE
        ),
        uiOutput(ns("abColsUI")),
        easyClose = FALSE,
        footer = NULL
      )
    }

    #' Processing log modal dialog
    #'
    #' @return Modal dialog UI
    processingLogModal <- function() {
      modalDialog(
        title = "Processing Log",
        size = "l",

        h5(
          "The following log details how your uploaded data were processed. During this step, entries were standardized based on taxonomic reference databases. This includes resolving known synonyms, flagging uncertain matches, and identifying any entries that could not be confidently matched. Review the sections below to understand what changes were made and why.",
          style = "text-align: center;"
        ),
        hr(),
        br(),
        div(
          id = "import-modal-footer",
          class = "align-end-row",
          div(
            class = "warning-parent",
            p(
              class = "antimicrobial-warning",
              "Please save changes made to the Antimicrobial change log"
            ),
            p(
              class = "microorganism-warning",
              "Please save changes made to the Microorganism change log"
            )
          ),
          downloadButton(ns("download_log"), "Download Log File", class = "changeLogButton white"),
          modalButton("Close")
        ),
        tabsetPanel(
          tabPanel("Microorganisms", change_log$ui(ns("moChangeLog"))),
          tabPanel("Antimicrobials", change_log$ui(ns("abChangeLog"))),
          if (!is.null(input$valueType) && input$valueType == "MIC") {
            tabPanel(
              "Interpretations",
              div(class = "readonly-table", DT::dataTableOutput(ns("interpretation_log")))
            )
          },
          if (!is.null(input$valueType) && input$valueType == "MIC") {
            tabPanel(
              "Is UTI?",
              div(class = "readonly-table", DT::dataTableOutput(ns("uti_log")))
            )
          }
        ),

        #' Cannot be easy close as changes need to be saved before closing for
        #' them to persist in the app.
        #' Instead move the close and download buttons to the top to the modal
        #' so the user does not need to scroll to the bottom.
        easyClose = FALSE,
        footer = NULL
      )
    }

    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    # Logic to disable dropdown menu if file is uploaded
    observe({
      if (!is.null(input$fileUploader$datapath)) {
        updateSelectInput(session, "dataSelect", selected = "Select a dataset")
        disable("dataSelect")
      } else {
        enable("dataSelect")
      }
    })

    # Assign read-in data to 'upload' variable
    observeEvent(input$fileUploader, {
      upload$file <- input$fileUploader
    })

    #' Read-in selected data (either upload or dropdown).
    #' Checks if "verified" (doesn't need to be cleaned).
    observeEvent(input$submit, {
      if (!is.null(upload$file)) {
        ext <- tools::file_ext(upload$file$name)
        upload$content <- switch(
          ext,
          "csv" = {
            data <- vroom(upload$file$datapath, delim = ",")
            verifiedData(FALSE) # Always set CSV as not verified
            data
          },
          "parquet" = {
            # Read the parquet
            data <- nanoparquet::read_parquet(upload$file$datapath)

            verified <- FALSE
            meta <- tryCatch(
              nanoparquet::read_parquet_metadata(upload$file$datapath),
              error = function(e) NULL
            )

            if (!is.null(meta)) {
              kv <- meta$file_meta_data$key_value_metadata
              if (!is.null(kv) && length(kv) >= 1) {
                kv1 <- kv[[1]]
                if (!is.null(kv1$key) && !is.null(kv1$value)) {
                  hit <- which(kv1$key == "verified")
                  if (length(hit)) {
                    val <- kv1$value[hit][1]
                    verified <- tolower(trimws(val)) %in% c("true", "1", "yes")
                  }
                }
              }
            }
            verifiedData(verified)

            data
          },
          NULL
        )
      } else if (input$dataSelect != "Select a dataset") {
        dataName <- input$dataSelect
        upload$content <- read.csv(paste("./Data/", dataName, sep = ""))

        verifiedData(FALSE)

        # Get latest NARMS data automatically
        # download.file("https://www.fda.gov/media/132928/download?attachment", "FDA_data.xlsx", mode = "wb")
        # data <- read_excel("FDA_data.xlsx")
      } else {
        showModal(modalDialog(
          title = "Error",
          "Please import a file or select a dataset before clicking submit.",
          easyClose = TRUE
        ))
        upload$content <- NULL
      }

      reactiveData(upload$content)
    })

    # Button logic ------------------------------------------------------------
    # Logic for "Clear" button
    observeEvent(input$clear, {
      upload$content <- NULL
      upload$file <- NULL
      reset("fileUploader")
      reset("dataSelect")
      enable("dataSelect")
      updateSelectInput(session, "dataSelect", selected = "Select a dataset")
      formattedData(NULL)
      cleanedData(NULL)
      displayCleanedData(FALSE)
      verifiedData(FALSE)
    })

    # Logic for "Reset" button
    observeEvent(input$resetUploader, {
      upload$content <- NULL
      upload$file <- NULL
      reset("fileUploader")
      enable("dataSelect")
    })

    # Logic for "Process Data" button
    observeEvent(input$processData, {
      req(availableData())

      # Reset the change log data
      mo_change_log(NULL)
      ab_change_log(NULL)
      bp_log(NULL)
      uti_log(NULL)
      showModal(modalDialog(
        title = tags$div(style = "text-align: center;", "Processing Your Data"),
        div(
          style = "text-align: center;",
          tags$img(src = "img/loading.gif", height = "150px"),
          h5("Please wait while we..."),
          tags$h4(id = "processingStep", "Standardize dates", style = "font-weight: 900;"),
          tags$div(
            style = "font-size: 14px; color: grey; margin-top: 10px;",
            "This may take a few moments depending on your dataset size."
          )
        ),
        tags$script(HTML(sprintf(
          "
    const steps = [
      'Standardize dates',
      'Harmonize antimicrobial names',
      'Assign antimicrobial classes',
      'Standardize microorganism names'%s
    ];
    let i = 0;
    setInterval(() => {
      document.getElementById('processingStep').innerText = steps[i];
      i = (i + 1) %% steps.length;
    }, 4000);
  ",
          if (is.null(input$valueType) || input$valueType != "SIR") {
            ", 'Interpret MIC values'"
          } else {
            ""
          }
        ))),
        tags$style(HTML(
          "
    .modal-content {
      border-radius: 15px;
      padding: 30px;
    }
    .modal-title {
      font-size: 22px;
      font-weight: bold;
    }
  "
        )),
        footer = NULL,
        easyClose = FALSE
      ))

      results <- dataCleaner(
        availableData(),
        additionalCols = selections$additionalCols,
        breakpoint = selections$selectedBreakpoint
      )
      mo_change_log(results$mo_log)
      ab_change_log(results$ab_log)
      bp_log(results$bp_log)
      uti_log(results$uti_log)
      cleanedData(results$cleaned_data)
      displayCleanedData(TRUE)
      removeModal()

      shinyjs::delay(500, {
        showModal(processingLogModal())
      })
    })

    observeEvent(input$reopenLog, {
      showModal(processingLogModal())
    })

    observe({
      req(input$sirCol)
      selections$idCol <- input$idCol
      selections$dateCol <- input$dateCol
      selections$yearCol <- input$yearCol
      selections$monthCol <- input$monthCol
      selections$regionCol <- input$regionCol
      selections$subregionCol <- input$subregionCol
      selections$speciesCol <- input$speciesCol
      selections$sourceCol <- input$sourceCol
      selections$valueType <- input$valueType
      selections$moCol <- input$moCol
      selections$drugCol <- input$drugCol
      selections$sirCol <- input$sirCol

      # req here to stop setting these as NULL if inputs are not yet created
      req(input$micSignCol, input$micValCol)
      selections$micSignCol <- input$micSignCol
      selections$micValCol <- input$micValCol
      selections$selectedBreakpoint <- input$selectedBreakpoint
    })

    observe({
      cols <- input$additionalCols
      selections$additionalCols <- cols %||% NULL
    })

    # Assign columns modal
    observeEvent(input$assignMenu, {
      showModal(modalDialog(
        title = "Data Configuration",
        wellPanel(
          tabsetPanel(
            ## Main configuration tab -------------------------------------------------
            tabPanel(
              title = "Main Configuration",
              fluidRow(
                column(
                  6,
                  h5("Patient Information"),
                  hr(),
                  selectizeInput(
                    ns("idCol"),
                    "ID",
                    choices = c("Not Present", names(reactiveData())),
                    selected = selections$idCol
                  ),
                  selectizeInput(
                    ns("dateCol"),
                    "Date",
                    choices = c("Not Present", names(reactiveData())),
                    selected = selections$dateCol
                  ),

                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'Not Present'", ns("dateCol")),
                    selectizeInput(
                      ns("yearCol"),
                      "Year",
                      choices = c("Not Present", names(reactiveData())),
                      selected = selections$yearCol
                    ),
                    selectizeInput(
                      ns("monthCol"),
                      "Month",
                      choices = c("Not Present", names(reactiveData())),
                      selected = selections$monthCol
                    )
                  ),

                  selectizeInput(
                    ns("regionCol"),
                    "Region",
                    choices = c("Not Present", names(reactiveData())),
                    selected = selections$regionCol
                  ),
                  selectizeInput(
                    ns("subregionCol"),
                    "Subregion",
                    choices = c("Not Present", names(reactiveData())),
                    selected = selections$subregionCol
                  ),
                  selectizeInput(
                    ns("speciesCol"),
                    "Species",
                    choices = c("Not Present", names(reactiveData())),
                    selected = selections$speciesCol
                  ),
                  selectizeInput(
                    ns("sourceCol"),
                    "Sampling Site",
                    choices = c("Not Present", names(reactiveData())),
                    selected = selections$sourceCol
                  )
                ),
                column(
                  6,
                  h5("Microorganism Information"),
                  hr(),
                  radioGroupButtons(
                    ns("valueType"),
                    "Data Format",
                    choices = c("SIR", "MIC"),
                    selected = selections$valueType,
                    justified = TRUE
                  ),
                  selectizeInput(
                    ns("moCol"),
                    "Microorganism",
                    choices = c("Not Present", names(reactiveData())),
                    selected = selections$moCol
                  ),
                  uiOutput(ns("valueInputs"))
                )
              ),
              class = "colAssignWell"
            ),

            ## Additional columns tab -------------------------------------------------
            tabPanel(
              title = "Additional Columns",
              br(),
              uiOutput(ns("additionalColsCheckbox"))
            )
          ),
          style = "background-color: white;"
        ),
        size = "m",
        easyClose = TRUE,
        footer = tagList(modalButton("Close"))
      ))
    })

    observe({
      req(selections$additionalCols)
      updateCheckboxGroupInput(
        session,
        "additionalCols",
        selected = selections$additionalCols
      )
    })

    # Make column assignment guesses
    observeEvent(upload$content, {
      req(upload$content)
      selections$idCol <- detectIdColumn(upload$content) %||% "Not Present"
      selections$dateCol <- detectDateColumn(upload$content) %||% "Not Present"
      selections$yearCol <- detectYearColumn(upload$content) %||% "Not Present"
      selections$monthCol <- detectMonthColumn(upload$content) %||% "Not Present"
      selections$regionCol <- detectRegionColumn(upload$content) %||% "Not Present"
      selections$subregionCol <- detectSubregionColumn(upload$content) %||% "Not Present"
      selections$speciesCol <- detectSpeciesColumn(upload$content) %||% "Not Present"
      selections$sourceCol <- detectSourceColumn(upload$content) %||% "Not Present"
      selections$moCol <- detectMoColumn(upload$content) %||% "Not Present"
      selections$drugCol <- detectDrugColumn(upload$content) %||% "Not Present"
      selections$sirCol <- detectSIRColumn(upload$content) %||% "Not Present"
      selections$micSignCol <- detectMICSignColumn(upload$content) %||% "Not Present"
      selections$micValCol <- detectMICValueColumn(upload$content) %||% "Not Present"
    })

    # Trigger Wide Data Modal when Data > 16 columns or Manually
    observeEvent(upload$content, {
      req(upload$content)

      if (ncol(upload$content) > 16) {
        showModal(wideFormatModal(ns))
      }
    })

    observeEvent(input$adjustWideCols, {
      showModal(wideFormatModal(ns))
    })

    # Store data format as "Wide" or "Long"
    observe({
      req(input$dataFormat)
      if (input$dataFormat == "Wide") {
        wideData(TRUE)
      } else {
        wideData(FALSE)
      }
    })

    # Long-data modal "Close" button logic
    observeEvent(input$closeAbCols, {
      removeModal()
    })

    # Logic for "Submit wide data columns" button
    observeEvent(input$submitAbCols, {
      if (!is.null(input$abCols) && nchar(trimws(input$abCols)) > 0) {
        showModal(modalDialog(
          title = "Processing...",
          "Please wait while we prepare your data.",
          footer = NULL,
          easyClose = FALSE
        ))

        req(reactiveData(), upload$content)

        data <- reactiveData()
        abColsVal <- input$abCols
        col_indices <- parseColSpec(abColsVal)

        valid_cols <- seq_len(ncol(upload$content))
        col_indices <- intersect(col_indices, valid_cols)

        if (length(col_indices) > 0) {
          long_df <- getLongData(data, col_indices)

          selections$drugCol <- "Antimicrobial"
          selections$sirCol <- ifelse(
            "Interpretation" %in% colnames(long_df),
            "Interpretation",
            "Not Present"
          )
          selections$micSignCol <- ifelse(
            "MIC_Sign" %in% colnames(long_df),
            "MIC_Sign",
            "Not Present"
          )
          selections$micValCol <- ifelse(
            "MIC_Value" %in% colnames(long_df),
            "MIC_Value",
            "Not Present"
          )

          reactiveData(long_df)
        } else {
          output$abColWarning <- renderUI({
            h6("No valid columns were selected. Please revise your input.")
          })
        }

        removeModal()
      } else {
        output$abColWarning <- renderUI({
          h6("Select at least 1 column to proceed.", style = "color: red;")
        })
      }
    })

    # Define Formatted Data
    observe({
      req(reactiveData())
      req(valueType())

      data <- reactiveData()

      #' TODO: Documentation
      #' [Summary]
      #'
      #' @param colName [Description]
      #' @return [Description]
      safeExtract <- function(colName) {
        if (colName %in% names(data) && !is.null(colName) && colName != "Not Present") {
          return(data[[colName]])
        } else {
          return(rep(NA, nrow(data)))
        }
      }

      date_present <- selections$dateCol != "Not Present"

      column_list <- list(
        ID = safeExtract(selections$idCol)
      )

      if (selections$dateCol != "Not Present") {
        column_list <- append(
          column_list,
          list(Date = safeExtract(selections$dateCol)),
          after = 1
        )
      } else {
        column_list <- append(
          column_list,
          list(
            Year = safeExtract(selections$yearCol),
            Month = safeExtract(selections$monthCol)
          ),
          after = 1
        )
      }

      column_list <- append(
        column_list,
        list(
          Region = safeExtract(selections$regionCol),
          Subregion = safeExtract(selections$subregionCol),
          Species = safeExtract(selections$speciesCol),
          Source = safeExtract(selections$sourceCol),
          Microorganism = safeExtract(selections$moCol),
          Antimicrobial = safeExtract(selections$drugCol)
        )
      )

      formattedDataFrame <- as.data.frame(column_list)

      if (!is.null(selections$additionalCols) && length(selections$additionalCols) > 0) {
        for (col in selections$additionalCols) {
          formattedDataFrame[[col]] <- safeExtract(col)
        }
      }

      if (valueType() == "SIR") {
        formattedDataFrame$Interpretation <- safeExtract(selections$sirCol)
      } else {
        formattedDataFrame$Sign <- safeExtract(selections$micSignCol)
        formattedDataFrame$Value <- safeExtract(selections$micValCol)
      }

      formattedData(formattedDataFrame)
    })

    # Combine Cleaned Data
    observeEvent(input$combineSubmit, {
      if (is.null(input$combineUploader)) {
        showModal(modalDialog(
          title = "Error",
          "Please select a file to be combined.",
          easyClose = TRUE
        ))
        return(NULL)
      }

      parquet_file <- read_parquet(input$combineUploader$datapath)
      metadata <- parquet_metadata(input$combineUploader$datapath)

      verified_val <- metadata$file_meta_data$key_value_metadata[[1]]$value[
        metadata$file_meta_data$key_value_metadata[[1]]$key == "verified"
      ]

      if (length(verified_val) > 0 && verified_val == "TRUE") {
        combined_df <- dplyr::bind_rows(cleanedData(), parquet_file)
        cleanedData(combined_df)
      } else {
        showModal(modalDialog(
          title = "Error",
          "This file has not been previously cleaned. Please process all files before attempting to combine.",
          easyClose = TRUE
        ))
      }
    })

    # ------------------------------------------------------------------------------
    # Download Handlers
    # ------------------------------------------------------------------------------

    # Download Cleaned Data
    output$downloadCleanedData <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_CleanedAMRVisualizerData.parquet")
      },
      content = function(file) {
        data <- cleanedData()
        verified <- c("verified" = "TRUE")
        nanoparquet::write_parquet(data, metadata = verified, file)
      }
    )

    # Download Processing Log
    output$download_log <- downloadHandler(
      filename = "ProcessingLog.html",
      content = function(file) {
        withProgress(message = 'Rendering, please wait!', {
          src <- normalizePath("./Reports/ProcessingLog.rmd")
          tmp <- tempdir()
          unlink(list.files(tmp, full.names = TRUE), recursive = TRUE, force = TRUE)

          owd <- setwd(tempdir())
          on.exit({
            setwd(owd)
            unlink(c("ProcessingLog.rmd"), recursive = TRUE)
          })
          file.copy(src, "ProcessingLog.rmd", overwrite = TRUE)

          render_env <- new.env()
          render_env$mo_change_log <- changeLogDataMo()
          render_env$ab_change_log <- changeLogDataAb()
          render_env$bp_log <- bp_log()
          render_env$uti_log <- uti_log()

          rmarkdown::render(
            input = "ProcessingLog.rmd",
            output_format = "html_document",
            output_file = "ProcessingLog.html",
            envir = render_env
          )

          file.rename("ProcessingLog.html", file)
        })
      }
    )

    # ------------------------------------------------------------------------------
    # Module return
    # ------------------------------------------------------------------------------

    return(
      list(
        data = cleanedData,
        guideline = reactive({
          selections$selectedBreakpoint
        }),
        mic_or_sir = reactive({
          selections$valueType
        }),
        bp_log = bp_log
      )
    )
  })
}

import_tab <- list(
  ui = ui,
  server = server
)
