# Module UI ---------------------------------------------------------------
importDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    uiOutput(ns("importTabUI"))
  )
}

# Module Server -----------------------------------------------------------
importDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
# Initiate reactive variables ---------------------------------------------
    upload <- reactiveValues(content = NULL, file = NULL)
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
      selectedBreakpoint = NULL,
      additionalCols = NULL
    )
    displayCleanedData <- reactiveVal(FALSE)
    log <- reactiveVal()

# Logic to disable dropdown menu if file is uploaded ----------------------
    observe({
      if (!is.null(input$fileUploader$datapath)) {
        updateSelectInput(session, "dataSelect", selected = "Select a dataset")
        disable("dataSelect")
      } else {
        enable("dataSelect")
      }
    })

# Assign read-in data to 'upload' variable ----------------------------------
    observeEvent(input$fileUploader, {
      upload$file <- input$fileUploader
    })

# Read-in selected data (either upload or dropdown) -----------------------
##Checks if "verified" (doesn't need to be cleaned) -----------------------
    observeEvent(input$submit, {
        if (!is.null(upload$file)) {
          ext <- file_ext(upload$file$name)
          upload$content <- switch(ext,
                                   "csv" = {
                                     data <- vroom(upload$file$datapath, delim = ",")
                                     verifiedData(FALSE)  # Always set CSV as not verified
                                     data
                                   },
                                   "parquet" = {
                                     parquet_file <- read_parquet(upload$file$datapath)
                                     metadata <- parquet_metadata(upload$file$datapath)
                                     # Check if 'verified' key exists and if its value is TRUE
                                     verified <- metadata$file_meta_data$key_value_metadata[[1]]$value[metadata$file_meta_data$key_value_metadata[[1]]$key == "verified"]
                                     # Store if data is verified
                                     if (length(verified) > 0 && verified == "TRUE") {
                                       verifiedData(TRUE)
                                     } else {
                                       verifiedData(FALSE)
                                     }
                                     parquet_file  # Use only the data for the 'content'
                                   },
                                   NULL)
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
## Logic for "Clear" button -----------------------------------------------
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
    
## Logic for "Reset" button -----------------------------------------------
    observeEvent(input$resetUploader, {
      upload$content <- NULL
      upload$file <- NULL
      reset("fileUploader")
      enable("dataSelect")
    })
    
## Logic for "Process Data" button ----------------------------------------
    observeEvent(input$processData, {
      req(availableData())
      
      showModal(modalDialog(
        title = "Processing Data",
        h4("Your data are being processed, please wait."),
        br(), br(),
        div(
          style = "font-size: 24px; text-align: center; padding-bottom: 75px;",
          "Loading", span(class = "dot", "."), span(class = "dot", "."), span(class = "dot", ".")
        ),
        br(),
        h5("After your data are processed, you will have the option to download the processed version for future use. If you upload data that have already been processed through the data visualizer, this processing step will automatically be skipped."),
        tags$style(HTML("
      @keyframes dot {
        0% { opacity: 0; }
        20% { opacity: 1; }
        40% { opacity: 0; }
      }
      .dot:nth-child(1) { animation: dot 1s infinite 0.2s; }
      .dot:nth-child(2) { animation: dot 1s infinite 0.4s; }
      .dot:nth-child(3) { animation: dot 1s infinite 0.6s; }
    ")),
        footer = NULL,
        easyClose = FALSE
      ))
      
      results <- dataCleaner(availableData(), additionalCols = selections$additionalCols)
      cleanedData(results$cleaned_data)
      displayCleanedData(TRUE)
      removeModal()
      
      renamed_log <- if (is.null(results$mo_renamed) || nrow(results$mo_renamed) < 1) {
        "No renamed microorganisms were found."
      } else {
        paste0(
          "* ", 
          results$mo_renamed$old, 
          " (", results$mo_renamed$ref_old, ")  ->  ", 
          results$mo_renamed$new, 
          " (", results$mo_renamed$ref_new, ")"
        )
      }
      
      uncertain_log <- capture.output(print(results$mo_uncertainties, n = Inf))
      
      failure_log <- if (is.null(results$mo_failures) || length(results$mo_failures) < 1) {
        "No failures."
      } else {
        results$mo_failures
      }
      
      shinyjs::delay(500, {
        showModal(modalDialog(
          title = "Processing Log",
          size = "l",
          
          h5("The following log details how your uploaded data were processed. During this step, entries were standardized based on taxonomic reference databases. This includes resolving known synonyms, flagging uncertain matches, and identifying any entries that could not be confidently matched. Review the sections below to understand what changes were made and why.",
             style = "text-align: center;"
          ),
          hr(), br(),
          
          tabsetPanel(
            tabPanel("Microorganisms",
                     tabsetPanel(type = "tabs",
                                 tabPanel("Change Log",
                                          pre(paste(
                                            sprintf("%-50s %s %s", results$mo_log$Microorganism, "→", results$mo_log$mo_name),
                                            collapse = "\n"
                                          ))
                                 ),
                                 tabPanel("Uncertainties",
                                          pre(paste(uncertain_log, collapse = "\n"))
                                 ),
                                 tabPanel("Failures",
                                          pre(paste(failure_log, collapse = "\n"))
                                 ),
                                 tabPanel("Renames",
                                          pre(paste(renamed_log, collapse = "\n"))
                                 )
                     )
            ),
            tabPanel("Antimicrobials",
                     tabsetPanel(type = "tabs",
                                 tabPanel("Change Log",
                                          pre(paste(
                                            sprintf("%-50s %s %s", results$ab_log$Antimicrobial, "→", results$ab_log$ab_name),
                                            collapse = "\n"
                                          ))
                                 )
                     )
            )
          ),
          
          easyClose = FALSE,
          footer = modalButton("Close")
        ))
      })
      
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
      selections$micSignCol <- input$micSignCol
      selections$micValCol <- input$micValCol
    })
    
    observe({
      cols <- input$additionalCols
      selections$additionalCols <- cols %||% NULL
    })
    
# Assign columns modal ----------------------------------------------------
    observeEvent(input$assignMenu, {
      showModal(modalDialog(
        title = "Data Configuration",
        wellPanel(
          tabsetPanel(
            
## Main configuration tab -------------------------------------------------
            tabPanel(
              title = "Main Configuration",
              fluidRow(
                column(6,
                       h5("Patient Information"),
                       hr(),
                       selectizeInput(ns("idCol"), "ID", choices = c("Not Present", names(reactiveData())), selected = selections$idCol),
                       selectizeInput(ns("dateCol"), "Date", choices = c("Not Present", names(reactiveData())), selected = selections$dateCol),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'Not Present'", ns("dateCol")),
                         selectizeInput(ns("yearCol"), "Year", choices = c("Not Present", names(reactiveData())), selected = selections$yearCol),
                         selectizeInput(ns("monthCol"), "Month", choices = c("Not Present", names(reactiveData())), selected = selections$monthCol)
                       ),
                       
                       selectizeInput(ns("regionCol"), "Region", choices = c("Not Present", names(reactiveData())), selected = selections$regionCol),
                       selectizeInput(ns("subregionCol"), "Subregion", choices = c("Not Present", names(reactiveData())), selected = selections$subregionCol),
                       selectizeInput(ns("speciesCol"), "Species", choices = c("Not Present", names(reactiveData())), selected = selections$speciesCol),
                       selectizeInput(ns("sourceCol"), "Sampling Site", choices = c("Not Present", names(reactiveData())), selected = selections$sourceCol)
                ),
                column(6,
                       h5("Microorganism Information"),
                       hr(),
                       radioGroupButtons(ns("valueType"), "Data Format", choices = c("SIR", "MIC"), selected = selections$valueType, justified = TRUE),
                       selectizeInput(ns("moCol"), "Microorganism", choices = c("Not Present", names(reactiveData())), selected = selections$moCol),
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
    
# Render input for additional columns -------------------------------------
    output$additionalColsCheckbox <- renderUI({
      req(availableData())
      allColumns <- names(upload$content)
      assignedColumns <- c(
        selections$drugCol, selections$idCol, selections$micSignCol, selections$micValCol, 
        selections$moCol, selections$monthCol, selections$regionCol, selections$selectedBreakpoint, 
        selections$sirCol, selections$sourceCol, selections$speciesCol, selections$subregionCol, 
        selections$valueType, selections$yearCol, selections$dateCol
      )
      unassignedColumns <- setdiff(allColumns, assignedColumns)

      tagList(
        h5("Please select any additional columns to include with your data. These columns will be available as custom filters."),
        h6(em("These columns will not be cleaned or standardized.")),
        prettyCheckboxGroup(ns("additionalCols"), "", choices = unassignedColumns, selected = selections$additionalCols)
      )
    })
    
    observe({
      req(selections$additionalCols)
      updateCheckboxGroupInput(
        session, 
        "additionalCols", 
        selected = selections$additionalCols
      )
    })
    
    output$valueInputs <- renderUI({
      if (selections$valueType == "SIR") {
        
        if(wideData() == TRUE){
          tagList(
            selectizeInput(ns("drugCol"), "Antimicrobial", choices = c("Not Present", names(reactiveData())), selected = selections$drugCol),
            selectizeInput(ns("sirCol"), "Interpretations", choices = c("Not Present", names(reactiveData())), selected = selections$sirCol),
            actionButton(ns("adjustWideCols"), "Adjust Test Result Columns", class = "clearButton", style = "margin-top: 25px;")
          )
        } else {
          tagList(
            selectizeInput(ns("drugCol"), "Antimicrobial", choices = c("Not Present", names(reactiveData())), selected = selections$drugCol),
            selectizeInput(ns("sirCol"), "Interpretations", choices = c("Not Present", names(reactiveData())), selected = selections$sirCol)
          )
        }
        
      } else {
        tagList(
          selectizeInput(ns("drugCol"), "Antimicrobial", choices = c("Not Present", names(reactiveData())), selected = selections$drugCol),
          selectizeInput(ns("micSignCol"), "MIC Sign", choices = c("Not Present", names(reactiveData())), selected = selections$micSignCol),
          selectizeInput(ns("micValCol"), "MIC Value", choices = c("Not Present", names(reactiveData())), selected = selections$micValCol),
          selectizeInput(ns("selectedBreakpoint"), "Breakpoint", choices = unique(AMR::clinical_breakpoints$guideline), selected = selections$selectedBreakpoint)
        )
      }
    })
  
# Main Dynamic UI ---------------------------------------------------------
    output$importTabUI <- renderUI({
      ns <- session$ns
      
## If no file is uploaded or selected -------------------------------------
      if (is.null(upload$content)) {
        wellPanel(
          icon("file-import", class = "uploadIcon"),
          h3(typed("Ready to get started?", typeSpeed = 20)),
          hr(),
          fileInput(ns("fileUploader"), "Upload your own data", accept = c(".csv", ".parquet")),
          actionButton(ns("resetUploader"), "Reset file upload", icon("rotate-left"), class = "resetButton"),
          br(),
          fluidRow(
            column(4, offset = 1, hr()),
            column(2, h4("or", style = "margin-top: 0px")),
            column(4, hr())
          ),
          br(),
          selectizeInput(ns("dataSelect"), "Browse available data", choices = c("Select a dataset", "2020 NARMS - National Antimicrobial Resistance Monitoring System" = "narms_2020.csv"), selected = NULL),
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
                h5("If you would like to select a new dataset, or adjust your columns, use the buttons above."),
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
                )
              ),
              
              column(
                width = 4,
                style = "display: flex; flex-direction: column; justify-content: flex-end;",
                
                wellPanel(
                  h4("Combine another cleaned file"),
                  fileInput(
                    inputId = ns("combineUploader"),
                    label   = "Upload another cleaned file",
                    accept  = c(".parquet")
                  ),
                  actionButton(ns("combineSubmit"), "Combine", class = "dataSaveButton", style = "margin-top: -20px"),
                  class = "contentWell", style = "margin-top: 75px"
                )
              )
            ),
            
            br(),
            h4("Preview of Cleaned Data"),
            wellPanel(
              div(
                h5(HTML(paste("<font color = #44CDC4>", format(nrow(cleanedData()), big.mark = ","), "</font> rows")), align = "left"),
                DTOutput(ns("cleanedDataPreview"))
              ),
              class = "dataPreviewWell"
            ),
            downloadButton(ns("downloadCleanedData"), "Download Cleaned Data", class = "dataSaveButton")
            
          )
          
        } else {

## If cleaned data are uploaded -------------------------------------------
          if(verifiedData()){
            cleanedData(upload$content)
            displayCleanedData(TRUE)
            
          } else {
            

## If data have been uploaded but not yet cleaned -------------------------
        tagList(
          actionButton(ns("clear"), "Clear Data", class = "clearButton"),
          h4("Raw Data"),
          wellPanel(
            h5(HTML(paste("Your data has <font color = #44CDC4>", format(nrow(upload$content), big.mark = ","), "</font> rows, previewing the first 100.")), align = "left"),
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

# Preview raw data --------------------------------------------------------
    output$rawDataPreview <- renderTable({
      
      rawData <- head(upload$content, 100)
      rawData <- rawData %>% 
        mutate(across(everything(), as.character))
      rawData

    })
    

# Set "SIR" mode as default -----------------------------------------------
    valueType <- reactive({
      if(is.null(input$valueType) || input$valueType == "") {
        "SIR"
      } else {
        input$valueType
      }
    })

# Make column assignment guesses ------------------------------------------
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
    })
    

# If wide-data is detected ------------------------------------------------
    wideFormatModal <- function(ns) {
      modalDialog(
        title = "Attention",
        h4("The uploaded file contains many columns, which suggests your data may be in wide format."),  
        
        h3("What is Wide-Format Data?"),  
        h5("Wide-format data has a single row for each sample submitted for testing, with each antimicrobial tested represented as a separate column on the same row."),  
        
        h3("What is Long-Format Data?"),  
        h5("Long-format data organizes each antimicrobial test result as a separate row, meaning a single sample may appear multiple times in the dataset—once for each antimicrobial tested."), 
        
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
    

# Trigger Wide Data Modal when Data > 16 columns or Manually --------------
    observeEvent(upload$content, {
      req(upload$content)
      
      if (ncol(upload$content) > 16) {
        showModal(wideFormatModal(ns))
      }
    })
    
    observeEvent(input$adjustWideCols, {
      showModal(wideFormatModal(ns))
    })
    
# Store data format as "Wide" or "Long" -----------------------------------
    observe({
      req(input$dataFormat)
      if(input$dataFormat == "Wide"){
        wideData(TRUE)
      } else {
        wideData(FALSE)
      }
    })
    
# Wide-data modal logic ---------------------------------------------------
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
          p("Confirm that only your susceptibility test result columns are highlighted below. Adjust the column range above if needed."),
          div(
            DTOutput(ns("rawDataPreview2")),
            style = "overflow-x: auto;"
          ),
          hr(),
          div(
            actionButton(ns("submitAbCols"), "Confirm columns", class = "submitButton"),
            style = "text-align: right;"
          )
        )
      }
    })

# View first line of data to assist with wide data column selection -------
    output$rawDataPreview2 <- renderDT({
      abColsVal <- input$abCols
      col_indices <- numeric(0)
      
      if (!is.null(abColsVal) && nchar(trimws(abColsVal)) > 0) {
        col_indices <- parseColSpec(abColsVal)
      }
      
      valid_cols  <- seq_len(ncol(upload$content))
      col_indices <- intersect(col_indices, valid_cols)
      
      data_preview         <- head(upload$content, 1)
      data_with_colnames   <- rbind(colnames(upload$content), data_preview)
      colnames(data_with_colnames) <- seq_len(ncol(data_with_colnames))
      
      dt <- datatable(
        data_with_colnames,
        options = list(
          pageLength = 5,
          dom        = 't',
          ordering   = FALSE, 
          searching  = FALSE,
          paging     = FALSE,
          info       = FALSE
        )
      )
      
      if (length(col_indices) > 0) {
        dt <- dt %>%
          formatStyle(
            columns         = col_indices,
            backgroundColor = "#44CDC4",
            fontWeight      = "bold"
          )
      }
      
      dt
    })

# Long-data modal "Close" button logic ------------------------------------
    observeEvent(input$closeAbCols, {
      removeModal()
    })
    
# Logic for "Submit wide data columns" button -----------------------------
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
          long_df <- upload$content %>%
            pivot_longer(
              cols = all_of(col_indices),
              names_to = "Antimicrobial",
              values_to = "Interpretation"
            )
        
          selections$drugCol <- "Antimicrobial"
          selections$sirCol <- "Interpretation" %||% detectSIRColumn(upload$content)
          
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
    

# Define Formatted Data ---------------------------------------------------
    observe({
      req(reactiveData())
      req(valueType())
      
      data <- reactiveData()

      safeExtract <- function(colName) {
        if(colName %in% names(data) && !is.null(colName) && colName != "Not Present") {
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
    
# Define Available Data ---------------------------------------------------
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
            !is.na(Sign),
            !is.na(Value)
          )
      }
      
      return(filteredData)
    })
    
# Number of Rows in Available Data ----------------------------------------
    
    output$availableRows <- renderUI({
      req(availableData())
      h5(HTML(paste("<font color = #44CDC4>", format(nrow(availableData()), big.mark = ","), "</font> rows are suitable for processing")), align = "left")
    })
    
# Preview of Available Data (Bottom Well) ---------------------------------
    output$availableDataPreview <- renderTable({
      req(availableData())
      availableData <- availableData()
      
      if ("Date" %in% names(availableData)) {
        availableData <- availableData %>%
          mutate(Date = as.character(Date))
      }
      
      head(availableData, 100)
    })
    
# Preview of Cleaned Data -------------------------------------------------
    output$cleanedDataPreview <- renderDT({
      req(cleanedData())
      DT::datatable(head(cleanedData(), 100), 
                    options = list(
                      dom = 't',
                      ordering = FALSE,
                      autoWidth = TRUE 
                    ),
                    rownames = FALSE,
                    class = "table"
      )
      
    })
    
# Download Cleaned Data ---------------------------------------------------
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

# Combine Cleaned Data ----------------------------------------------------
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
      metadata     <- parquet_metadata(input$combineUploader$datapath)
      
      verified_val <- metadata$file_meta_data$key_value_metadata[[1]]$value[metadata$file_meta_data$key_value_metadata[[1]]$key == "verified"]
      
      if (length(verified_val) > 0 && verified_val == "TRUE") {
        
        combined_df <- dplyr::bind_rows(cleanedData(), parquet_file)
        cleanedData(combined_df)
        
      } else {

        showModal(modalDialog(
          title = "Error",
          "This file has not been previously cleaned. Please process all files before attepting to combine.",
          easyClose = TRUE
        ))
      }
    })
    
    return(
      data = reactive({ cleanedData() })
    )
    
  })
}
