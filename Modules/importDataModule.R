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
    selections <- reactiveValues(
      idCol = NULL, 
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
    

# Logic to disable dropdown menu if file is uploaded ----------------------
    observe({
      if (!is.null(input$fileUploader$datapath)) {
        updateSelectInput(session, "dataSelect", selected = "Select a dataset")
        disable("dataSelect")
      } else {
        enable("dataSelect")
      }
    })

# Assign read-in data to upload variable ----------------------------------
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
                                     parquet_file  # Use only the data for the content
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
        br(),
        br(),
        div(
          style = "font-size: 24px; text-align: center; padding-bottom: 75px;",
          "Loading",
          span(class = "dot", "."), span(class = "dot", "."), span(class = "dot", ".")
        ),
        br(),
        h5("After your data are processed, you will have the option to download the processed version for future use. If you upload data that have already been processed through the data visualizer, this processing step will automatically be skipped."),
        tags$style(
          HTML("
          @keyframes dot {
            0% { opacity: 0; }
            20% { opacity: 1; }
            40% { opacity: 0; }
          }
          .dot:nth-child(1) { animation: dot 1s infinite 0.2s; }
          .dot:nth-child(2) { animation: dot 1s infinite 0.4s; }
          .dot:nth-child(3) { animation: dot 1s infinite 0.6s; }
        ")
        ),
        footer = NULL
      ))
    
      cleanedData(dataCleaner(availableData(), additionalCols = selections$additionalCols))
      displayCleanedData(TRUE)
      removeModal()
    })
    
    
    observe({
      req(input$sirCol)
      selections$idCol <- input$idCol
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
    
    observeEvent(input$additionalCols, {
      selections$additionalCols <- input$additionalCols
    })
    
    output$valueInputs <- renderUI({
      if (selections$valueType == "SIR") {
        selectizeInput(ns("sirCol"), "Interpretations", choices = c("Not Present", names(reactiveData())), selected = selections$sirCol)
      } else {
        tagList(
          selectizeInput(ns("micSignCol"), "MIC Sign", choices = c("Not Present", names(reactiveData())), selected = selections$micSignCol),
          selectizeInput(ns("micValCol"), "MIC Value", choices = c("Not Present", names(reactiveData())), selected = selections$micValCol),
          selectizeInput(ns("selectedBreakpoint"), "Breakpoint", choices = unique(AMR::clinical_breakpoints$guideline), selected = selections$selectedBreakpoint)
        )
      }
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
                       selectizeInput(ns("yearCol"), "Year", choices = c("Not Present", names(reactiveData())), selected = selections$yearCol),
                       selectizeInput(ns("monthCol"), "Month", choices = c("Not Present", names(reactiveData())), selected = selections$monthCol),
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
                       selectizeInput(ns("drugCol"), "Antimicrobial", choices = c("Not Present", names(reactiveData())), selected = selections$drugCol),
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
        selections$valueType, selections$yearCol
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
      head(upload$content, 100)
    })
    
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
    observeEvent(upload$content, {
      req(upload$content)
      
      if (ncol(upload$content) > 30) {
        showModal(modalDialog(
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
            choices = c("Long", 
                        "Wide"),
            selected = character(0),
            justified = T
          ),
          
          uiOutput(ns("abColsUI")),
          easyClose = FALSE,
          footer = NULL
        ))
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
            textInput(
              inputId = ns("abCols"),
              label   = "Select columns containing your test results (e.g. 1,3,5,6-12)",
              value   = NULL
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

# Long-data modal "Close" button logic ------------------------------------
    observeEvent(input$closeAbCols, {
      removeModal()
    })
    
    reactiveData <- reactiveVal()
    observeEvent(upload$content, {
      req(upload$content)
      reactiveData(upload$content)  # Store raw uploaded data
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    

# Logic for "Submit wide data columns" button -----------------------------
     observeEvent(input$submitAbCols, {
      showModal(modalDialog(
        title = "Processing...",
        "Please wait while we prepare your data.",
        footer = NULL,
        easyClose = FALSE
      ))
      req(reactiveData())
      data <- reactiveData()
      
      abColsVal <- input$abCols
      col_indices <- numeric(0)
      
      if (!is.null(abColsVal) && nchar(trimws(abColsVal)) > 0) {
        col_indices <- parseColSpec(abColsVal)
      }
      
      valid_cols  <- seq_len(ncol(upload$content))
      col_indices <- intersect(col_indices, valid_cols)
      
      long_df <- data %>%
        pivot_longer(
          cols = col_indices, 
          names_to = "Antimicrobial",
          values_to = "Value"
        )
      reactiveData(long_df)
      selections$drugCol <- "Antimicrobial"
      selections$sirCol <- detectSIRColumn(upload$content) %||% "Value"
      removeModal()
    })
    
    
    observe({
      req(reactiveData())
    })
    
    observe({
      req(reactiveData())
      req(valueType())
      
      data <- reactiveData()

      safeExtract <- function(colName) {
        
        if(colName %in% names(data) && !is.null(colName) && colName != "Not Present") {
          return(data[[colName]])
        } else {
          return(rep(NA, nrow(data)))  # Return NA if the column doesn't exist
        }
      }

      formattedDataFrame <- data.frame(
        ID = safeExtract(selections$idCol),
        Year = safeExtract(selections$yearCol),
        Month = safeExtract(selections$monthCol),
        Region = safeExtract(selections$regionCol),
        Subregion = safeExtract(selections$subregionCol),
        Species = safeExtract(selections$speciesCol),
        Source = safeExtract(selections$sourceCol),
        Microorganism = safeExtract(selections$moCol),
        Antimicrobial = safeExtract(selections$drugCol)
      )
      
      # Add the additional columns if selected
      if (!is.null(selections$additionalCols) && length(selections$additionalCols) > 0) {
        for (col in selections$additionalCols) {
          formattedDataFrame[[col]] <- safeExtract(col)  # Add each additional column to the data frame
        }
      }
      
      # Add specific columns based on the valueType
      if (valueType() == "SIR") {
        formattedDataFrame$Interpretation <- safeExtract(selections$sirCol)
      } else {
        formattedDataFrame$Sign <- safeExtract(selections$micSignCol)
        formattedDataFrame$Value <- safeExtract(selections$micValCol)
      }
      
      # Update the formattedData with the final data frame
      formattedData(formattedDataFrame)
    })
    
    
    availableData <- reactive({
      data <- formattedData()
      if (is.null(data)) {
        return(NULL)
      }
      
      required_cols <- c("Year", "Region", "Species", "Source", "Microorganism", "Antimicrobial")
      if (selections$valueType == "SIR") {
        required_cols <- c(required_cols, "Interpretation")
      } else {
        required_cols <- c(required_cols, "Sign", "Value")
      }
      
      missing_cols <- setdiff(required_cols, names(data))
      
      if (selections$valueType == "SIR") {
        filteredData <- data %>%
          filter(
            !is.na(Year),
            !is.na(Region),
            !is.na(Species),
            !is.na(Source),
            !is.na(Microorganism),
            !is.na(Antimicrobial),
            !is.na(Interpretation)
          )
      } else {
        filteredData <- data %>%
          filter(
            !is.na(Year),
            !is.na(Region),
            !is.na(Species),
            !is.na(Source),
            !is.na(Microorganism),
            !is.na(Antimicrobial),
            !is.na(Sign),
            !is.na(Value)
          )
      }
      
      return(filteredData)
    })
    
    output$availableRows <- renderUI({
      req(availableData())
      h5(HTML(paste("<font color = #44CDC4>", format(nrow(availableData()), big.mark = ","), "</font> rows are suitable for processing")), align = "left")
    })
    
    output$availableDataPreview <- renderTable({
      req(availableData())
      head(availableData(), 100)
    })
    
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
