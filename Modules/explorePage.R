# library(shinyBS)
# library(jsonlite)

explorePageUI <- function(id, data) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content"))
  )
}

explorePageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dataGenerated <- reactiveVal(FALSE)
    inputs <- reactiveValues(MO = NULL, AB = NULL, Site = NULL, Species = NULL, Subregion = NULL, startDate = min(data$Date, na.rm = T), endDate = max(data$Date, na.rm = T),
                             groupMO = F, groupAB = F, groupSite = F, groupSpecies = F, groupSubregion = F, groupMonths = F)
    
    observeEvent(input$generate, {
      inputs$MO <- input$selectedMO
      inputs$AB <- input$selectedAB
      inputs$Site <- input$selectedSite
      inputs$Species <- input$selectedSpecies
      inputs$Subregion <- input$selectedSubregion
      inputs$startDate <- input$selectedDates[1]
      inputs$endDate <- input$selectedDates[2]
      inputs$groupMO <- input$groupMO
      inputs$groupAB <- input$groupAB
      inputs$groupSite <- input$groupSite
      inputs$groupSpecies <- input$groupSpecies
      inputs$groupSubregion <- input$groupSubregion
      inputs$groupMonths <- input$groupMonths
    })
    
    output$content <- renderUI({
      if (!dataGenerated()) {
        div(
          uiOutput(ns("filtersPanel")),
          style = "margin: auto;"
        )
      } else {
        tagList(
          fluidRow(
            column(4,
                   uiOutput(ns("filtersPanel"))
            ),
            column(8,
                   wellPanel(
                     dataTableOutput(ns("table")),
                     class = "dataPreviewWell"
                   ),
                   div(
                   downloadButton(ns("downloadParams"), "Download inputs", class = "clearButton"),
                   downloadButton(ns("downloadTable"), "Download table", class = "submitButton"),
                   style = "float: right; margin-top: 15px;"
                   )
            )
          )
        )
      }
    })
    
    output$filtersPanel <- renderUI({
      wellPanel(
        h4("Summary table filters"),
        hr(),
        div(
          bsCollapse(id = "exFilters", open = "Microorganism", multiple = T,
                     bsCollapsePanel("Microorganism",
                                     selectizeInput(ns("selectedMO"), label = NULL,
                                                    choices = c(sort(unique(data$Microorganism), na.last = TRUE)),
                                                    selected = inputs$MO,
                                                    multiple = T),
                                     awesomeCheckbox(ns("groupMO"), "Aggregate microorganisms?", value = inputs$groupMO)
                     ),
                     bsCollapsePanel("Antimicrobial",
                                     selectizeInput(ns("selectedAB"), label = NULL,
                                                    choices = c(sort(unique(data$Antimicrobial), na.last = TRUE)),
                                                    selected = inputs$AB,
                                                    multiple = T),
                                     awesomeCheckbox(ns("groupAB"), "Aggregate by class?", value = inputs$groupAB)
                     ),
                     bsCollapsePanel("Sampling site",
                                     selectizeInput(ns("selectedSite"), label = NULL,
                                                    choices = c(sort(unique(data$Source), na.last = TRUE)),
                                                    selected = inputs$Site,
                                                    multiple = T),
                                     awesomeCheckbox(ns("groupSite"), "Aggregate sites?", value = inputs$groupSite)
                     ),
                     bsCollapsePanel("Species",
                                     selectizeInput(ns("selectedSpecies"), label = NULL,
                                                    choices = c(sort(unique(data$Species), na.last = TRUE)),
                                                    selected = inputs$Species,
                                                    multiple = T),
                                     awesomeCheckbox(ns("groupSpecies"), "Aggregate species?", value = inputs$groupSpecies)
                     ),
                     bsCollapsePanel("Subregion",
                                     selectizeInput(ns("selectedSubregion"), label = NULL,
                                                    choices = c(sort(unique(data$Subregion), na.last = TRUE)),
                                                    selected = inputs$Subregion,
                                                    multiple = T),
                                     awesomeCheckbox(ns("groupSubregion"), "Aggregate subregions?", value = inputs$groupSubregion)
                     ),
                     bsCollapsePanel("Timeframe",
                                     dateRangeInput(ns("selectedDates"), label = NULL,
                                                    min = min(data$Date), max = max(data$Date), 
                                                    start = inputs$startDate, end = inputs$endDate,
                                                    width = "100%"),
                                     awesomeCheckbox(ns("groupMonths"), "Aggregate months?", value = inputs$groupMonths)
                     )
          ),
          style = "max-height: 50vh; overflow-y: auto;"
        ),
        hr(),
        h5("Upload parameter file"),
        fileInput(ns("paramsUpload"), label = NULL, accept = ".json"),
        actionButton(ns("generate"), "Generate table", class = "submitButton"),
        class = "exploreFiltersWell",
      )
    })
    
    observeEvent(input$paramsUpload, {
      req(input$paramsUpload)
      
      params <- fromJSON(readLines(input$paramsUpload$datapath))

        updateSelectizeInput(session, "selectedMO", selected = params$Inputs$Microorganism$Selected)
        updateSelectizeInput(session, "selectedAB", selected = params$Inputs$Antimicrobial$Selected)
        updateSelectizeInput(session, "selectedSite", selected = params$Inputs$Source$Selected)
        updateSelectizeInput(session, "selectedSpecies", selected = params$Inputs$Species$Selected)
        updateSelectizeInput(session, "selectedSubregion", selected = params$Inputs$Subregion$Selected)
        updateDateRangeInput(session, "selectedDates", start = params$Inputs$Dates$Selected[1], end = params$Inputs$Dates$Selected[2])
        updateCheckboxInput(session, "groupMO", value = params$Inputs$Microorganism$Aggregated)
        updateCheckboxInput(session, "groupAB", value = params$Inputs$Antimicrobial$Aggregated)
        updateCheckboxInput(session, "groupSite", value = params$Inputs$Source$Aggregated)
        updateCheckboxInput(session, "groupSpecies", value = params$Inputs$Species$Aggregated)
        updateCheckboxInput(session, "groupSubregion", value = params$Inputs$Subregion$Aggregated)
        updateCheckboxInput(session, "groupMonths", value = params$Inputs$Dates$Aggregated)
  
    })
    
    tableData <- reactiveVal(data.frame())
    
    observeEvent(input$generate, {
      dataGenerated(TRUE)
      
      tableData <- data
      
      if (!is.null(input$selectedMO)) {
        tableData <- tableData %>% filter(Microorganism %in% input$selectedMO)
      }
      if (!is.null(input$selectedAB)) {
        tableData <- tableData %>% filter(Antimicrobial %in% input$selectedAB)
      }
      if (!is.null(input$selectedSite)) {
        tableData <- tableData %>% filter(Source %in% input$selectedSite)
      }
      if (!is.null(input$selectedSpecies)) {
        tableData <- tableData %>% filter(Species %in% input$selectedSpecies)
      }
      if (!is.null(input$selectedSubregion)) {
        tableData <- tableData %>% filter(Subregion %in% input$selectedSubregion)
      }
      if (!is.null(input$selectedDates)) {
        tableData <- tableData %>% filter(Date >= input$selectedDates[1] & Date <= input$selectedDates[2])
      }
      
      tableData <- tableData %>% 
        select(
          if (!input$groupMonths) "Date",
          if (!input$groupMO) "Microorganism",
          if (!input$groupAB) "Antimicrobial",
          if (!input$groupSite) "Source",
          if (!input$groupSpecies) "Species",
          if (!input$groupSubregion) c("Subregion", "Region"),
          "Class", "Interpretation"
        ) %>% 
        group_by(across()) %>%
        summarize(int_count = n()) %>%
        pivot_wider(names_from = Interpretation, values_from = int_count, values_fill = 0) %>% 
        mutate(Total = sum(S,R,I)) %>% 
        relocate(S, I, R, .before = Total)
      
      output$table <- renderDataTable({
        datatable(tableData, class = "table",
                  options = list(
                    columnDefs = list(list(className = 'dt-center', targets = 5)),
                    pageLength = 20,
                    lengthMenu = c(10, 20, 50, 100)
                  ))
      })
      
      tableData(tableData)
    })
    
    output$downloadParams <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-AMRDataVisualizer.json")
      },
      content = function(file) {
        paramsJSON <- list(
          AppInfo = list(
            Title = "AMR Data Visualizer - Summary Table",
            SystemDate = as.character(Sys.Date())
          ),
          Inputs = list(
            Microorganism = list(
              Selected = input$selectedMO,
              Aggregated = input$groupMO
            ),
            Antimicrobial = list(
              Selected = input$selectedAB,
              Aggregated = input$groupAB
            ),
            Source = list(
              Selected = input$selectedSite,
              Aggregated = input$groupSite
            ),
            Species = list(
              Selected = input$selectedSpecies,
              Aggregated = input$groupSpecies
            ),
            Subregion = list(
              Selected = input$selectedSubregion,
              Aggregated = input$groupSubregion
            ),
            Dates = list(
              Selected = input$selectedDates,
              Aggregated = input$groupMonths
            )
          )
        )

        jsonData <- toJSON(paramsJSON, pretty = TRUE)
        
        writeLines(jsonData, file)
      }
    )
    
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-AMRDataVisualizerSummaryTable.csv")
      },
      content = function(file) {
        write.csv(tableData(), file)
      }
    ) 

  })
}
