mdrPageUI <- function(id, data) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      
      # Main Content ------------------------------------------------------------
      
      column(9,
             uiOutput(ns("content"))
      ),
      
      # Filters -----------------------------------------------------------------
      
      column(3,
             
             wellPanel(
               h4("Filters", style = "text-align: center;"),
               selectizeInput(ns("moFilter"), "Microorganism", 
                              choices = c(sort(unique(data$Microorganism), na.last = TRUE)),
                              multiple = TRUE,
                              selected = names(which.max(table(data$Microorganism)))),
               selectizeInput(ns("speciesFilter"), "Species", 
                              choices = c(sort(unique(data$Species), na.last = TRUE)),
                              multiple = TRUE),
               selectizeInput(ns("sourceFilter"), "Source", 
                              choices = c(sort(unique(data$Source), na.last = TRUE)),
                              multiple = TRUE),
               dateRangeInput(ns("timeFilter"), "Timeframe", 
                              min = min(data$Date), max = max(data$Date), 
                              start = min(data$Date), end = max(data$Date)),
               div(
                 actionButton(ns("last3Months"), "3 mo", class = "quickDateButton"),
                 actionButton(ns("last6Months"), "6 mo", class = "quickDateButton"),
                 actionButton(ns("pastYear"), "1 yr", class = "quickDateButton"),
                 actionButton(ns("yearToDate"), "YTD", class = "quickDateButton"),
                 actionButton(ns("allData"), "All", class = "quickDateButton"),
                 class = "quickDateButtonDiv"
               ),
               actionButton(ns("applyFilter"), "Apply", class = "submitButton"),
               class = "contentWell",
               height = "35vh"
             ),
             
             
             # Legend ------------------------------------------------------------------
             
             wellPanel(
               h4("Legend", class = "legend-title"),
               h5("Correlation", class = "legend-section"),
               div(class = "mdrLegend"),
               div(class = "mdrLegendLabels",
                   span("-1"),
                   span("0"),
                   span("1")
               ),
               class = "legendWell"
             )
             
      )
    )
  )
}

mdrPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    most_common_mo <- names(which.max(table(data$Microorganism)))
    
    observe({
      updateSelectizeInput(session, ns("moFilter"), selected = most_common_mo)
    })
    
    observeEvent(input$last3Months, {
      updateDateRangeInput(session, "timeFilter", 
                           min = min(data$Date), max = max(data$Date), 
                           start = max(data$Date) %m-% months(3), end = max(data$Date))
    })
    
    observeEvent(input$last6Months, {
      updateDateRangeInput(session, "timeFilter", 
                           min = min(data$Date), max = max(data$Date), 
                           start = max(data$Date) %m-% months(6), end = max(data$Date))
    })
    
    observeEvent(input$pastYear, {
      updateDateRangeInput(session, "timeFilter", 
                           min = min(data$Date), max = max(data$Date), 
                           start = max(data$Date) %m-% years(1), end = max(data$Date))
    })
    
    observeEvent(input$yearToDate, {
      updateDateRangeInput(session, "timeFilter", 
                           min = min(data$Date), max = max(data$Date), 
                           start = make_date(year(max(data$Date)), 1, 1), end = max(data$Date))
    })
    
    observeEvent(input$allData, {
      updateDateRangeInput(session, "timeFilter",
                           min = min(data$Date), max = max(data$Date), 
                           start = min(data$Date), end = max(data$Date))
    })
    
    initialData <- reactive({
      data %>%
        filter(Microorganism %in% most_common_mo)
    })
    
    filteredData <- eventReactive(input$applyFilter, {
      tempData <- data
      if (length(input$moFilter) > 0) {
        tempData <- tempData[tempData$Microorganism %in% input$moFilter, ]
      }
      if (length(input$speciesFilter) > 0) {
        tempData <- tempData[tempData$Species %in% input$speciesFilter, ]
      }
      if (length(input$sourceFilter) > 0) {
        tempData <- tempData[tempData$Source %in% input$sourceFilter, ]
      }
      if (!is.null(input$timeFilter)) {
        tempData <- tempData[tempData$Date >= input$timeFilter[1] & tempData$Date <= input$timeFilter[2], ]
      }
      tempData
    })
    
    plotData <- reactive({
      if (input$applyFilter > 0) {
        filteredData()
      } else {
        initialData()
      }
    })
    
    output$content <- renderUI({
      req(plotData())
      if (!is.null(plotData()) && nrow(plotData()) > 0) {
        wellPanel(style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh;",
                  div(style = "min-height: 750px",
                      plotlyOutput(ns("mdr"), height = "75vh")
                  ),
                  class = "contentWell"
        )
      } else {
        wellPanel(
          style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
          div(
            style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
            uiOutput(ns("errorHandling"))
          ),
          class = "contentWell"
        )
      }
    })
    
    output$errorHandling <- renderUI({
      div(style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
          icon("disease", style = "font-size:100px; color: #44CDC4"),
          h4("Oops... looks like there isn't enough data for this plot."),
          h6("Try reducing the number of filters applied or adjust your data in the 'Import' tab.")
      )
    })
    
    output$mdr <- renderPlotly({
      mdrData <- plotData() %>%
        group_by(ID, Microorganism) %>%
        summarise(n_classes = n_distinct(Class[Interpretation %in% c("R", "I")])) %>%
        mutate(mdro = ifelse(n_classes > 2, 1, 0))
      
      matrixData <- plotData() %>%
        left_join(mdrData, by = c("ID", "Microorganism")) %>%
        mutate(Interpretation = ifelse(Interpretation == "S", 1, 0),
               classDrug = paste(Class, as.ab(Antimicrobial), sep = "-"),
               ID = paste(ID, Date, Source, Microorganism)) %>%
        arrange(ID, Date) %>%
        distinct() %>%
        select(ID, "Drug" = classDrug, Interpretation) %>%
        group_by(ID, Drug) %>%
        summarize(Interpretation = max(Interpretation, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(id_cols = ID, names_from = Drug, values_from = Interpretation) %>%
        mutate(across(everything(), ~ replace(., . == "NULL", NA))) %>% 
        select_if(~ sum(!is.na(.)) >= 30) %>% 
        select(-ID) %>% 
        select(sort(names(.))) 
      
      cor_and_counts <- function(data) {
        n <- ncol(data)
        cor_matrix <- matrix(NA, n, n)
        count_matrix <- matrix(0, n, n)
        for (i in 1:n) {
          for (j in i:n) {
            non_na_pair <- complete.cases(data[, c(i, j)])
            if (sum(non_na_pair) >= 30) {
              cor_matrix[i, j] <- cor(data[non_na_pair, i], data[non_na_pair, j], use = "complete.obs")
              cor_matrix[j, i] <- cor_matrix[i, j]
              count_matrix[i, j] <- sum(non_na_pair)
              count_matrix[j, i] <- count_matrix[i, j]
            }
          }
        }
        list(cor = cor_matrix, count = count_matrix)
      }
      
      result <- cor_and_counts(matrixData)
      phi <- result$cor
      counts <- result$count
      
      phi[counts < 30] <- NA
      
      keep <- !apply(is.na(phi), 1, all)
      phi <- phi[keep, keep]
      counts <- counts[keep, keep]
      labels <- colnames(matrixData)[keep]
      
      hovertext <- matrix("", nrow = length(labels), ncol = length(labels))
      for (i in 1:length(labels)) {
        for (j in 1:length(labels)) {
          if (!is.na(phi[i, j])) {
            hovertext[i, j] <- paste0(labels[i], "<br>",
                                      labels[j], "<br>",
                                      "Correlation: ", round(phi[i, j], 2), "<br>",
                                      "Observations: ", counts[i, j])
          }
        }
      }
      
      plotly_heatmap <- plot_ly(
        z = phi,
        x = labels,
        y = labels,
        type = "heatmap",  
        text = hovertext,
        hoverinfo = "text",
        colorscale = list(
          list(0, 'tomato'),
          list(0.25, 'pink'),
          list(0.5, '#F4F4F4'),
          list(0.75, '#44CDC4'),
          list(1, '#34435a')
        ),
        xgap = 1,
        ygap = 1,
        zmin = -1,
        zmax = 1,
        showscale = F,
        hoverlabel = list(bgcolor = "white")
      ) %>%
        layout(
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10)
        ) %>% 
        config(displayModeBar = FALSE)
      
      plotly_heatmap
    })
    
  })
}
