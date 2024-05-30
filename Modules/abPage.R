abPageUI <- function(id, data) {
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
               h5("Size", class = "legend-section"),
               div(
                 class = "legend-section",
                 div(
                   class = "legend-item",
                   tags$i(class = "fas fa-circle legend-circle", style = "font-size: 10px; margin-left: 10px;"),
                   span("Low susceptibility (<70%)", class = "legend-label", style = "margin-left: 20px;")
                 ),
                 div(
                   class = "legend-item",
                   tags$i(class = "fas fa-circle legend-circle", style = "font-size: 20px; margin-left: 5px;"),
                   span("Moderate susceptibility (70 - 90%)", class = "legend-label", style = "margin-left: 15px;")
                 ),
                 div(
                   class = "legend-item",
                   tags$i(class = "fas fa-circle legend-circle", style = "font-size: 30px; margin-right: 10px;"),
                   span("High susceptibility (>90%)", class = "legend-label", style = "margin-left: 0px;")
                 )
               ),
               h5("Opacity", class = "legend-section"),
               div(
                 class = "opacity-container",
                 div(
                   class = "opacity-item",
                   tags$i(class = "fas fa-circle legend-circle", style = "opacity: 0.1;"),
                   span("<30 Samples", class = "legend-label")
                 ),
                 div(class = "vertical-divider"),
                 div(
                   class = "opacity-item",
                   tags$i(class = "fas fa-circle legend-circle"),
                   span("30+ Samples", class = "legend-label")
                 )
               ),
               h6("Bubbles are colored by antimicrobial class."),
               h6("Hover over a bubble for more details"),
               class = "legendWell"
             )
             
      )
    )
  )
}


abPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    initialData <- reactive({
      data
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
    
    filteredData <- eventReactive(input$applyFilter, {
      tempData <- data
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
    
    #Load plot initially with all data then only reload when "Apply" is clicked.
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
                  div(style = "min-width: 1150px; min-height: 750px",
                      plotlyOutput(ns("antibiogramPlot"), height = "700px")
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
    
    output$antibiogramPlot <- renderPlotly({
      
      result_table <- plotData() %>%
        filter(Interpretation %in% c("S", "R", "I")) %>%
        mutate(Interpretation = ifelse(Interpretation == "S", 1, 0)) %>%
        add_count(Microorganism, name = "Frequency") %>%
        filter(Frequency > min(tail(sort(unique(Frequency)), 15))) %>%
        group_by(Microorganism, Antimicrobial, Class) %>%
        summarise(
          obs = n(),
          prop = round(mean(Interpretation == 1), 3),
          .groups = 'drop'
        ) %>%
        mutate(size = cut(prop, breaks = c(0,0.7, 0.9, 1), labels = c("s", "m", "l"))) %>%
        arrange(Class, Antimicrobial)
      
      unique_drugs <- distinct(result_table, Antimicrobial, Class, .keep_all = TRUE) %>%
        arrange(Class, Antimicrobial)
      
      
      g <- ggplot(result_table, aes(x = interaction(Antimicrobial, Class),
                                    y = Microorganism,
                                    size = size,
                                    colour = Class,
                                    fill = Class,
                                    text = paste("Microorganism:", Microorganism,
                                                 "<br>Antimicrobial:", Antimicrobial,
                                                 "<br>Class:", Class,
                                                 "<br>% Susceptible:", round(prop * 100, 2),
                                                 "<br>Isolates tested:", obs)
      )
      ) +
        geom_point(shape = 21, stroke = 0.5, aes(alpha = ifelse(obs > 30, 1, obs / 30))) +
        scale_x_discrete(label = str_to_sentence(str_sub(unique_drugs$Antimicrobial, 1, 15))) +
        scale_size_manual(values = c("s" = 2, "m" = 5, "l" = 7)) +
        labs(
          title = "",
          x = "",
          y = ""
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill = 'transparent', color = NA),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          axis.text.y = element_text(colour = "grey20"),
          axis.text.x = element_text(angle = 90, hjust = 0, color = "grey20")
        ) +
        guides(fill = "none")
      
      ggplotly(g, tooltip = c("text")) %>% 
        config(displayModeBar = FALSE)
      
    })
    
    output$errorHandling <- renderUI({
      div(style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
          icon("disease", style = "font-size:100px; color: #44CDC4"),
          h4("Oops... looks like there isn't enough data for this plot."),
          h6("Try reducing the number of filters applied or adjust your data in the 'Import' tab.")
      )
    })
    
  })
}