abPageUI <- function(id, data) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(9,
             wellPanel(style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh;",
                   div(style = "min-width: 1150px; min-height: 750px",
                       plotlyOutput(ns("antibiogramPlot"), height = "700px", width = "100%")
                       ),
               class = "contentWell"
             )
      ),
      column(3,
             wellPanel(
               h4("Filters", style = "text-align: center;"),
               hr(),
               selectizeInput(ns("speciesFilter"), "Species", 
                              choices = c(sort(unique(data$Species), na.last = TRUE)),
                              multiple = TRUE),
               selectizeInput(ns("sourceFilter"), "Source", 
                              choices = c(sort(unique(data$Source), na.last = TRUE)),
                              multiple = TRUE),
               dateRangeInput(ns("timeFilter"), "Timeframe", 
                              min = min(data$Date), max = max(data$Date), 
                              start = min(data$Date), end = max(data$Date)),
               actionButton(ns("applyFilter"), "Apply", class = "submitButton"),
               class = "contentWell",
               height = "35vh"
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
    
    filteredData <- eventReactive(input$applyFilter, {
      tempData <- data
      if (length(input$speciesFilter) > 0) {
        tempData <- tempData[tempData$Species %in% input$speciesFilter, ]
      }
      if (length(input$sourceFilter) > 0) {
        tempData <- tempData[tempData$Source %in% input$sourceFilter, ]
      }
      if (length(input$resistanceFilter) > 0) {
        tempData <- tempData[tempData$Antimicrobial %in% input$resistanceFilter, ]
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
    
    output$antibiogramPlot <- renderPlotly({
      
      result_table <- plotData() %>%
        filter(Interpretation %in% c("S", "R", "I")) %>%
        mutate(Interpretation = ifelse(Interpretation == "S", 1, 0)) %>%
        add_count(Microorganism, name = "Frequency") %>%
        filter(Frequency >= min(tail(sort(unique(Frequency)), 20))) %>%
        group_by(Microorganism, Antimicrobial, Class) %>%
        summarise(
          obs = n(),
          prop = round(mean(Interpretation == 1), 3),
          .groups = 'drop'
        ) %>%
        mutate(size = cut(prop, breaks = c(0, 0.5, 0.7, 0.9, 1), labels = c("xs", "s", "m", "l"))) %>%
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
        scale_size_manual(values = c("xs" = 1.5, "s" = 3, "m" = 4.5, "l" = 6)) +
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
    
  })
}