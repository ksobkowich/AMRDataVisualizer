library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)


# To-do -------------------------------------------------------------------
# - Confidence bands?

tsPageUI <- function(id, data) {
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
               selectizeInput(ns("abFilter"), "Antimicrobial", 
                              choices = c(sort(unique(data$Class), na.last = TRUE)),
                              multiple = TRUE,
                              selected = names(which.max(table(data$Class)))),
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
               h4("Plot Controls", class = "legend-title"),
               radioGroupButtons(ns("tsType"), "Smoothing",
                 choices = c("None", "Rolling Mean", "LOWESS"),
                 direction = "vertical",
                 selected = "None",
                 justified = TRUE
               ),
               uiOutput(ns("additionalControls")),
               class = "legendWell"
             )
             
      )
    )
  )
}

tsPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    most_common_mo <- names(which.max(table(data$Microorganism)))
    most_common_ab <- names(which.max(table(data$Class)))
    
    observe({
      updateSelectizeInput(session, ns("moFilter"), selected = most_common_mo)
      updateSelectizeInput(session, ns("abFilter"), selected = most_common_ab)
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
        filter(Microorganism %in% most_common_mo, Class %in% most_common_ab)
    })
    
    filteredData <- eventReactive(input$applyFilter, {
      tempData <- data
      if (length(input$moFilter) > 0) {
        tempData <- tempData[tempData$Microorganism %in% input$moFilter, ]
      }
      if (length(input$abFilter) > 0) {
        tempData <- tempData[tempData$Class %in% input$abFilter, ]
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
                      plotlyOutput(ns("ts"), height = "71vh")
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
    
    output$additionalControls <- renderUI({
      if(input$tsType == "Rolling Mean"){
        sliderInput(ns("rmWindow"), "Window", min = 2, max = 12, value = 6, step = 1)
      } else if (input$tsType == "LOWESS"){
        sliderInput(ns("lowessSpan"), "Span", min = 0.1, max = 1, value = 0.3, step = .1)
      } else {
        NULL
      }
    })
    
    output$ts <- renderPlotly({
    tsData <- plotData() %>%
      select(Date, Class, Interpretation) %>%
      mutate(Interpretation = ifelse(Interpretation == "S", 1, 0)) %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(Date, Class) %>%
      summarize(Susceptible = sum(Interpretation),
                Count = n(),
                .groups = "drop") %>%
      arrange(Class, Date)
    
    
    tsData$Date <- as.Date(tsData$Date)
    
    roll_forward <- function(df) {
      new_df <- df[1,]
      new_df$Count <- 0
      new_df$Susceptible <- 0
      
      for (i in 1:nrow(df)) {
        new_df$Susceptible[nrow(new_df)] <- new_df$Susceptible[nrow(new_df)] + df$Susceptible[i]
        new_df$Count[nrow(new_df)] <- new_df$Count[nrow(new_df)] + df$Count[i]
        new_df$Date[nrow(new_df)] <- df$Date[i]
        
        if (new_df$Count[nrow(new_df)] >= 30) {
          if (i < nrow(df)) {
            # Start a new row for the next accumulation
            new_df <- rbind(new_df, df[i+1,])
            new_df$Count[nrow(new_df)] <- 0
            new_df$Susceptible[nrow(new_df)] <- 0
          }
        }
      }
      return(new_df)
    }
    
    tsData <- tsData %>%
      group_by(Class) %>%
      group_modify(~ roll_forward(.)) %>% 
      mutate(propS = Susceptible / Count) %>% 
      filter(Count >= 30)
    

# Rolling Mean Plot -------------------------------------------------------

    if(input$tsType == "Rolling Mean"){
      
      tsDataRM <- tsData %>%
        group_by(Class) %>%
        arrange(Date) %>%
        mutate(ma_propS = rollmean(propS, k = input$rmWindow, fill = NA, align = "right"))
      
      numColors <- length(unique(tsDataRM$Class))
      
      gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
      }
      
      colorPalette = gg_color_hue(numColors)
      
      plot_ly(tsDataRM, 
              x = ~Date, 
              y = ~ma_propS, 
              type = 'scatter', 
              mode = 'lines+markers',
              color = ~Class, colors = colorPalette,
              text = ~paste("Class:", Class, "<br>Isolates tested:", Count, "<br>% Susceptible:", round(ma_propS, 3)*100, "<br>Date:", Date),
              hoverinfo = "text") %>%
        layout(title = "",
               xaxis = list(title = "Date"),
               yaxis = list(title = "% Susceptible", range = c(0, 1))) %>% 
        config(displayModeBar = FALSE)
      

# LOWESS Plot -------------------------------------------------------------

    } else if (input$tsType == "LOWESS"){
      apply_lowess <- function(df, x, y, f = input$lowessSpan) {
        lowess_result <- stats::lowess(df[[x]], df[[y]], f = f)
        df$low_propS <- lowess_result$y
        return(df)
      }
      
      tsData_clean <- tsData %>%
        drop_na(Date, propS)
      
      tsDataLowess <- tsData_clean %>%
        group_by(Class) %>%
        nest() %>%
        mutate(data = map(data, ~ apply_lowess(.x, "Date", "propS"))) %>%
        unnest(cols = c(data)) %>%
        ungroup()
      
      numColors <- length(unique(tsDataLowess$Class))
      
      gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
      }
      
      colorPalette = gg_color_hue(numColors)
      
      plot_ly(tsDataLowess,
              x = ~Date, 
              y = ~low_propS, 
              type = 'scatter', 
              mode = 'lines+markers',
              color = ~Class, colors = colorPalette,
              text = ~paste("Class:", Class, "<br>Isolates tested:", Count, "<br>% Susceptible:", round(low_propS, 3)*100, "<br>Date:", Date),
              hoverinfo = "text") %>%
        layout(title = "",
               xaxis = list(title = "Date"),
               yaxis = list(title = "% Susceptible", range = c(0, 1))) %>% 
        config(displayModeBar = FALSE)
      

# Point Plot --------------------------------------------------------------

    } else {
    
    numColors <- length(unique(tsData$Class))
    
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    colorPalette = gg_color_hue(numColors)
    
    
  plot_ly(tsData, 
                 x = ~Date, 
                 y = ~propS, 
                 type = 'scatter', 
                 mode = 'lines+markers', 
                 color = ~Class, 
                 colors = colorPalette,
                 text = ~paste("Class:", Class, "<br>Isolates tested:", Count, "<br>% Susceptible:", round(propS,3)*100, "<br>Date:", Date),
                 hoverinfo = "text") %>%
      layout(title = "",
             xaxis = list(title = "Date"),
             yaxis = list(title = "% Susceptible", range = c(0, 1))) %>% 
    config(displayModeBar = FALSE)
    }
    })
    
  })
}
