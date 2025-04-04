# library(tidyverse)
# library(lubridate)
# library(plotly)
# library(zoo)


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
             filterPanelUI(ns("filters")),
             
             # Controls ------------------------------------------------------------------
             
             bsCollapse(
               id = "collapsePanel",
               open = NULL,
               multiple = T,
               bsCollapsePanel(
                 HTML("Controls <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
            style='float: right; color: #aaa;'></span>"),
                 radioGroupButtons(ns("tsType"), "Smoothing",
                                   choices = c("None", "Rolling Mean", "LOWESS"),
                                   direction = "vertical",
                                   selected = "None",
                                   justified = TRUE
                 ),
                 uiOutput(ns("additionalControls"))
               )
             )
      )
    ),
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('savePlot', function(data) {
        var plotElement = document.getElementById(data.plotId); // Target the plot by id
        if (plotElement) {
          // Download image with specified width, height, and scale
          Plotly.downloadImage(plotElement, {
            format: 'png', // File format
            filename: data.filename, // Filename for download
            width: data.width,  // Custom width
            height: data.height, // Custom height
            scale: data.scale // Resolution scaling factor (default is 1, larger values improve quality)
          });
        } else {
          console.error('Plot element not found for id: ' + data.plotId);
        }
      });
      "
    ))
  )
}

tsPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    filters <- filterPanelServer(
      "filters", 
      data, 
      default_filters = c("Antimicrobial", "Microorganism", "Species", "Source", "Date"), 
      auto_populate = list(Antimicrobial = TRUE, Microorganism = TRUE)
    )
    
    plotData <- reactive({ filters$filteredData() })
    
    initialData <- reactive({
      data
    })
    
    output$content <- renderUI({
      req(plotData())
      if (!is.null(plotData()) && nrow(plotData()) > 0) {
        tagList(
        wellPanel(style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh;",
                  div(style = "min-height: 750px",
                      plotlyOutput(ns("plot"), height = "71vh")
                  ),
                  class = "contentWell"
        ),
        actionButton(ns("save_btn"), "Save", class = "plotSaveButton")
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
    
    output$plot <- renderPlotly({
      tsData <- plotData() %>%
        select(Date, Antimicrobial, Interpretation) %>%
        mutate(Interpretation = ifelse(Interpretation == "S", 1, 0)) %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Date, Antimicrobial) %>%
        summarize(Susceptible = sum(Interpretation),
                  Count = n(),
                  .groups = "drop") %>%
        arrange(Antimicrobial, Date)
      
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
              new_df <- rbind(new_df, df[i+1,])
              new_df$Count[nrow(new_df)] <- 0
              new_df$Susceptible[nrow(new_df)] <- 0
            }
          }
        }
        return(new_df)
      }
      
      tsData <- tsData %>%
        group_by(Antimicrobial) %>%
        group_modify(~ roll_forward(.)) %>% 
        mutate(propS = (Susceptible / Count) * 100) %>%  # Convert to percentage
        filter(Count >= 30)
      
      if(input$tsType == "Rolling Mean"){
        
        tsDataRM <- tsData %>%
          group_by(Antimicrobial) %>%
          arrange(Date) %>%
          mutate(ma_propS = rollmean(propS, k = input$rmWindow, fill = NA, align = "right"))
        
        numColors <- length(unique(tsDataRM$Antimicrobial))
        
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
                color = ~Antimicrobial, colors = colorPalette,
                text = ~paste("Antimicrobial:", Antimicrobial, "<br>Isolates tested:", Count, "<br>% Susceptible:", round(ma_propS, 3), "<br>Date:", Date),
                hoverinfo = "text") %>%
          layout(title = "",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "% Susceptible", range = c(0, 100))) %>%  # Adjust range to 0-100%
          config(displayModeBar = FALSE)
        
      } else if (input$tsType == "LOWESS"){
        apply_lowess <- function(df, x, y, f = input$lowessSpan) {
          lowess_result <- stats::lowess(df[[x]], df[[y]], f = f)
          df$low_propS <- lowess_result$y
          return(df)
        }
        
        tsData_clean <- tsData %>%
          drop_na(Date, propS)
        
        tsDataLowess <- tsData_clean %>%
          group_by(Antimicrobial) %>%
          nest() %>%
          mutate(data = map(data, ~ apply_lowess(.x, "Date", "propS"))) %>%
          unnest(cols = c(data)) %>%
          ungroup()
        
        numColors <- length(unique(tsDataLowess$Antimicrobial))
        
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
                color = ~Antimicrobial, colors = colorPalette,
                text = ~paste("Antimicrobial:", Antimicrobial, "<br>Isolates tested:", Count, "<br>% Susceptible:", round(low_propS, 3), "<br>Date:", Date),
                hoverinfo = "text") %>%
          layout(title = "",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "% Susceptible", range = c(0, 100))) %>%  # Adjust range to 0-100%
          config(
            displayModeBar = TRUE, 
            modeBarButtonsToRemove = c(
              "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
              "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian"
            ),
            modeBarButtonsToAdd = c(
              'drawline', 'drawcircle', 'drawrect', 'eraseshape'
            ),
            toImageButtonOptions = list(format = "png",
                                        height = 850, width = 1250, scale = 3,
                                        filename = paste(Sys.Date(), "AMRVisualizerTrends", sep = "_"))
          )
        
      } else {
        
        numColors <- length(unique(tsData$Antimicrobial))
        
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
                color = ~Antimicrobial, 
                colors = colorPalette,
                text = ~paste("Antimicrobial:", Antimicrobial, "<br>Isolates tested:", Count, "<br>% Susceptible:", round(propS,3), "<br>Date:", Date),
                hoverinfo = "text") %>%
          layout(title = "",
                 legend = list(orientation = 'h'),
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "% Susceptible", range = c(0, 100))) %>%  # Adjust range to 0-100%
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = list(
                   'sendDataToCloud',
                   'autoScale2d',
                   'resetScale2d',
                   'hoverClosestCartesian',
                   'hoverCompareCartesian',
                   'zoom2d', 
                   'pan2d',
                   'select2d',
                   'lasso2d',
                   'zoomIn2d', 
                   'zoomOut2d',
                   'toggleSpikelines'
                 )
          )
      }
    })
    
    observeEvent(input$save_btn, {
      session$sendCustomMessage("savePlot", list(
        plotId = ns("plot"),
        filename = paste0(Sys.Date(), "AMRVisualizerTimeSeries"),
        width = 1200,
        height = 800,
        scale = 3
      ))
    })
    
  })
}
