# library(leaflet)
# library(sf)
# library(tigris)
# library(stringdist)
# library(colorspace)
# library(HatchedPolygons)

mapPageUI <- function(id, data) {
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
                              choices = c(sort(unique(data$Antimicrobial), na.last = TRUE)),
                              multiple = TRUE,
                              selected = names(which.max(table(data$Antimicrobial)))),
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
               h5("Color", class = "legend-section"),
               div(
                 class = "legend-section",
                 div(
                   class = "legend-item",
                   tags$i(class = "fa-solid fa-square", class = "legendColorBlock", style = "color: #E0F7E1"),
                   span("90+% Susceptible", class = "legend-label")
                 ),
                 div(
                   class = "legend-item",
                   tags$i(class = "fa-solid fa-square", class = "legendColorBlock", style = "color: #71CDB8"),
                   span("80-89% Susceptible", class = "legend-label")
                 ),
                 div(
                   class = "legend-item",
                   tags$i(class = "fa-solid fa-square", class = "legendColorBlock", style = "color: #009FB3"),
                   span("70-79% Susceptible", class = "legend-label")
                 ),
                 div(
                   class = "legend-item",
                   tags$i(class = "fa-solid fa-square", class = "legendColorBlock", style = "color: #2E5F90"),
                   span("60-69% Susceptible", class = "legend-label")
                 ),
                 div(
                   class = "legend-item",
                   tags$i(class = "fa-solid fa-square", class = "legendColorBlock", style = "color: #3E294E"),
                   span("50-59% Susceptible", class = "legend-label")
                 ),
                 div(
                   class = "legend-item",
                   tags$i(class = "fa-solid fa-square", class = "legendColorBlock", style = "color: #070707"),
                   span("<50% Susceptible", class = "legend-label")
                 )
               ),
               h5("Hatching", class = "legend-section"),
               div(
                 class = "legend-section",
                 div(
                   class = "legend-item",
                   tags$i(class = "fa-solid fa-signal", class = "legendColorBlock", style = "font-size: 20px"),
                   span("Regions with less than 30 observations are denoted with hatch marks.", class = "legend-label")
                 )
               ),
               class = "legendWell"
             )
             
      )
    )
  )
}

mapPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    most_common_mo <- names(which.max(table(data$Microorganism)))
    most_common_ab <- names(which.max(table(data$Antimicrobial)))
    
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
        filter(Microorganism %in% most_common_mo, Antimicrobial %in% most_common_ab)
    })
    
    filteredData <- eventReactive(input$applyFilter, {
      tempData <- data
      if (length(input$moFilter) > 0) {
        tempData <- tempData[tempData$Microorganism %in% input$moFilter, ]
      }
      if (length(input$abFilter) > 0) {
        tempData <- tempData[tempData$Antimicrobial %in% input$abFilter, ]
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
                      leafletOutput(ns("map"), height = "71vh")
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
    
    baseMap <- preprocessMapData(data)
    
    output$map <- renderLeaflet({
      req(baseMap)
      req(plotData())

      mapData <- preprocessPlotData(plotData())
      mapData <- matchSubregions(baseMap, mapData) 

      map <- baseMap %>%
        left_join(mapData, by = c("Region", "Subregion")) %>%
        mutate(Subregion = str_to_sentence(Subregion))

      pal <- sequential_hcl(n = 6, palette = "Mako")
      breakpoints <- c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
      color_pal <- colorBin(pal, domain = NULL, bins = breakpoints, na.color = "#999999")

      map1 <- map %>%
        filter(Count >= 30)

      map2 <- map %>%
        filter(Count < 30) %>%
        mutate(ID = as.character(row_number()))

      map2.hatch <- hatched.SpatialPolygons(map2, density = 35, angle = c(0,45,90,135))

      map2 <- map2 %>%
        st_drop_geometry()

      map2.hatch <- map2.hatch %>%
        left_join(map2, by = "ID")
        
      popups <- paste0(
        "<div style='font-family: Carme, sans-serif; line-height: 1.4;'>",
        "<h4 style='color: #44CDC4;'><b>", map$Subregion, " County, ", "<span style='color: #34435a;'>", map$Region, "</span></b></h4>",
        "<hr style='border-top: 1px solid #cccccc;'>",
        "<i class='fa fa-bacterium' style='color: #44CDC4; font-size: 20px;'></i> <span style='font-family: Carme;'>: ", 
        ifelse(length(input$moFilter) > 1, "Multiple selected", paste(input$moFilter, collapse=", ")), "</span>",
        "<br>", 
        "<i class='fa fa-pills' style='color: #34435a; font-size: 20px;'></i> <span style='font-family: Carme;'>: ", 
        ifelse(length(input$abFilter) > 1, "Multiple selected", paste(input$abFilter, collapse=", ")), "</span>",
        "<br>",
        sapply(1:length(map$propS), function(i) {
          if (is.na(map$propS[i]) || map$Count[i] < 30) {
            ""
          } else if (map$propS[i] >= 0.9) {
            "<i class='fa fa-check-circle' style='color: green; font-size: 20px;'></i> <span style='font-family: Carme;'>High observed susceptibility in isolates tested.</span>"
          } else if (map$propS[i] >= 0.7) {
            "<i class='fa fa-exclamation-circle' style='color: gold; font-size: 20px;'></i> <span style='font-family: Carme;'>Moderate observed susceptibility in isolates tested.</span>"
          } else {
            "<i class='fa fa-times-circle' style='color: red; font-size: 20px;'></i> <span style='font-family: Carme;'>Low observed susceptibility in isolates tested.</span>"
          }
        }),
        "<hr style='border-top: 1px solid #cccccc;'>",
        "<h5><b>Number of isolates tested: </b>", format(round(as.integer(map$Count), 0), nsmall=1, big.mark=","), "</h5>",
        "<h5><b>Percentage of isolates susceptible: </b>", format(round(as.numeric(map$propS * 100), 0)), "%</h5>",
        "<h5><b>Percentage of isolates intermediate: </b>", format(round(as.numeric(map$propI * 100), 0)), "%</h5>",
        "<h5><b>Percentage of isolates resistant: </b>", format(round(as.numeric(map$propR * 100), 0)), "%</h5>",
        "</div>"
      )
      
      

      leaflet(map) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(
          data = map1,
          fillColor = ~color_pal(propS),
          weight = 0,
          fillOpacity = 0.8,
        ) %>% 
        addPolylines(
          data = map2.hatch,
          color = ~color_pal(propS),
          weight = 2,
        ) %>%
        addPolygons(
          fillColor = "transparent",
          color = "#777777",
          weight = 1.5,
          popup = popups
        )
    })
    
  })
}