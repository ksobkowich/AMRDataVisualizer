pathogenPageUI <- function(id, data) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             wellPanel(
               actionButton(ns("filters"), label = NULL, icon = icon("filter"), style = "float: right", class = "clearButton"),
               h4("Common microorganisms found in your data."),
               em(h6("Double-click on a bar to view information about a specific microorganism")),
               uiOutput(ns("filterTags")),
               hr(),
               uiOutput(ns("moContent")),
               class = "contentWell",
               style = "min-height: 60vh"
             )
      ),
      column(6,
             wellPanel(
               selectizeInput(ns("moFilter"), label = NULL, 
                              choices = c(sort(unique(data$Microorganism), na.last = TRUE)),
                              multiple = F,
                              selected = names(which.max(table(data$Microorganism)))),
               hr(),
               div(
                 uiOutput(ns("pathogenInfo")),
                 class = "pathogenInfoDiv"
               ),
               class = "contentWell",
               height = "60vh"
             )
      )
    )
    
  )
  
}

pathogenPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    moInformation <- read.csv("./Data/microorganismInformation.csv")
    
    filters <- reactiveValues(
      species = NULL,
      site = NULL,
      region = NULL,
      subregion = NULL
    )
    
    filteredData <- reactive({
      plotData <- data
      
      if (!is.null(filters$species) && length(filters$species) > 0) {
        plotData <- plotData %>% filter(Species %in% filters$species)
      }
      if (!is.null(filters$site) && length(filters$site) > 0) {
        plotData <- plotData %>% filter(Source %in% filters$site)
      }
      if (!is.null(filters$region) && length(filters$region) > 0) {
        plotData <- plotData %>% filter(Region %in% filters$region)
      }
      if (!is.null(filters$subregion) && length(filters$subregion) > 0) {
        plotData <- plotData %>% filter(Subregion %in% filters$subregion)
      }
      
      plotData
    })
    
    activeFilters <- reactive({
      tags <- tagList()
      
      if (!is.null(filters$species) && length(filters$species) > 0) {
        tags <- c(tags, lapply(filters$species, function(x) {
          div(class = "activeFilters", x)
        }))
      }
      if (!is.null(filters$site) && length(filters$site) > 0) {
        tags <- c(tags, lapply(filters$site, function(x) {
          div(class = "activeFilters", x)
        }))
      }
      if (!is.null(filters$region) && length(filters$region) > 0) {
        tags <- c(tags, lapply(filters$region, function(x) {
          div(class = "activeFilters", x)
        }))
      }
      if (!is.null(filters$subregion) && length(filters$subregion) > 0) {
        tags <- c(tags, lapply(filters$subregion, function(x) {
          div(class = "activeFilters", x)
        }))
      }
      
      tags
    })
    
    output$filterTags <- renderUI({
      activeFilterTags <- activeFilters()
      
      if (length(activeFilterTags) > 0) {
        tags$div(
          h4("Active Filters"),
          activeFilterTags,
        )
      } else {
        NULL
      }
    })
    
    output$moPlot <- renderPlotly({
      
      plotData <- filteredData()
      
      moBreakdown <- plotData %>%
        group_by(Microorganism) %>%
        summarize(Count = n()) %>%
        ungroup() %>%
        arrange(desc(Count)) %>%
        head(10) %>%
        mutate(Microorganism = factor(Microorganism, levels = rev(Microorganism)))
      
      moBreakdown$alpha <- scales::rescale(moBreakdown$Count, to = c(0.6, 1))
      
      p <- plot_ly(data = moBreakdown, x = ~Count, y = ~Microorganism, type = 'bar', orientation = 'h',
              marker = list(color = '#44cdc4', opacity = ~alpha), hoverinfo = "none",
              source = "moPlotSource") %>%
        layout(
          xaxis = list(
            title = "",
            fixedrange = TRUE
          ),
          yaxis = list(
            title = "",
            fixedrange = TRUE,
            ticksuffix = "  "
          ),
          margin = list(l = 0, r = 0, t = 0, b = 0),
          hovermode = 'closest',
          plot_bgcolor = 'white',
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
      
      p <- event_register(p, "plotly_click")
      
      p
    })
    
    output$moContent <- renderUI({
      
      plotData <- filteredData()
      
      if(nrow(plotData) >= 1){
        plotlyOutput(ns("moPlot"))
      } else {
        div(style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
            icon("disease", style = "font-size:100px; color: #44CDC4"),
            h4("Oops... looks like there isn't enough data for this plot."),
            h6("Try reducing the number of filters applied or adjust your data in the 'Import' tab.")
        )
      }
    })
    
    output$pathogenInfo <- renderUI({
      if (is.na(input$moFilter)) {
        info <- NULL
      } else {
        name <- input$moFilter
        info <- moInformation %>%
          filter(Microorganism %in% name)
        
        if (nrow(info) == 0) {
          name <- mo_genus(input$moFilter)
          info <- moInformation %>%
            filter(Microorganism %in% name)
        }
      }
      
      tagList(
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-triangle-exclaimation"></i> Microorganism</span>')),
        h5(paste(name)),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-circle-info"></i> Precautions</span>')),
        h5(info$Precautions),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-clipboard"></i> Notes</span>')),
        h5(info$Information),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-microscope"></i> Gram Stain</span>')),
        h5(str_to_sentence(mo_gramstain(input$moFilter))),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-lungs"></i> Oxygen Tolerance</span>')),
        h5(str_to_sentence(mo_oxygen_tolerance(input$moFilter))),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-prescription"></i> Treatment Considerations</span>')),
        h5(info$Treatment),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-chart-simple"></i> Epidemiology</span>')),
        h5(info$Epidemiology),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-stethoscope"></i> Diagnosis</span>')),
        h5(info$Diagnosis),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-square-plus"></i> Associated Syndromes</span>')),
        h5(info$Syndromes),
        h4(HTML('<span style="color: #44CDC4;"><i class="fa-solid fa-circle-info"></i> Additional Information</span>')),
        h5(info$`Additional Information`)
      )
    })
    
    observeEvent(event_data("plotly_click", source = "moPlotSource"), {
      click_data <- event_data("plotly_click", source = "moPlotSource")
      if (!is.null(click_data)) {
        updateSelectizeInput(session, "moFilter", selected = click_data$y)
      }
    })
    
    observeEvent(input$filters, {
      showModal(modalDialog(
        title = "Filter Data",
        selectizeInput(ns("species"), "Species", choices = unique(data$Species), selected = filters$species, multiple = TRUE),
        selectizeInput(ns("site"), "Site", choices = unique(data$Source), selected = filters$site, multiple = TRUE),
        selectizeInput(ns("region"), "Region", choices = unique(data$Region), selected = filters$region, multiple = TRUE),
        selectizeInput(ns("subregion"), "Subregion", choices = unique(data$Subregion), selected = filters$subregion, multiple = TRUE),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("applyFilters"), "Apply Filters")
        )
      ))
    })
    
    observeEvent(input$applyFilters, {
      filters$species <- input$species
      filters$site <- input$site
      filters$region <- input$region
      filters$subregion <- input$subregion
      removeModal()
    })
    
    
  })
}

