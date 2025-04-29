ovPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8, 
             h4("Tests over time"),
             wellPanel(
               withSpinner(plotOutput(ns("nOverTimePlot"), height = "35vh"), type = 4, color = "#44CDC4"),
               class = "contentWell"
             )
      ),
      
      column(4, 
             h4("Summary Stats"),
             wellPanel(
               div(
                 id = "summary-stats-table-container",
                 withSpinner(tableOutput(ns("summaryTable")), type = 4, color = "#44CDC4")
               ),
               class = "contentWell",
               style = "height: 39vh; overflow: scroll;"
             )
      ),
    ),
    
    fluidRow(
      column(6, 
             h4("Antimicrobials: Number of tests results"),
             wellPanel(
               withSpinner(plotlyOutput(ns("abPlot"), height = "35vh"), type = 4, color = "#44CDC4"),
               class = "contentWell"
             )
      ),
      
      column(6, 
             h4("Frequency Plot"),
             wellPanel(
               uiOutput(ns("freqWell")),
               class = "contentWell",
               style = "height: 39vh; overflow-y: auto;"
             )
      )
    )
  )
}

ovPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pal <- c("#44cdc4", "#30908c", "#d4fffe", "#aafffc")
    
    nOverTime <- data %>%
      group_by(Date, ID, Microorganism) %>% 
      slice(1) %>% 
      ungroup() %>% 
      group_by(Date) %>%
      summarize(Tests = n()) %>%
      mutate(Date = as.Date(Date)) %>%
      ungroup()
    
    output$nOverTimePlot <- renderPlot({
      ggplot(nOverTime, aes(x = Date, y = Tests, group = 1)) +
        geom_area_pattern(
          colour = '#44CDC4',
          pattern = 'gradient',
          fill = NA,
          pattern_fill = "transparent",
          pattern_fill2 = "#E6F9F7"
        ) +
        scale_x_date(date_breaks = "12 months", date_labels = "%b %Y") +
        theme(
          panel.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
          plot.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.grid.minor.y = element_line(colour = "grey90"),
          axis.text.x = element_text(size = 12)
        )
    })
    
    output$summaryTable <- renderTable({
      
      #Calculations
      unique_microorganisms <- data %>% distinct(Microorganism) %>% nrow()
      unique_antimicrobials <- data %>% distinct(Antimicrobial) %>% nrow()
      n_samples <- data %>%
        distinct(ID, Date) %>%
        nrow()
      n_isoaltes <- num_samples <- data %>%
        distinct(ID, Date, Microorganism) %>%
        nrow()
      mean_isolates_per_sample <- data %>%
        distinct(ID, Date, Microorganism) %>%
        count(ID, Date) %>%
        pull(n) %>%
        mean()
      n_tests <- nrow(data)
      mean_test_per_isolate <- data %>%
        distinct(ID, Date, Microorganism, Antimicrobial) %>%
        count(ID, Date, Microorganism) %>%
        pull(n) %>%
        mean()
      
      data.frame(
        Metric = c(
          "Unique microorganisms",
          "Unique antimicrobials",
          "Total samples",
          "Total isolates",
          "Total tests",
          "Mean isolates per sample",
          "Mean tests per isolate"
        ),
        Value = c(
          format(round(unique_microorganisms, 0), big.mark = ","),
          format(round(unique_antimicrobials, 0), big.mark = ","),
          format(round(n_samples, 0), big.mark = ","),
          format(round(n_isoaltes, 0), big.mark = ","),
          format(round(n_tests, 0), big.mark = ","),
          format(round(mean_isolates_per_sample, 2), big.mark = ","),
          format(round(mean_test_per_isolate, 2), big.mark = ",")
        ),
        stringsAsFactors = FALSE
      )
    }, striped = FALSE, bordered = FALSE, hover = FALSE, spacing = 's')
    
    output$abPlot <- renderPlotly({
      abBreakdown <- data %>%
        group_by(Antimicrobial, Class) %>%
        summarize(Count = n(), .groups = "drop") %>%
        arrange(-Count) %>%
        mutate(ids = ifelse(Class == "", Antimicrobial, paste0(Antimicrobial, "-", Class)))
      
      parentInfo <- abBreakdown %>%
        group_by(Class) %>%
        summarise(Count = sum(Count)) %>%
        rename(Antimicrobial = Class) %>%
        mutate(Class = "", ids = Antimicrobial) %>%
        select(names(abBreakdown))
      
      plotData <- rbind(abBreakdown, parentInfo)
      
      numColors <- length(unique(plotData$Class))
      
      gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
      }
      
      colorPalette = gg_color_hue(numColors)
      
      plot_ly(
        data = plotData, 
        branchvalues = "total",
        type = "treemap",
        labels = ~Antimicrobial,
        parents = ~Class,
        values = ~Count,
        ids = ~ids,
        maxdepth = 2
      ) %>%
        layout(
          showlegend = FALSE,
          margin = list(t = 0, b = 0, l = 0, r = 0),
          hovermode = 'closest',
          modebar = list(add = NULL, remove = "all"),
          colorway = colorPalette
        ) %>%
        config(p,
               modeBarButtonsToAdd = list('toImage'),
               toImageButtonOptions = list(
                 height = 800,
                 width = 1200,
                 scale = 2
               ),
               displaylogo = FALSE)
    })
    
    
    output$freqWell <- renderUI({
      tagList(
        selectizeInput(ns("freqPlotSelect"),
                       label = NULL,
                       choices = c("Antimicrobial", "Class", "Microorganism", "Region", "Source", "Species", "Subregion"),
                       selected = "Microorganism"),
        withSpinner(plotlyOutput(ns("freqPlot"), height = "30vh"), type = 4, color = "#44CDC4")
      )
    })
    
    output$freqPlot <- renderPlotly({
      
      req(input$freqPlotSelect)

      breakdown <- data %>%
        select(Date, ID, !!sym(input$freqPlotSelect)) %>% 
        group_by(Date, ID, !!sym(input$freqPlotSelect)) %>%
        slice(1) %>%
        ungroup() %>%
        group_by(!!sym(input$freqPlotSelect)) %>%
        summarize(Count = n(), .groups = "drop") %>%
        arrange(-Count) %>%
        slice_head(n = 10) %>%
        mutate(!!sym(input$freqPlotSelect) := factor(!!sym(input$freqPlotSelect),
                                                     levels = !!sym(input$freqPlotSelect))) %>%
        mutate(tooltipText = paste("Count:", Count))
      
      breakdown$alpha <- rescale(breakdown$Count, to = c(0.6, 1))
      
      plot_ly(data = breakdown, x = ~Count, y = ~get(input$freqPlotSelect), type = 'bar', orientation = 'h',
              marker = list(color = '#44cdc4', opacity = ~alpha), hoverinfo = ~tooltipText) %>%
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
        config(p,
               modeBarButtonsToAdd = list('toImage'),
               toImageButtonOptions = list(
                 height = 800,
                 width = 1200,
                 scale = 2
               ),
               displaylogo = FALSE)
      
    })
    
  })
}