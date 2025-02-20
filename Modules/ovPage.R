ovPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8, 
             h4("Tests over time"),
             wellPanel(
               plotOutput(ns("nOverTimePlot"), height = "35vh"),
               class = "contentWell"
             )
      ),
      column(4, 
             h4("Species"),
             wellPanel(
               plotOutput(ns("speciesPlot"), height = "35vh"),
               class = "contentWell"
             )
      ),
    ),
    fluidRow(
      column(6, 
             h4("Antimicrobials: Number of tests results"),
             wellPanel(
               plotlyOutput(ns("abPlot"), height = "35vh"),
               class = "contentWell"
             )
      ),
      column(6, 
             h4("Microorganisms: 10 most common"),
             wellPanel(
               plotlyOutput(ns("moPlot"), height = "35vh"),
               class = "contentWell"
             )
      ),
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
    
    output$speciesPlot <- renderPlot({
      speciesBreakdown <- data %>% 
        group_by(Date, ID, Microorganism) %>% 
        slice(1) %>% 
        ungroup() %>% 
        group_by(Species) %>% 
        summarize(Count = n()) %>% 
        ungroup
      
      ggplot(speciesBreakdown, aes(x = 2, y = Count, fill = Species)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 1) +
        theme_void() +
        xlim(-3, 2.5) +
        scale_fill_manual(values = pal) +
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 12)
        )
    })
    
    output$moPlot <- renderPlotly({
      moBreakdown <- data %>% 
        group_by(Date, ID, Microorganism) %>% 
        slice(1) %>% 
        ungroup() %>% 
        group_by(Microorganism) %>% 
        summarize(Count = n()) %>% 
        ungroup() %>% 
        arrange(-Count) %>% 
        head(10) %>%
        mutate(Microorganism = factor(Microorganism, levels = Microorganism[order(-Count)])) %>% 
        mutate(tooltipText = paste("Count: ", Count))
      
      
      moBreakdown$alpha <- rescale(moBreakdown$Count, to = c(0.6, 1))
      
      plot_ly(data = moBreakdown, x = ~Count, y = ~Microorganism, type = 'bar', orientation = 'h',
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
        config(displayModeBar = FALSE)
      
    })
    
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
          treemapcolorway = colorPalette
        ) %>% 
        config(displayModeBar = FALSE)
    })
    
  })
}
