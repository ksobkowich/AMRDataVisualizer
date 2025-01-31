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
             
             filterPanelUI(ns("filters")),
             
             # Legend ------------------------------------------------------------------
             
             uiOutput(ns("legend"))
             
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


abPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    filteredData <-  filterPanelServer("filters", data, 
                                          default_filters = c("Microorganism", "Species", "Source", "Date"), 
                                          auto_populate = list())
    
    initialData <- reactive({
      data
    })
    
    plotData <- reactive({
        filteredData()
      })
    

# Legend Well -------------------------------------------------------------
    
    output$legend <- renderUI({
      req(input$abType)
      if(input$abType == 1 | is.null(input$abType)){
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
      } else {
        wellPanel(class = "legendWell")
      }
    })
    
# Content Well ------------------------------------------------------------
    
    abTypeSelected <- reactiveVal(1)
    
    observeEvent(input$abType, {
      abTypeSelected(input$abType) 
    })
    
    output$content <- renderUI({
      req(plotData())
      
      if (!is.null(plotData()) && nrow(plotData()) > 0) {
        tagList(
          wellPanel(
            style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh;",
            div(style = "height: 100%;",
                plotlyOutput(ns("plot"), height = "80vh")
            ),
            class = "contentWell"
          ),
          div(
            fluidRow(
              column(9, radioGroupButtons(ns("abType"), "Antibiogram Type:",
                                          choices = c("Visual" = 1, "Classic" = 2),
                                          selected = abTypeSelected()  # Use the reactive value here
              )),
              column(3, actionButton(ns("save_btn"), "Save", class = "plotSaveButton"))
            )
          )
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
    
    
    plot <- reactive({
      shorten_bacteria_names <- function(names) {
        str_replace_all(
          names,
          pattern = "\\b(\\w)\\w*\\s(\\w+)",
          replacement = "\\1. \\2"
        )
      }
      
      result_table <- plotData() %>%
        filter(Interpretation %in% c("S", "R", "I")) %>%
        mutate(Interpretation = ifelse(Interpretation == "S", 1, 0)) %>%
        group_by(Microorganism) %>%
        mutate(Frequency = n()) %>%
        ungroup() %>%
        {
          if (n_distinct(.$Microorganism) > 1) {
            filter(., Frequency >= min(tail(sort(unique(.$Frequency)), 15)))
          } else {
            .
          }
        } %>%
        group_by(Microorganism, Stain, Antimicrobial, Class) %>%
        summarise(
          obs = n(),
          prop = round(mean(Interpretation == 1), 3),
          .groups = 'drop'
        ) %>%
        mutate(size = cut(prop, breaks = c(-0.1, 0.7, 0.9, 1), labels = c("s", "m", "l")),
               short_form = shorten_bacteria_names(Microorganism)) %>%
        arrange(Class, Antimicrobial)
      
      vDividers <- result_table %>% 
        select(Class, Antimicrobial) %>% 
        distinct() %>% 
        group_by(Class) %>% 
        summarise(Count = n()) %>% 
        mutate(vDividers = cumsum(Count) + 0.5)
      
      result_table <- result_table %>% 
        left_join(vDividers, by = "Class")
      
      if(input$abType == 1){
        
        g <- ggplot(result_table, 
                    aes(x = interaction(Antimicrobial, Class),
                        y = short_form,
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
          geom_vline(aes(xintercept = vDividers), color = "grey25", linewidth = 0.3) +
          geom_point(shape = 21, stroke = 0.5, aes(alpha = ifelse(obs > 30, 1, obs / 30))) +
          scale_x_discrete(label = str_to_sentence(str_sub(unique(result_table$Antimicrobial), 1, 15))) +
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
            plot.background = element_rect(fill = 'white', color = NA),
            panel.grid.major = element_line(color = "grey90"),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(colour = "grey20"),
            axis.text.x = element_text(angle = 90, hjust = 1, color = "grey20"),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            panel.spacing = unit(0, "lines"), 
            panel.border = element_rect(color = "grey25", fill = NA, size = 0.8)
          ) +
          guides(fill = "none")
        
      } else {
        
        g <- ggplot(result_table, aes(x = interaction(Antimicrobial, Class), 
                                      y = short_form,
                                      text = paste("Microorganism:", Microorganism,
                                                   "<br>Antimicrobial:", Antimicrobial,
                                                   "<br>Class:", Class,
                                                   "<br>% Susceptible:", round(prop * 100, 2),
                                                   "<br>Isolates tested:", obs))) +
          geom_tile(aes(fill = size, alpha = ifelse(obs > 30, 1, obs / 30)), color = "white", size = 0.5, width = 1) +
          geom_vline(aes(xintercept = vDividers), color = "grey25", linewidth = 0.4) +
          geom_text(aes(label = ifelse(is.na(prop), "", round(prop * 100, 0))), color = "black", size = 2) +
          scale_fill_manual(
            values = c("l" = "#66D3C2", "m" = "#FFD966", "s" = "#FF8A66"),
            na.value = "transparent",
            guide = guide_legend(title = "Value Range")
          ) +
          scale_x_discrete(label = str_to_sentence(str_sub(unique(result_table$Antimicrobial), 1, 15))) +
          labs(
            title = "",
            x = "",
            y = ""
          ) +
          theme_minimal() +
          theme(
            legend.position = "none",
            panel.background = element_rect(fill = 'transparent'),
            plot.background = element_rect(fill = 'white', color = NA),
            panel.grid = element_blank(),
            axis.text.y = element_text(colour = "grey20"),
            axis.text.x = element_text(angle = 90, hjust = 1, color = "grey20"),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            panel.spacing = unit(0, "lines"), 
            panel.border = element_rect(color = "grey25", fill = NA, size = 0.8)
          )+
          guides(fill = "none")
        
      }
      
      
      plotly_plot <- ggplotly(g, tooltip = c("text")) %>%
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
        ) %>% 
        layout(
          yaxis = list(fixedrange = TRUE)  # Freeze the y-axis to prevent zooming/scrolling
        )
    })
    
    output$plot <- renderPlotly({
      plot()
    })
    
    output$errorHandling <- renderUI({
      div(style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
          icon("disease", style = "font-size:100px; color: #44CDC4"),
          h4("Oops... looks like there isn't enough data for this plot."),
          h6("Try reducing the number of filters applied or adjust your data in the 'Import' tab.")
      )
    })
    
    observeEvent(input$save_btn, {
      session$sendCustomMessage("savePlot", list(
        plotId = ns("plot"),
        filename = paste0(Sys.Date(), "AMRVisualizerAntibiogram"),
        width = 1200,
        height = 800,
        scale = 3
      ))
    })
    
  })
}