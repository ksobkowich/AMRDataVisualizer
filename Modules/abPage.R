abPageUI <- function(id, data) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      
      # Main Content ------------------------------------------------------------
      
      column(9,
             uiOutput(ns("content"))
      ),
      
      # Side menus -----------------------------------------------------------------
      
      column(3,
             
             # Filters -----------------------------------------------------------------
             
             filterPanelUI(ns("filters")),
             
             # Plot controls -----------------------------------------------------------
             uiOutput(ns("controls")),
             
             # Legend ------------------------------------------------------------------
             
             uiOutput(ns("legend"))
             
      )
    ),
    
    
    # Save Plot Logic ---------------------------------------------------------
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
    
    filteredData <-  filterPanelServer("filters", 
                                       data, 
                                       default_filters = c("Microorganism", "WHO AWaRe Class:", "Species", "Source", "Date"), 
                                       auto_populate = list())
    
    plotData <- reactive({filteredData()})
    
    showColors <- reactiveVal(TRUE)
    aggByGenus <- reactiveVal(FALSE)
    abType <- reactiveVal("Classic")
    lowCounts <- reactiveVal("Include")
    yVar <- reactiveVal("Microorganism")
    
    observeEvent(input$applyControl, {
      showColors(input$abColors)
      abType(input$abType)
      aggByGenus(input$aggGenus)
      lowCounts(input$handleLowCount)
      yVar(input$yVar)
    })
    
    
    # Render Legend -----------------------------------------------------------
    output$legend <- renderUI({
      
      if(abType() == "Visual"){
        
        # Visual Legend -----------------------------------------------------------
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
          h6("Hover over a bubble for more details."),
          class = "legendWell"
        )
        
        # Classic Legend ----------------------------------------------------------
      } else {
        
        wellPanel(        
          h4("Legend", class = "legend-title"),
          h5("Color", class = "legend-section"),
          div(
            class = "legend-section",
            div(
              class = "legend-item",
              tags$i(icon("square"), style = "font-size: 20px; margin-left: 5px; color: grey;"),
              span("Too few observations", class = "legend-label", style = "margin-left: 15px;")
            ),
            div(
              class = "legend-item",
              tags$i(class = "fas fa-solid fa-square", style = "font-size: 20px; margin-left: 5px; color: #D73027;"),
              span("Low susceptibility (<70%)", class = "legend-label", style = "margin-left: 15px;")
            ),
            div(
              class = "legend-item",
              tags$i(class = "fas fa-solid fa-square", style = "font-size: 20px; margin-left: 5px; color: #FEE08B;"),
              span("Moderate susceptibility (70 - 90%)", class = "legend-label", style = "margin-left: 15px;")
            ),
            div(
              class = "legend-item",
              tags$i(class = "fas fa-solid fa-square", style = "font-size: 20px; margin-left: 5px; color: #44CDC4;"),
              span("High susceptibility (>90%)", class = "legend-label", style = "margin-left: 15px;")
            )
          ),
          h6("Vertical divisions represent antimicrobial class."),
          h6("Horizontal divisions represent microorganism gram stain."),
          h6("Hover over a cell for more details."),
          class = "legendWell")
      }
      
    })
    
    
    # Render Controls ---------------------------------------------------------
    output$controls <- renderUI({
      tagList(
        bsCollapse(
          id = "collapsePanel",
          open = NULL,
          multiple = T,
          bsCollapsePanel(
            HTML("Controls <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
            style='float: right; color: #aaa;'></span>"),         
            selectizeInput(ns("yVar"), "Y-axis variable", selected = "Microorganism", choices = c("Microorganism", "Source")),
            radioGroupButtons(ns("abType"), label = "Antibiogram style:", selected = "Classic", choices = c("Classic", "Visual")),
            radioGroupButtons(ns("handleLowCount"), label = "Handle low-count (<30) results", selected = "Include", choices = c("Include", "Exclude")),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Classic'", ns("abType")), 
              materialSwitch(ns("abColors"), label = "Show colors?", value = TRUE)
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Microorganism'", ns("yVar")), 
              materialSwitch(ns("aggGenus"), label = "Aggregate by Genus?", value = F)
            ),
            
            actionButton(ns("applyControl"), "Apply", class = "submitButton")
          )
        )
      )
    })
    
    
    # Render Content -------------------------------------------------------------
    output$content <- renderUI({
      data <- req(plotData())
      
      if (nrow(data) > 0) {
        
        # Show Plot ---------------------------------------------------------------
        tagList(
          wellPanel(
            style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh;",
            div(
              style = paste0("min-width: 900px;"),
              withSpinner(plotlyOutput(ns("plot"), height = "750px"), type = 4, color = "#44CDC4")
            ),
            class = "contentWell"
          ),
          actionButton(ns("save_btn"), "Save", class = "plotSaveButton")
        )
        
        # Show Error Handling -----------------------------------------------------
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
    
    
    # Define Error Handling ---------------------------------------------------
    
    output$errorHandling <- renderUI({
      div(style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
          icon("disease", style = "font-size:100px; color: #44CDC4"),
          h4("Oops... looks like there isn't enough data for this plot."),
          h6("Try reducing the number of filters applied or adjust your data in the 'Import' tab.")
      )
    })    
    
    # Define Plot -------------------------------------------------------------
    plot <- reactive({
      
      shorten_bacteria_names <- function(names) {
        str_replace(
          names,
          pattern = "\\b(\\w)\\w*\\s(\\w+)",
          replacement = "\\1. \\2"
        )
      }
      
      yVar <- yVar()
      
      result_table <- plotData() %>%
        filter(Interpretation %in% c("S", "R", "I")) %>%
        mutate(
          # Convert "S" to 1, everything else to 0
          Interpretation = ifelse(Interpretation == "S", 1, 0),
          # Conditionally switch to mo_genus() if aggByGenus() is TRUE
          Microorganism  = if (aggByGenus()) mo_genus(Microorganism) else Microorganism
        ) %>% 
        group_by(!!sym(yVar)) %>%
        mutate(Frequency = n()) %>%
        ungroup() %>%
        {
          if (n_distinct(.[[yVar]]) > 1) {
            filter(., Frequency > min(tail(sort(unique(.$Frequency)), 15)))
          } else {
            .
          }
        } %>%
        group_by(!!sym(yVar), Antimicrobial, Class) %>%
        summarise(
          obs = n(),
          prop = round(mean(Interpretation == 1), 3),
          .groups = 'drop'
        ) %>%
        mutate(size = cut(prop, breaks = c(0, 0.7, 0.9, 1), labels = c("s", "m", "l")))
      
      result_table <- result_table %>% 
        { 
          if (yVar == "Microorganism") {
            mutate(result_table, short_form = shorten_bacteria_names(.[[yVar]]))
          } else {
            mutate(result_table, short_form = ifelse(str_length(.[[yVar]]) > 15, 
                                                     str_c(str_sub(.[[yVar]], 1, 15), "..."), 
                                                     .[[yVar]]))
          }
        }%>% 
        arrange(Class, Antimicrobial)
      
      uniqueDrugs <- result_table %>%
        distinct(Antimicrobial, Class, .keep_all = TRUE) %>%
        arrange(Class, Antimicrobial) %>%
        mutate(Antimicrobial = as.ab(Antimicrobial)) %>% 
        pull(Antimicrobial)
      
      # Visual AB ---------------------------------------------------------------
      if(abType() == "Visual"){
        
        if(lowCounts() == "Exclude") {
          result_table <- result_table %>% 
            filter(obs >= 30) %>% 
            mutate(alpha = 1)
          
        } else {
          
          result_table <- result_table %>% 
            mutate(alpha = ifelse(obs > 30, 1, obs / 30))
          
        }
        
        g <- ggplot(result_table, aes(x = interaction(Antimicrobial, Class),
                                      y = short_form,
                                      size = size,
                                      colour = Class,
                                      fill = Class,
                                      text = paste(yVar, !!sym(yVar),
                                                   "<br>Antimicrobial:", Antimicrobial,
                                                   "<br>Class:", Class,
                                                   "<br>% Susceptible:", round(prop * 100, 2),
                                                   "<br>Isolates tested:", obs)
        )
        ) +
          geom_point(shape = 21, stroke = 0.5, aes(alpha = alpha)) +
          scale_alpha_identity()+
          scale_x_discrete(label = ifelse(str_length(unique(data$Antimicrobial)) > 15, str_c(str_sub(unique(data$Antimicrobial), 1, 15), "..."), unique(data$Antimicrobial))) + 
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
            legend.background = element_rect(fill = 'transparent'),
            axis.text.y = element_text(colour = "grey20"),
            axis.text.x = element_text(angle = 90, hjust = 0, color = "grey20")
          ) +
          guides(fill = "none")
        
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
          )
        
        # Classic AB --------------------------------------------------------------
      } else {
        
        full_table <- result_table %>%
          select(!!sym(yVar), Antimicrobial) %>%
          distinct() %>%
          expand(!!sym(yVar), Antimicrobial)
        
        classicAB_table <- full_table %>%
          left_join(result_table, by = c(yVar, "Antimicrobial"))
        
        classicAB_table <- classicAB_table %>%
          mutate(
            obs = ifelse(is.na(obs), 0, obs),
            size = ifelse(is.na(size), "x", size),
            size = ifelse(obs < 30, "x", size),
            Class = ab_group(Antimicrobial)
          )
        
        if(lowCounts() == "Exclude"){
          classicAB_table <- classicAB_table %>% 
            mutate(prop = ifelse(size == "x", NA, prop),
                   obs = ifelse(size == "x", 0, obs)
            )
        }
        
        if(yVar == "Microorganism" && aggByGenus() == F){
          classicAB_table <- classicAB_table %>% 
            mutate(
              Gram = coalesce(mo_gramstain(Microorganism), "Unknown"),
              short_form = shorten_bacteria_names(Microorganism),
              short_form = factor(short_form, 
                                  levels = unique(classicAB_table %>%
                                                    arrange(Gram, short_form) %>%
                                                    pull(short_form)))
            )
          
          gram_count1 <- classicAB_table %>%
            select(Microorganism, Gram) %>%
            distinct() %>%
            group_by(Gram) %>%
            summarize(Count = n()) %>% 
            filter(Gram == "Gram-negative") %>%
            pull(Count)
          
          gram_count2 <- classicAB_table %>%
            select(Microorganism, Gram) %>%
            distinct() %>%
            group_by(Gram) %>%
            summarize(Count = n()) %>% 
            filter(Gram == "Gram-positive") %>%
            pull(Count)
          
          uniqueMO <- result_table %>% 
            select(Microorganism) %>% 
            distinct() %>% 
            nrow()
          
          gram_line1 = ifelse(is.integer(gram_count1) && length(gram_count1) == 0L, 0, gram_count1 + 0.5)
          gram_line2 = ifelse(is.integer(gram_count2) && length(gram_count2) == 0L || gram_count1 + gram_count2 == uniqueMO, 0, gram_count1 + gram_count2 + 0.5)
          
        } else {
          
          classicAB_table <- classicAB_table %>% 
            mutate(
              short_form = ifelse(str_length(.[[yVar]]) > 15, 
                                  str_c(str_sub(.[[yVar]], 1, 15), "..."), 
                                  .[[yVar]]),
              short_form = factor(short_form, 
                                  levels = unique(classicAB_table %>%
                                                    arrange(short_form) %>%
                                                    pull(short_form)))
            )
          
        }
        
        classicAB_table <- classicAB_table %>% 
          group_by(!!sym(yVar)) %>% 
          mutate(obs_range = paste0("(n = ", min(obs[obs > 0]), " - ", max(obs), ")"),
                 short_form = paste0(short_form, "\n", obs_range))
        
        if(showColors() == TRUE){
          color_palette <- c("1" = "#D73027", "2" = "#FEE08B", "3" = "#44CDC4", "x" = "white")
        } else {
          color_palette <- c("1" = "white", "2" = "white", "3" = "white", "x" = "white")
        }
        
        plot_list <- classicAB_table %>% 
          split(.$Class) %>% 
          lapply(function(data) {
            ggplot(data, aes(x = interaction(Antimicrobial, Class), 
                             y = short_form, 
                             fill = size,
                             text = paste(
                               yVar, !!sym(yVar), 
                               if (yVar == "Microorganism" & aggByGenus() == F) {
                                 paste("<br>Gram-stain:", Gram)
                               } else {
                                 ""
                               },
                               "<br>Antimicrobial: ", Antimicrobial,
                               "<br>Class: ", Class,
                               "<br>Observations: ", obs,
                               "<br>Proportion: ", round(prop * 100, 2), "%"
                             ))) +
              geom_tile(color = "grey90", size = 0.2) +
              geom_text(size = 3, color = "grey20", check_overlap = T,
                        aes(label = round(prop * 100, 0))) +
              { 
                if(yVar == "Microorganism" & aggByGenus() == F) {
                  list(
                    geom_hline(yintercept = gram_line1, color = "grey20", size = 0.5),
                    geom_hline(yintercept = gram_line2, color = "grey20", size = 0.5)
                  )
                } else {
                  NULL
                }
              } +
              scale_x_discrete(label = ifelse(str_length(unique(data$Antimicrobial)) > 15, str_c(str_sub(unique(data$Antimicrobial), 1, 15), "..."), unique(data$Antimicrobial))) + 
              #scale_y_discrete(label = paste0(short_form, "\n", obs_range)) +
              scale_fill_manual(values = color_palette) +
              labs(title = "", x = "", y = "") +
              theme_minimal() + 
              theme(
                legend.position = "none",
                panel.background = element_rect(fill = 'transparent'),
                panel.border = element_rect(colour = "grey20", fill = 'transparent', linewidth = 1.1),
                plot.background = element_rect(fill = 'white', color = NA),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.y = element_text(colour = "grey20"),
                axis.text.x = element_text(angle = 90, hjust = 1, color = "grey20"),
                strip.text.x = element_blank(),
                panel.spacing.x = unit(0, "mm")
              )
          })
        
        plotly_list <- lapply(plot_list, function(p) {
          ggplotly(p, tooltip = "text")
        })
        
        class_counts <- classicAB_table %>%
          group_by(Class) %>%
          summarise(n = n_distinct(Antimicrobial)) %>%
          mutate(prop_width = n / sum(n))
        
        widths <- class_counts$prop_width
        
        subplot(plotly_list, nrows = 1, widths = widths, margin = 0, shareY = TRUE) %>% 
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
                 ))
      }
    })
    
    output$plot <- renderPlotly({
      plot()
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