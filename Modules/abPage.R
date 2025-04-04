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
    )
    
  )
}

abPageServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    filters <- filterPanelServer(
      "filters", 
      data, 
      default_filters = c("Microorganism", "WHO AWaRe Class:", "Species", "Source", "Date"), 
      auto_populate = list()
    )
    
    plotData <- reactive({ filters$filteredData() })
    activeFilters <- reactive({ filters$activeFilters() })
    
    showColors <- reactiveVal(TRUE)
    aggByGenus <- reactiveVal(FALSE)
    abType <- reactiveVal("Classic")
    lowCounts <- reactiveVal("Include")
    yVar <- reactiveVal("Microorganism")
    sortBy <- reactiveVal("Frequency")
    numAb <- reactiveVal(15)
    splitGram <- reactiveVal(FALSE)
    splitData <- reactiveVal()
    plot2 <- reactiveVal()
    
    observeEvent(input$applyControl, {
      showColors(input$abColors)
      abType(input$abType)
      aggByGenus(input$aggGenus)
      lowCounts(input$handleLowCount)
      yVar(input$yVar)
      sortBy(input$sortBy)
      numAb(input$numAb)
      splitGram(input$splitGram)
    })
    
    
    # Render Legend -----------------------------------------------------------
    output$legend <- renderUI({
      
      if(abType() == "Simplified"){
        
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
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Microorganism'", ns("yVar")), 
              selectizeInput(ns("sortBy"), "Sort by", selected = "Frequency", choices = c("Alphabetical", "Frequency", "Gram Stain")),
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Source'", ns("yVar")), 
              selectizeInput(ns("sortBy"), "Sort by", selected = "Frequency", choices = c("Alphabetical", "Frequency")),
            ),
            
            radioGroupButtons(ns("abType"), label = "Antibiogram style:", selected = "Classic", choices = c("Classic", "Simplified")),
            radioGroupButtons(ns("handleLowCount"), label = "Handle low-count (<30) results", selected = "Include", choices = c("Include", "Exclude")),
            numericInput(ns("numAb"), "Maximum Rows", value = 15, step = 1, min = 1, max = 30),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Classic'", ns("abType")), 
              materialSwitch(ns("abColors"), label = "Show colors", value = TRUE)
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Microorganism'", ns("yVar")), 
              materialSwitch(ns("aggGenus"), label = "Aggregate by Genus", value = F)
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Microorganism'", ns("yVar")), 
              materialSwitch(ns("splitGram"), label = "Split by Gram Stain", value = F)
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
              if(abType() == "Classic"){
                if(splitGram() == TRUE && yVar() == "Microorganism"){
                  tagList(
                    h4("Gram Negative"),
                    withSpinner(DTOutput(ns("classicAB")), type = 4, color = "#44CDC4"),
                    hr(),
                    h4("Gram Positive"),
                    withSpinner(DTOutput(ns("classicAB2")), type = 4, color = "#44CDC4")
                  )
                } else {
                  withSpinner(DTOutput(ns("classicAB")), type = 4, color = "#44CDC4")
                }
              } else {
                withSpinner(plotlyOutput(ns("plot"), height = "750px"), type = 4, color = "#44CDC4")
              }
            ),
            class = "contentWell"
          ),
          downloadButton(ns("save_btn"), "Save", class = "plotSaveButton")
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
          Interpretation = ifelse(Interpretation == "S", 1, 0),
          Microorganism  = if (aggByGenus()) mo_genus(Microorganism) else Microorganism
        ) %>% 
        group_by(!!sym(yVar)) %>%
        mutate(Frequency = n()) %>%
        ungroup() %>%
        {
          if (n_distinct(.[[yVar]]) > 1) {
            filter(., Frequency >= min(tail(sort(unique(.$Frequency)), numAb())))
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
      
      # Simplified AB ---------------------------------------------------------------
      if(abType() == "Simplified"){
        
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
        
        df_wide <- result_table %>%
          select(!!sym(yVar), Antimicrobial, prop, obs) %>%
          mutate(prop = round(prop * 100, 0)) %>% 
          
          {
            if (lowCounts() == "Exclude") {
              filter(., obs >= 30)
            } else {
              .
            }
          } %>%
          
          pivot_wider(
            id_cols     = !!sym(yVar),
            names_from  = Antimicrobial,
            values_from = c(prop, obs),
            names_sep   = "_"
          ) %>% 
          rowwise() %>%
          mutate(`n =` = paste0(
            "(",
            min(c_across(starts_with("obs_")), na.rm = TRUE),
            " - ",
            max(c_across(starts_with("obs_")), na.rm = TRUE),
            ")"
          )) %>%
          ungroup() %>%
          select(!!sym(yVar), `n =`, everything())
        
        n_total <- ncol(df_wide)
        n_drug   <- (n_total - 2) / 2
        
        colnames(df_wide) <- gsub("prop_", "", colnames(df_wide))
        obs_cols <- which(grepl("obs_", names(df_wide)))
        
        drug_names <- names(df_wide)[3:(n_drug + 2)]
        drug_classes <- ab_group(drug_names)
        drug_group_list <- split(seq_along(drug_names), drug_classes)
        
        drug_class_starts <- sapply(drug_group_list, function(x) min(x)) + 1
        
        drug_targets <- 2:(n_drug + 2)
        
        combined_js <- JS(paste0(
          "function(td, cellData, rowData, row, col) {",
          "  var n_drug = (rowData.length - 2) / 2;",
          "  var obsIndex = col + n_drug;",
          "  var obsValue = parseFloat(rowData[obsIndex]);",
          "  var cellValue = parseFloat(cellData);",
          "  var tooltipText = '';",
          "  var showColors = ", tolower(as.character(showColors())), ";",
          
          "  if (!isNaN(obsValue)) {",
          "    tooltipText = 'Observations: ' + obsValue;",
          "  }",
          
          "  $(td).attr('title', tooltipText);",
          
          "  if (!isNaN(obsValue) && obsValue >= 30 && showColors === true) {",
          "    if (!isNaN(cellValue)) {",
          "      if (cellValue < 70) {",
          "        $(td).css({'background-color': '#D73027', 'color': 'white'});",
          "      } else if (cellValue < 90) {",
          "        $(td).css({'background-color': '#FEE08B'});",
          "      } else if (cellValue >= 90) {",
          "        $(td).css({'background-color': '#44CDC4', 'color': 'white'});",
          "      }",
          "    }",
          "  }",
          
          "  var drug_class_starts = [", paste(drug_class_starts, collapse = ","), "];",
          "  if (drug_class_starts.includes(col)) {",
          "    $(td).css({'border-left': '3px dashed black'});",
          "  }",
          "}"
        ))
        
        df_wide <- switch(
          sortBy(),
          "Frequency" = df_wide %>%
            rowwise() %>%
            mutate(total_obs = sum(c_across(starts_with("obs_")), na.rm = TRUE)) %>%
            ungroup() %>%
            arrange(desc(total_obs)) %>%
            select(-total_obs),
          "Alphabetical" = df_wide %>% arrange(!!sym(yVar)),
          "GramStain" = df_wide %>%
            mutate(gram = mo_gramstain(yVar)) %>%
            arrange(gram, !!sym(yVar)),
          df_wide
        )
        
        if(splitGram() == T && yVar == "Microorganism"){
          df_wide_neg <- df_wide %>%
            mutate(gram = mo_gramstain(Microorganism)) %>%
            filter(gram == "Gram-negative") %>%
            select(-gram)
          
          df_wide_pos <- df_wide %>%
            mutate(gram = mo_gramstain(Microorganism)) %>%
            filter(gram == "Gram-positive") %>%
            select(-gram)
          
          negTable <- datatable(
            df_wide_neg,
            rownames = FALSE,
            class = "cell-border",
            extensions = "FixedColumns",
            options = list(
              autoWidth = TRUE,
              scrollX = TRUE,
              scrollY = "350px",
              scrollCollapse = TRUE,
              fixedHeader = TRUE,
              dom = 't',
              fixedColumns = list(leftColumns = 2),
              paging = FALSE,
              ordering = FALSE,
              columnDefs = list(
                list(targets = obs_cols - 1, visible = FALSE),
                list(targets = drug_targets, createdCell = combined_js),
                list(targets = drug_targets, width = '15px'),
                list(
                  targets = 0:1,
                  createdCell = JS("function(td, cellData, rowData, row, col) {
          $(td).css({
            'max-width': '150px',
            'white-space': 'nowrap',
            'overflow': 'hidden',
            'text-overflow': 'ellipsis'
          });
        }")
                )
              ),
              headerCallback = JS(
                "function(thead, data, start, end, display) {",
                "  var $ths = $(thead).find('th');",
                "  var betterCells = [];",
                "  $ths.each(function(index) {",
                "    var cell = $(this);",
                "    if (index === 0 || index === 1) {",
                "      betterCells.push(cell.html());",
                "    } else {",
                "      var newDiv = $('<div>', {style: 'height: auto; width: 10px; transform: rotate(-90deg); white-space: nowrap;'});",
                "      var newInnerDiv = $('<div>', {text: cell.text()});",
                "      newDiv.append(newInnerDiv);",
                "      betterCells.push(newDiv);",
                "    }",
                "  });",
                "  $ths.each(function(i) {",
                "    $(this).html(betterCells[i]);",
                "  });",
                "  $(thead).find('th:first-child').html('');",
                "  $(thead).find('th').css({",
                "    'vertical-align': 'bottom',",
                "    'text-align': 'center',",
                "    'height': '120px'",
                "  });",
                "}"
              )
            )
          )
          
          posTable <- datatable(
            df_wide_pos,
            rownames = FALSE,
            class = "cell-border",
            extensions = "FixedColumns",
            options = list(
              autoWidth = TRUE,
              scrollX = TRUE,
              scrollY = "350px",
              scrollCollapse = TRUE,
              fixedHeader = TRUE,
              dom = 't',
              fixedColumns = list(leftColumns = 2),
              paging = FALSE,
              ordering = FALSE,
              columnDefs = list(
                list(targets = obs_cols - 1, visible = FALSE),
                list(targets = drug_targets, createdCell = combined_js),
                list(targets = drug_targets, width = '15px'),
                list(
                  targets = 0:1,
                  createdCell = JS("function(td, cellData, rowData, row, col) {
          $(td).css({
            'max-width': '150px',
            'white-space': 'nowrap',
            'overflow': 'hidden',
            'text-overflow': 'ellipsis'
          });
        }")
                )
              ),
              headerCallback = JS(
                "function(thead, data, start, end, display) {",
                "  var $ths = $(thead).find('th');",
                "  var betterCells = [];",
                "  $ths.each(function(index) {",
                "    var cell = $(this);",
                "    if (index === 0 || index === 1) {",
                "      betterCells.push(cell.html());",
                "    } else {",
                "      var newDiv = $('<div>', {style: 'height: auto; width: 10px; transform: rotate(-90deg); white-space: nowrap;'});",
                "      var newInnerDiv = $('<div>', {text: cell.text()});",
                "      newDiv.append(newInnerDiv);",
                "      betterCells.push(newDiv);",
                "    }",
                "  });",
                "  $ths.each(function(i) {",
                "    $(this).html(betterCells[i]);",
                "  });",
                "  $(thead).find('th:first-child').html('');",
                "  $(thead).find('th').css({",
                "    'vertical-align': 'bottom',",
                "    'text-align': 'center',",
                "    'height': '120px'",
                "  });",
                "}"
              )
            )
          )
          
          plot2(posTable)
          return(negTable)
          
        } else {
          
          datatable(
            df_wide,
            rownames = FALSE,
            class = "cell-border",
            extensions = "FixedColumns",
            options = list(
              autoWidth = TRUE,
              scrollX = TRUE,
              scrollY = "750px",
              scrollCollapse = TRUE,
              fixedHeader = TRUE,
              dom = 't',
              fixedColumns = list(leftColumns = 2),
              paging = FALSE,
              ordering = FALSE,
              columnDefs = list(
                list(targets = obs_cols - 1, visible = FALSE),
                list(targets = drug_targets, createdCell = combined_js),
                list(targets = drug_targets, width = '15px'),
                list(
                  targets = 0:1,
                  createdCell = JS("function(td, cellData, rowData, row, col) {
          $(td).css({
            'max-width': '150px',
            'white-space': 'nowrap',
            'overflow': 'hidden',
            'text-overflow': 'ellipsis'
          });
        }")
                )
              ),
              headerCallback = JS(
                "function(thead, data, start, end, display) {",
                "  var $ths = $(thead).find('th');",
                "  var betterCells = [];",
                "  $ths.each(function(index) {",
                "    var cell = $(this);",
                "    if (index === 0 || index === 1) {",
                "      betterCells.push(cell.html());",
                "    } else {",
                "      var newDiv = $('<div>', {style: 'height: auto; width: 10px; transform: rotate(-90deg); white-space: nowrap;'});",
                "      var newInnerDiv = $('<div>', {text: cell.text()});",
                "      newDiv.append(newInnerDiv);",
                "      betterCells.push(newDiv);",
                "    }",
                "  });",
                "  $ths.each(function(i) {",
                "    $(this).html(betterCells[i]);",
                "  });",
                "  $(thead).find('th:first-child').html('');",
                "  $(thead).find('th').css({",
                "    'vertical-align': 'bottom',",
                "    'text-align': 'center',",
                "    'height': '120px'",
                "  });",
                "}"
              )
            )
          )
          
        }
        
      } 
    })
    
    output$plot <- renderPlotly({
      plot()
    })
    
    output$classicAB <- renderDT({
      plot()
    })
    
    output$classicAB2 <- renderDT({
      plot2()
    })
    
    output$save_btn <- downloadHandler(
      
      filename = "Antibiogram.html",
      
      content = function(file) {
        src <- normalizePath("./Reports/Antibiogram.qmd")
        
        tmp <- tempdir()
        unlink(list.files(tmp, full.names = TRUE), recursive = TRUE, force = TRUE)
        
        owd <- setwd(tempdir())
        on.exit({
          setwd(owd)
          unlink(c("filters.RDS", 
                   "antibiogram_table.html", 
                   "antibiogram_table2.html", 
                   "antibiogram_table.png", 
                   "antibiogram_table2.png", 
                   "Antibiogram.qmd"), recursive = TRUE)
        })
        
        file.copy(src, "Antibiogram.qmd", overwrite = TRUE)
        
        htmltools::save_html(plot(), "antibiogram_table.html")
        
        html_lines <- readLines("antibiogram_table.html")
        
        table_data <- plot()
        num_columns <- ncol(table_data$x$data)
        num_rows <- nrow(table_data$x$data)
        col_width_px <- 25
        row_height_px <- 25
        vwidth <- num_columns * col_width_px + 300
        vheight <- num_rows * row_height_px + 300
        font_size <- max(12, 16 - 0.3 * num_columns)
        
        font_css <- sprintf('
  <link href="https://fonts.googleapis.com/css2?family=Carme&display=swap" rel="stylesheet">
  <style>
    body, table, td, th {
      font-family: "Carme", sans-serif !important;
            font-size: %dpx !important;
    }
  </style>', round(font_size))
        
        html_lines <- sub("</head>", paste0(font_css, "\n</head>"), html_lines)
        
        writeLines(html_lines, "antibiogram_table.html")
        
        webshot2::webshot(
          url = "antibiogram_table.html",
          file = "antibiogram_table.png",
          vwidth = vwidth,
          vheight = vheight,
          zoom = 2
        )
        
        if (splitGram()) {
          htmltools::save_html(plot2(), "antibiogram_table2.html")
          html_lines2 <- readLines("antibiogram_table2.html")
          html_lines2 <- sub("</head>", paste0(font_css, "\n</head>"), html_lines2)
          writeLines(html_lines2, "antibiogram_table2.html")

          table_data2 <- plot2()
          num_columns2 <- ncol(table_data2$x$data)
          num_rows2 <- nrow(table_data2$x$data)
          vwidth2 <- num_columns2 * col_width_px + 300
          vheight2 <- num_rows2 * row_height_px + 300

          webshot2::webshot(
            url = "antibiogram_table2.html",
            file = "antibiogram_table2.png",
            vwidth = vwidth2,
            vheight = vheight2,
            zoom = 2
          )
        }
        
        saveRDS(activeFilters(), "filters.RDS")
        writeLines(lowCounts(), "low_counts_flag.txt")
        
        quarto::quarto_render(
          input = "Antibiogram.qmd",
          output_format = "html",
          output_file = "Antibiogram.html"
        )
        
        file.rename("Antibiogram.html", file)
      }
    )

  })
}
