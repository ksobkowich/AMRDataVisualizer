# To-do -------------------------------------------------------------------
# - Confidence bands?

#' UI for the trends tab module.
#'
#' @param id  Module ID.
#' @return    Module UI.
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # Main Content ------------------------------------------------------------

      column(9, uiOutput(ns("content"))),

      # Filters -----------------------------------------------------------------

      column(
        3,
        filter_panel$ui(ns("filters")),

        # Controls ------------------------------------------------------------------

        bsCollapse(
          id = "collapsePanel",
          open = NULL,
          multiple = TRUE,
          bsCollapsePanel(
            HTML(
              "Controls <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
              style='float: right; color: #aaa;'></span>"
            ),

            # Metric toggle
            radioGroupButtons(
              ns("metric"),
              "Metric",
              choices = c("% Susceptible", "% Organism Prevalence"),
              direction = "vertical",
              selected = "% Susceptible",
              justified = TRUE
            ),

            # Data source toggle (only shown for prevalence)
            conditionalPanel(
              condition = sprintf("input['%s'] == '%% Organism Prevalence'", ns("metric")),
              radioGroupButtons(
                ns("dataSource"),
                "Data Source",
                choices = c("AST isolates only" = "ast", "All cultures" = "all"),
                direction = "vertical",
                selected = "ast",
                justified = TRUE
              ),
              radioGroupButtons(
                  ns("timePeriod"),
                  "Bin by",
                  choices = c("Month", "Quarter", "Year"),
                  direction = "vertical",
                  selected = "Month",
                  justified = TRUE
                )
              ),

            radioGroupButtons(
              ns("tsType"),
              "Smoothing",
              choices = c("None", "Rolling Mean", "LOWESS"),
              direction = "vertical",
              selected = "None",
              justified = TRUE
            ),

            # Rolling Mean slider (always exists, conditionally shown)
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Rolling Mean'", ns("tsType")),
              sliderInput(
                ns("rmWindow"),
                "Window",
                min = 2,
                max = 12,
                value = 2,
                step = 1
              )
            ),

            # LOWESS slider (always exists, conditionally shown)
            conditionalPanel(
              condition = sprintf("input['%s'] == 'LOWESS'", ns("tsType")),
              sliderInput(
                ns("lowessSpan"),
                "Span",
                min = 0.1,
                max = 1,
                value = 0.1,
                step = 0.1
              )
            )
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

#' Server logic for the trends tab module.
#'
#' @param id            The ID of the module.
#' @param reactiveData  A reactive that returns the cleaned data.
#' @param allCulturesData A reactive that returns all cultures data (optional).
#' @return              None.
server <- function(id, reactiveData, allCulturesData = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    # Active data source: switches between AST-only and all-cultures data based
    # on the user-selected metric and data source toggle. This reactive feeds
    # the filter panel, so when the user switches data sources, the filter
    # dropdowns update to reflect the new dataset's contents.
    activeData <- reactive({
      use_all_cultures <- !is.null(input$metric) &&
        input$metric == "% Organism Prevalence" &&
        !is.null(input$dataSource) &&
        input$dataSource == "all"

      if (use_all_cultures) {
        d <- allCulturesData()
        # Fall back to AST data if all-cultures data isn't available
        # (user didn't opt in during import)
        if (is.null(d) || nrow(d) == 0) {
          return(reactiveData())
        }
        return(d)
      }
      reactiveData()
    })

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------

    filters <- filter_panel$server(
      "filters",
      activeData,  # NEW: switches between AST-only and all-cultures data
      default_filters = c("Antimicrobial", "Microorganism", "Species", "Source", "Date"),
      auto_populate = list(Antimicrobial = TRUE, Microorganism = TRUE)
    )

    # ------------------------------------------------------------------------------
    # More reactives (downstream of filter panel)
    # ------------------------------------------------------------------------------

    plotData <- reactive({
      filters$filteredData()
    })

    initialData <- reactive({
      reactiveData()
    })

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------

    #' Assign time-based bins to dates.
    #'
    #' @param dates Vector of Date objects
    #' @param period One of "Month", "Quarter", "Year"
    #' @return Data frame with columns: Date (original), bin_id, bin_label (for display)
    assign_time_bins <- function(dates, period = "Month") {
      df <- data.frame(Date = dates) %>%
        arrange(Date) %>%
        distinct()
      
      if (period == "Month") {
        df <- df %>%
          mutate(
            bin_id = format(Date, "%Y-%m"),
            bin_label = format(Date, "%b %Y")  # e.g., "Jan 2023"
          )
      } else if (period == "Quarter") {
        df <- df %>%
          mutate(
            year = year(Date),
            quarter = quarter(Date),
            bin_id = paste0(year, "-Q", quarter),
            bin_label = paste0("Q", quarter, " ", year)  # e.g., "Q1 2023"
          ) %>%
          select(-year, -quarter)
      } else {  # Year
        df <- df %>%
          mutate(
            bin_id = format(Date, "%Y"),
            bin_label = format(Date, "%Y")  # e.g., "2023"
          )
      }
      
      return(df)
    }

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    output$content <- renderUI({
      req(plotData())
      if (!is.null(plotData()) && nrow(plotData()) > 0) {
        tagList(
          uiOutput(ns("idWarning")), # warning shown when ID column is missing in prevalence mode
          wellPanel(
            style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh;",
            div(style = "min-height: 750px", plotlyOutput(ns("plot"), height = "71vh")),
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
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
        icon("disease", style = "font-size:100px; color: #44CDC4"),
        h4("Oops... looks like there isn't enough data for this plot."),
        h6("Try reducing the number of filters applied or adjust your data in the 'Import' tab.")
      )
    })

    # Warn user when ID column is missing during prevalence analysis.
    # Without an ID column, isolates tested against multiple antimicrobials
    # will be double-counted, inflating prevalence estimates.
    output$idWarning <- renderUI({
      req(input$metric == "% Organism Prevalence")
      req(plotData())
      d <- plotData()
      if (!"ID" %in% names(d) || all(is.na(d$ID))) {
        div(
          style = "background-color: #fff3cd; border: 1px solid #ffeeba; color: #856404; 
                   padding: 10px; border-radius: 4px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          strong(" Warning:"),
          " No 'ID' column is mapped in your data. Without unique isolate IDs, isolates tested 
          against multiple antimicrobials will be counted multiple times, which will inflate 
          prevalence estimates. To fix this, return to the 'Import' tab and map an ID column."
        )
      }
    })

    output$plot <- renderPlotly({
      
      # ----------------------------------------------------------------------------
      # Build tsData based on selected metric
      # ----------------------------------------------------------------------------
      if (input$metric == "% Susceptible") {
        
        groupLabel <- "Antimicrobial"
        yLabel <- "% Susceptible"
        hoverNumLabel <- "% Susceptible"
        countLabel <- "Isolates tested"
        
        tsData <- plotData() %>%
          select(Date, Antimicrobial, Interpretation) %>%
          mutate(Interpretation = ifelse(
            (!is.na(Interpretation) & Interpretation == "S"), 1, 0
          )) %>%
          mutate(Date = as.Date(Date)) %>%
          group_by(Date, Antimicrobial) %>%
          summarize(Numerator = sum(Interpretation), Count = n(), .groups = "drop") %>%
          rename(Group = Antimicrobial) %>%
          arrange(Group, Date)
        
      } else {
        # ----- % Organism Prevalence -----
        groupLabel <- "Microorganism"
        yLabel <- "% Organism Prevalence"
        hoverNumLabel <- "% Prevalence"
        countLabel <- "Total isolates"
        
        # Deduplicate: one row per isolate (so each isolate is counted once)
        isolates <- plotData() %>%
          select(ID, Date, Microorganism) %>%
          distinct() %>%
          mutate(Date = as.Date(Date)) %>%
          filter(!is.na(Date))
        
        # ------------------------------------------------------------
        # Time-based binning for prevalence.
        # Each bin represents a fixed time period (month/quarter/year)
        # so all organisms share the same temporal boundaries.
        # ------------------------------------------------------------
        
        # Assign time bins to each date
        timeBins <- assign_time_bins(
          unique(isolates$Date), 
          period = input$timePeriod
        )
        
        # Join bin assignments to isolates
        isolates <- isolates %>%
          left_join(timeBins, by = "Date")
        
        # Compute per-bin totals (denominator) and use last date in bin as label
        binSummary <- isolates %>%
          group_by(bin_id) %>%
          summarize(
            Count = n(),
            Date = max(Date),  # Last date in bin for x-axis positioning
            bin_label = first(bin_label),
            .groups = "drop"
          )
        
        # Aggregate per (bin, organism) for numerators
        tsData <- isolates %>%
          group_by(bin_id, Microorganism) %>%
          summarize(Numerator = n(), .groups = "drop") %>%
          left_join(binSummary, by = "bin_id") %>%
          transmute(
            Date = Date,
            Group = Microorganism,
            Numerator = Numerator,
            Count = Count,
            bin_label = bin_label  # Keep for potential hover text enhancement
          ) %>%
          arrange(Group, Date)
      }
      
      tsData$Date <- as.Date(tsData$Date)
      
      # ----------------------------------------------------------------------------
      # For % Susceptibility mode, apply the per-antimicrobial rolling bin logic.
      # (Prevalence mode already produced binned tsData above using shared bins.)
      # ----------------------------------------------------------------------------
      if (input$metric == "% Susceptible") {
        
        #' Bins consecutive time points so each bin has at least 30 in the denominator.
        #'
        #' @param df A data frame with columns Date, Group, Numerator, Count.
        #' @return A binned data frame with the same columns.
        roll_forward <- function(df) {
          new_df <- df[1, ]
          new_df$Count <- 0
          new_df$Numerator <- 0
          
          for (i in 1:nrow(df)) {
            new_df$Numerator[nrow(new_df)] <- new_df$Numerator[nrow(new_df)] + df$Numerator[i]
            new_df$Count[nrow(new_df)] <- new_df$Count[nrow(new_df)] + df$Count[i]
            new_df$Date[nrow(new_df)] <- df$Date[i]
            
            if (new_df$Count[nrow(new_df)] >= 30) {
              if (i < nrow(df)) {
                new_df <- rbind(new_df, df[i + 1, ])
                new_df$Count[nrow(new_df)] <- 0
                new_df$Numerator[nrow(new_df)] <- 0
              }
            }
          }
          return(new_df)
        }
        
        tsData <- tsData %>%
          group_by(Group) %>%
          group_modify(~ roll_forward(.)) %>%
          mutate(propS = ifelse(Count > 0, (Numerator / Count) * 100, NA_real_)) %>%
          filter(Count >= 30, is.finite(propS), !is.na(Date))
      } else {
        # Prevalence: keep all bins (including the incomplete final bin).
        tsData <- tsData %>%
          mutate(propS = ifelse(Count > 0, (Numerator / Count) * 100, NA_real_)) %>%
          filter(is.finite(propS), !is.na(Date))
      }
            
      # ----------------------------------------------------------------------------
      # Helper: build hover text
      # ----------------------------------------------------------------------------
      make_hover <- function(group_vals, count_vals, y_vals, date_vals, bin_labels = NULL) {
        date_display <- if (!is.null(bin_labels)) bin_labels else as.character(date_vals)
        paste0(
          groupLabel, ": ", group_vals,
          "<br>", countLabel, ": ", count_vals,
          "<br>", hoverNumLabel, ": ", round(y_vals, 3),
          "<br>Period: ", date_display
        )
      }
      
      # ----------------------------------------------------------------------------
      # Plot — branch on smoothing type
      # ----------------------------------------------------------------------------
      if (input$tsType == "Rolling Mean") {
        
        k <- as.integer(input$rmWindow)
        
        tsDataRM <- tsData %>%
          group_by(Group) %>%
          arrange(Date) %>%
          mutate(
            ma_propS = if (n() >= k) {
              zoo::rollmean(propS, k = k, fill = NA, align = "right")
            } else {
              NA_real_
            }
          )
        
        numColors <- length(unique(tsDataRM$Group))
        colorPalette <- get_gg_color_hue(numColors)
        
        plot_ly(
          tsDataRM,
          x = ~Date,
          y = ~ma_propS,
          type = 'scatter',
          mode = 'lines+markers',
          color = ~Group,
          colors = colorPalette,
          text = ~make_hover(Group, Count, ma_propS, Date, 
                            if ("bin_label" %in% names(tsDataRM)) tsDataRM$bin_label else NULL),
          hoverinfo = "text"
        ) %>%
          layout(
            title = "",
            legend = list(title = list(text = groupLabel)),
            xaxis = list(title = "Date"),
            yaxis = list(title = yLabel, range = c(0, 100))
          ) %>%
          config(displayModeBar = FALSE)
        
      } else if (input$tsType == "LOWESS") {
        
        #' Apply LOWESS smoothing to a data frame.
        #'
        #' @param df Data frame.
        #' @param x  Name of the x column.
        #' @param y  Name of the y column.
        #' @param f  LOWESS span.
        #' @return   Data frame with a new low_propS column.
        apply_lowess <- function(df, x, y, f) {
          
          if (is.null(f) || !is.finite(f) || f <= 0) {
            df$low_propS <- NA_real_
            return(df)
          }
          
          df <- df %>%
            filter(is.finite(.data[[y]]), !is.na(.data[[x]]))
          
          if (nrow(df) < 3) {
            df$low_propS <- NA_real_
            return(df)
          }
          
          lw <- stats::lowess(df[[x]], df[[y]], f = f)
          df$low_propS <- lw$y
          df
        }
        
        tsDataLowess <- tsData %>%
          group_by(Group) %>%
          nest() %>%
          mutate(
            data = map(
              data,
              ~ apply_lowess(.x, "Date", "propS", input$lowessSpan)
            )
          ) %>%
          unnest(cols = data) %>%
          ungroup()
        
        numColors <- length(unique(tsDataLowess$Group))
        colorPalette <- get_gg_color_hue(numColors)
        
        plot_ly(
          tsDataLowess,
          x = ~Date,
          y = ~low_propS,
          type = 'scatter',
          mode = 'lines+markers',
          color = ~Group,
          colors = colorPalette,
          text = ~make_hover(Group, Count, low_propS, Date,
                            if ("bin_label" %in% names(tsDataLowess)) tsDataLowess$bin_label else NULL),
          hoverinfo = "text"
        ) %>%
          layout(
            title = "",
            legend = list(title = list(text = groupLabel)),
            xaxis = list(title = "Date"),
            yaxis = list(title = yLabel, range = c(0, 100))
          ) %>%
          config(
            displayModeBar = TRUE,
            modeBarButtonsToRemove = c(
              "zoom2d",
              "pan2d",
              "select2d",
              "lasso2d",
              "zoomIn2d",
              "zoomOut2d",
              "autoScale2d",
              "resetScale2d",
              "hoverClosestCartesian",
              "hoverCompareCartesian"
            ),
            modeBarButtonsToAdd = c(
              'drawline',
              'drawcircle',
              'drawrect',
              'eraseshape'
            ),
            toImageButtonOptions = list(
              format = "png",
              height = 850,
              width = 1250,
              scale = 3,
              filename = paste(Sys.Date(), "AMRVisualizerTrends", sep = "_")
            )
          )
        
      } else {
        # ----- No smoothing -----
        numColors <- length(unique(tsData$Group))
        colorPalette <- get_gg_color_hue(numColors)
        
        plot_ly(
          tsData,
          x = ~Date,
          y = ~propS,
          type = 'scatter',
          mode = 'lines+markers',
          color = ~Group,
          colors = colorPalette,
          text = ~make_hover(Group, Count, propS, Date,
                            if ("bin_label" %in% names(tsData)) tsData$bin_label else NULL),
          hoverinfo = "text"
        ) %>%
          layout(
            title = "",
            legend = list(orientation = 'h', title = list(text = groupLabel)),
            xaxis = list(title = "Date"),
            yaxis = list(title = yLabel, range = c(0, 100))
          ) %>%
          config(
            displaylogo = FALSE,
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

    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    observeEvent(input$save_btn, {
      session$sendCustomMessage(
        "savePlot",
        list(
          plotId = ns("plot"),
          filename = paste0(Sys.Date(), "AMRVisualizerTimeSeries"),
          width = 1200,
          height = 800,
          scale = 3
        )
      )
    })

    # ------------------------------------------------------------------------------
    # Module return
    # ------------------------------------------------------------------------------
  })
}

trends_tab <- list(
  ui = ui,
  server = server
)
