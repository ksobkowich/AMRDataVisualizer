#' UI for the map tab module.
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

        # Controls ----------------------------------------------------------------
        
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
            
            # Data source toggle (conditional on prevalence)
            conditionalPanel(
              condition = sprintf("input['%s'] == '%% Organism Prevalence'", ns("metric")),
              radioGroupButtons(
                ns("dataSource"),
                "Data Source",
                choices = c("AST isolates only" = "ast", "All cultures" = "all"),
                direction = "vertical",
                selected = "ast",
                justified = TRUE
              )
            )
          )
        ),

        # Legend ------------------------------------------------------------------

        uiOutput(ns("legend"))
      )
    )
  )
}

#' Server logic for the map tab module.
#'
#' @param id  The ID of the module.
#' @return    None.
server <- function(id, reactiveData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------

    filters <- filter_panel$server(
      "filters",
      reactiveData,
      default_filters = c("Antimicrobial", "Microorganism", "Species", "Source", "Date"),
      auto_populate = list(Antimicrobial = TRUE, Microorganism = TRUE)
    )

    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    plotData <- reactive({
      filters$filteredData()
    })

    activeFilters <- reactive({
      filters$activeFilters()
    })
    
    # Extract date filter for prevalence denominator calculation
    dateFilter <- reactive({
      af <- activeFilters()
      
      # Check if af is a list and has a Date element
      if (is.list(af) && !is.null(af$Date)) {
        # af$Date might itself be a list with start_date and end_date
        if (is.list(af$Date)) {
          list(
            start_date = af$Date$start_date,
            end_date = af$Date$end_date
          )
        } else {
          # If Date is not a list, return NULL (can't extract date range)
          NULL
        }
      } else {
        NULL
      }
    })

    baseMap <- reactive({
      req(reactiveData())
      preprocessMapData(reactiveData())
    })

    map_reactive <- reactive({
      req(baseMap())
      req(plotData())

      # Choose preprocessing function based on metric
      # For prevalence, use unfiltered data for denominator
      if (!is.null(input$metric) && input$metric == "% Organism Prevalence") {
        mapData <- preprocessPlotDataPrevalence(
          filteredData = plotData(),
          unfilteredData = reactiveData(),
          dateFilter = dateFilter()
        )
      } else {
        mapData <- preprocessPlotData(plotData())
      }
      
      mapData <- matchSubregions(baseMap(), mapData)

      # Determine join strategy based on what geographic data is present
      sample_subregions <- unique(reactiveData()$Subregion[!is.na(reactiveData()$Subregion)])
      has_subregion <- length(sample_subregions) > 0
      is_zip_code <- has_subregion && all(grepl("^\\d{3,5}$", sample_subregions))

      if (is_zip_code) {
        # ZIP codes: join only on Subregion (baseMap has Region = NA for ZIPs)
        map <- baseMap() %>%
          left_join(mapData, by = "Subregion") %>%
          mutate(Subregion = str_to_sentence(Subregion))
      } else if (has_subregion) {
        # Counties: join on both Region and Subregion
        map <- baseMap() %>%
          left_join(mapData, by = c("Region", "Subregion")) %>%
          mutate(Subregion = str_to_sentence(Subregion))
      } else {
        # State-level only (no Subregion data): join on Region only
        map <- baseMap() %>%
          left_join(mapData, by = "Region")
      }

      # Determine which column to use for coloring (define before use)
      colorColumn <- if (!is.null(input$metric) && input$metric == "% Organism Prevalence") {
        "propPrevalence"
      } else {
        "propS"
      }

      # Set color palette and breakpoints based on metric
      if (!is.null(input$metric) && input$metric == "% Organism Prevalence") {
        pal <- sequential_hcl(n = 7, palette = "Mako")
        breakpoints <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 1)
      } else {
        pal <- sequential_hcl(n = 6, palette = "Mako")
        breakpoints <- c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
      }
      color_pal <- colorBin(pal, domain = NULL, bins = breakpoints, na.color = "#999999")

      map1 <- map %>%
        filter(Count >= 30)

      map2 <- map %>%
        filter(Count < 30)

      # Only create hatching if there are regions with < 30 observations
      if (nrow(map2) > 0) {
        map2 <- map2 %>%
          mutate(ID = as.character(row_number()))
        
        # Store the color column value BEFORE dropping geometry
        map2 <- map2 %>%
          mutate(colorValue = get(colorColumn))
        
        map2.hatch <- hatched.SpatialPolygons(map2, density = 35, angle = c(0, 45, 90, 135))
        
        map2_data <- map2 %>%
          st_drop_geometry()
        
        map2.hatch <- map2.hatch %>%
          left_join(map2_data, by = "ID")
      } else {
        # No regions with < 30 observations, create empty sf object
        map2.hatch <- st_sf(geometry = st_sfc())
      }

      # Build popups based on metric
      if (!is.null(input$metric) && input$metric == "% Organism Prevalence") {
        # Prevalence popups
        popups <- paste0(
          "<div style='font-family: Carme, sans-serif; line-height: 1.4;'>",
          
          ifelse(
            !is.na(map$Subregion) & map$Subregion != "",
            paste0(
              "<h4 style='color: #44CDC4;'><b>",
              map$Subregion,
              " County, ",
              "<span style='color: #34435a;'>",
              map$Region,
              "</span></b></h4>"
            ),
            paste0("<h4 style='color: #44CDC4;'><b>", map$Region, "</b></h4>")
          ),
          
          "<hr style='border-top: 1px solid #cccccc;'>",
          
          "<i class='fa fa-bacterium' style='color: #44CDC4; font-size: 20px;'></i> <span style='font-family: Carme;'>: ",
          ifelse(
            length(unique(plotData()$Microorganism)) > 1,
            "Multiple selected",
            paste(unique(plotData()$Microorganism), collapse = ", ")
          ),
          "</span>",
          
          "<hr style='border-top: 1px solid #cccccc;'>",
          
          "<h5><b>Total isolates in region: </b>",
          format(round(as.integer(map$Count), 0), nsmall = 1, big.mark = ","),
          "</h5>",
          "<h5><b>Isolates of selected organism(s): </b>",
          format(round(as.integer(map$Numerator), 0), nsmall = 1, big.mark = ","),
          "</h5>",
          "<h5><b>% Prevalence: </b>",
          format(round(as.numeric(map$propPrevalence * 100), 1)),
          "%</h5>",
          "</div>"
        )
      } else {
        # Susceptibility popups (original)
        popups <- paste0(
          "<div style='font-family: Carme, sans-serif; line-height: 1.4;'>",

          # Only include the Subregion line if it is not NA or empty
          ifelse(
            !is.na(map$Subregion) & map$Subregion != "",
            paste0(
              "<h4 style='color: #44CDC4;'><b>",
              map$Subregion,
              " County, ",
              "<span style='color: #34435a;'>",
              map$Region,
              "</span></b></h4>"
            ),
            paste0("<h4 style='color: #44CDC4;'><b>", map$Region, "</b></h4>")
          ),

          "<hr style='border-top: 1px solid #cccccc;'>",

          "<i class='fa fa-bacterium' style='color: #44CDC4; font-size: 20px;'></i> <span style='font-family: Carme;'>: ",
          ifelse(
            length(unique(plotData()$Microorganism)) > 1,
            "Multiple selected",
            paste(unique(plotData()$Microorganism), collapse = ", ")
          ),
          "</span>",

          "<br>",

          "<i class='fa fa-pills' style='color: #34435a; font-size: 20px;'></i> <span style='font-family: Carme;'>: ",
          ifelse(
            length(unique(plotData()$Antimicrobial)) > 1,
            "Multiple selected",
            paste(unique(plotData()$Antimicrobial), collapse = ", ")
          ),
          "</span>",

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

          "<h5><b>Number of isolates tested: </b>",
          format(round(as.integer(map$Count), 0), nsmall = 1, big.mark = ","),
          "</h5>",
          "<h5><b>Percentage of isolates susceptible: </b>",
          format(round(as.numeric(map$propS * 100), 0)),
          "%</h5>",
          "<h5><b>Percentage of isolates intermediate: </b>",
          format(round(as.numeric(map$propI * 100), 0)),
          "%</h5>",
          "<h5><b>Percentage of isolates resistant: </b>",
          format(round(as.numeric(map$propR * 100), 0)),
          "%</h5>",
          "</div>"
        )
      }

      # Determine which column to use for coloring
      colorColumn <- if (!is.null(input$metric) && input$metric == "% Organism Prevalence") {
        "propPrevalence"
      } else {
        "propS"
      }
      
      map_output <- leaflet(map) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          data = map1,
          fillColor = ~ color_pal(get(colorColumn)),
          weight = 0,
          fillOpacity = 0.8,
        )
      
      # Only add hatching layer if there are hatched regions
      if (nrow(map2.hatch) > 0) {
        map_output <- map_output %>%
          addPolylines(
            data = map2.hatch,
            color = ~ color_pal(colorValue),
            weight = 2,
          )
      }
      
      map_output <- map_output %>%
        addPolygons(
          fillColor = "transparent",
          color = "#777777",
          weight = 1.5,
          popup = popups
        )
      
      return(map_output)
    })

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    output$content <- renderUI({
      req(plotData())
      
      # Show the AST-less info banner even when plotData is empty
      # (it helps users understand WHY the map is empty)
      banner <- uiOutput(ns("astLessInfo"))
      
      if (
        !is.null(plotData()) &&
          nrow(plotData()) > 0 &&
          !all(is.na(plotData()$Region) | is.null(plotData()$Region))
      ) {
        tagList(
          banner,
          wellPanel(
            style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh;",
            div(
              style = "min-height: 750px",
              withSpinner(leafletOutput(ns("map"), height = "71vh"), type = 4, color = "#44CDC4")
            ),
            class = "contentWell"
          ),
          downloadButton(ns("save_image"), "Save Report", class = "plotSaveButton")
        )
      } else if (all(is.na(plotData()$Region) | is.null(plotData()$Region))) {
        tagList(
          banner,
          wellPanel(
            style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
            div(
              style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
              uiOutput(ns("errorHandlingNoLocation"))
            ),
            class = "contentWell"
          )
        )
      } else {
        tagList(
          banner,
          wellPanel(
            style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
            div(
              style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
              uiOutput(ns("errorHandling"))
            ),
            class = "contentWell"
          )
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

    output$errorHandlingNoLocation <- renderUI({
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
        icon("bacterium", style = "font-size:100px; color: #44CDC4"),
        h4("No location information found in your data."),
        h6("Please return to the 'Import' tab and ensure your dataset includes locations.")
      )
    })

    # Show helpful message when only culture data is available (no AST data)
    output$astLessInfo <- renderUI({
      # Check if the active data has AST information
      # AST data must have Antimicrobial and Interpretation columns with actual values
      data <- reactiveData()
      
      hasAst <- !is.null(data) && 
                nrow(data) > 0 && 
                "Antimicrobial" %in% names(data) && 
                "Interpretation" %in% names(data) &&
                any(!is.na(data$Antimicrobial)) &&
                any(!is.na(data$Interpretation))
      
      # Only show tip when there's no AST data
      if (!hasAst) {
        div(
          style = "background-color: #d1ecf1; border: 1px solid #bee5eb; color: #0c5460; 
                   padding: 15px; border-radius: 4px; margin-bottom: 15px;",
          icon("info-circle"),
          strong(" Tip: "),
          "No antimicrobial susceptibility data detected. Switch to ",
          strong("'% Organism Prevalence'"),
          " to analyze your culture data."
        )
      }
    })

    output$legend <- renderUI({
      req(input$metric)
      
      if (input$metric == "% Susceptible") {
        # Susceptibility legend
        wellPanel(
          h4("Legend", class = "legend-title"),
          h5("Color", class = "legend-section"),
          div(
            class = "legend-section",
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #E0F7E1"
              ),
              span("90+% Susceptible", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #71CDB8"
              ),
              span("80-89% Susceptible", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #009FB3"
              ),
              span("70-79% Susceptible", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #2E5F90"
              ),
              span("60-69% Susceptible", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #3E294E"
              ),
              span("50-59% Susceptible", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #070707"
              ),
              span("<50% Susceptible", class = "legend-label")
            )
          ),
          h5("Hatching", class = "legend-section"),
          div(
            class = "legend-section",
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-signal legendColorBlock",
                style = "font-size: 20px"
              ),
              span(
                "Regions with less than 30 observations are denoted with hatch marks.",
                class = "legend-label"
              )
            )
          ),
          class = "legendWell"
        )
      } else {
        # Prevalence legend
        wellPanel(
          h4("Legend", class = "legend-title"),
          h5("Color", class = "legend-section"),
          div(
            class = "legend-section",
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #E0F7E1"
              ),
              span("75+% Prevalence", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #71CDB8"
              ),
              span("50-74% Prevalence", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #009FB3"
              ),
              span("25-49% Prevalence", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #2E5F90"
              ),
              span("10-24% Prevalence", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #3E294E"
              ),
              span("5-9% Prevalence", class = "legend-label")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-square legendColorBlock",
                style = "color: #070707"
              ),
              span("<5% Prevalence", class = "legend-label")
            )
          ),
          h5("Hatching", class = "legend-section"),
          div(
            class = "legend-section",
            div(
              class = "legend-item",
              tags$i(
                class = "fa-solid fa-signal legendColorBlock",
                style = "font-size: 20px"
              ),
              span(
                "Regions with less than 30 observations are denoted with hatch marks.",
                class = "legend-label"
              )
            )
          ),
          class = "legendWell"
        )
      }
    })

    output$map <- renderLeaflet({
      map_reactive()
    })

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Download Handlers
    # ------------------------------------------------------------------------------

    output$save_image <- downloadHandler(
      filename = "Map.html",

      content = function(file) {
        withProgress(message = 'Rendering, please wait!', {
          src <- normalizePath("./Reports/Map.qmd")
          tmp <- tempdir()
          unlink(list.files(tmp, full.names = TRUE), recursive = TRUE, force = TRUE)

          owd <- setwd(tmp)
          on.exit({
            setwd(owd)
            unlink(c("filters.RDS", "map.html", "map.png", "Map.qmd"), recursive = TRUE)
          })

          file.copy(src, "Map.qmd", overwrite = TRUE)

          htmlwidgets::saveWidget(
            widget = map_reactive(),
            file = "map.html",
            selfcontained = TRUE
          )

          webshot2::webshot(
            url = "map.html",
            file = "map.png",
            vwidth = 1200,
            vheight = 800
          )

          saveRDS(activeFilters(), "filters.RDS")

          quarto::quarto_render(
            input = "Map.qmd",
            output_format = "html",
            output_file = "Map.html"
          )

          file.rename("Map.html", file)
        })
      }
    )

    # ------------------------------------------------------------------------------
    # Module return
    # ------------------------------------------------------------------------------
  })
}

map_tab <- list(
  ui = ui,
  server = server
)
