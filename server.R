# ------------------------------------------------------------------------------
# Main server script
# Author: Kurtis Sobkowich
# Description: Defines the server-side logic for the AMR Visualizer app
#              Coordinates multiple external modules to handle user input,
#              reactive processing, and dynamic outputs.
# ------------------------------------------------------------------------------

#' Main server function for the Shiny application.
#'
#' @param input   Shiny input object.
#' @param output  Shiny output object.
#' @param session Shiny session object.
#' @return        None. This function defines the server-side logic for the Shiny app.
server <- function(input, output, session) {
  # ------------------------------------------------------------------------------
  # Sub-modules
  # ------------------------------------------------------------------------------

  home_tab$server("home")
  importResults <- import_tab$server("dataImport") # Gather cleaned data from `importDataModule.R`
  micData <- mic_table_tab$server(
    "micModule",
    reactiveData = clean,
    processedGuideline = processedGuideline,
    bp_log = importResults$bp_log
  )
  mic_distribution_tab$server("micDistModule", reactiveData = dataWithCustomBreakpoints)
  antibiogram_tab$server(
    "antibiogramModule",
    reactiveData = dataWithCustomBreakpoints,
    customBreakpoints = customBreakpoints,
    mic_or_sir = importResults$mic_or_sir,
    bp_log = importResults$bp_log
  )
  map_tab$server("mapModule", reactiveData = primaryData)
  trends_tab$server("tsModule", reactiveData = primaryData, allCulturesData = cleanAllCultures)
  # MicroGuide
  mdr_tab$server("mdrModule", reactiveData = dataWithCustomBreakpoints)
  explore_tab$server("exModule", reactiveData = dataWithCustomBreakpoints)

  # ------------------------------------------------------------------------------
  # Module variables
  # ------------------------------------------------------------------------------

  # Variables from import tab
  clean <- importResults$data
  cleanAllCultures <- importResults$dataAllCultures
  processedGuideline <- importResults$guideline

  # Variables from MIC tab
  dataWithCustomBreakpoints <- micData$dataWithCustomBreakpoints
  customBreakpoints <- micData$customBreakpoints





  # ------------------------------------------------------------------------------
  # Reactives
  # ------------------------------------------------------------------------------

  # Check for cleaned data returned from `importDataModule.R`
  isDataPresent <- reactive({
    if (is.null(clean())) {
      return(FALSE)
    }
    !is.null(clean()) && nrow(clean()) > 0
  })

  # Does the user have AST (susceptibility test) data?
  # This is TRUE when the cleaned data has at least one row with antimicrobial
  # test results. When FALSE, tabs that require AST data (MIC tables, antibiogram,
  # MDR, etc.) should be disabled.
  hasAstData <- reactive({
    !is.null(clean()) && nrow(clean()) > 0
  })

  # Does the user have any usable data (AST or all-cultures)?
  # This is TRUE when either AST data OR all-cultures data is available.
  # Used to decide whether to show any menu items at all.
  hasAnyData <- reactive({
    hasAstData() ||
      (!is.null(cleanAllCultures()) && nrow(cleanAllCultures()) > 0)
  })

  # Primary data reactive: returns AST-with-custom-breakpoints when available,
  # otherwise falls back to the all-cultures data. This is the data source that
  # flows into tabs that support both AST and prevalence-only workflows
  # (Overview, Trends, and eventually Map).
  primaryData <- reactive({
    if (hasAstData()) {
      dataWithCustomBreakpoints()
    } else {
      cleanAllCultures()
    }
  })


  # ------------------------------------------------------------------------------
  # Render UI
  # ------------------------------------------------------------------------------

  # If cleaned data exist, render full sidebar menu.
  # Selectively show sidebar menu if data is present.
  output$menu <- renderUI({
    if (hasAstData()) {
      # --- Full menu: AST data is present ---------------------------------------
      menu_items <- list(
        menuItem(
          "Overview",
          tabName = "ovTab",
          icon = icon("magnifying-glass-chart", class = "nav-icon")
        ),
        menuItem("Antibiogram", tabName = "abTab", icon = icon("braille", class = "nav-icon")),
        menuItem("Map", tabName = "mapTab", icon = icon("map-location-dot", class = "nav-icon")),
        menuItem("Trends", tabName = "trendsTab", icon = icon("chart-line", class = "nav-icon")),
        menuItem("MDR", tabName = "mdrTab", icon = icon("pills", class = "nav-icon")),
        menuItem("Explore", tabName = "exploreTab", icon = icon("table-list", class = "nav-icon"))
      )

      # Show "MIC Tables" tab if MIC data were imported
      if ("MIC" %in% names(clean())) {
        mic_item <- menuItem(
          "MIC Tables",
          tabName = "micTab",
          icon = icon("vial", class = "nav-icon")
        )
        mic_dist_item <- menuItem(
          "MIC Distributions",
          tabName = "micDistTab",
          icon = icon("chart-simple", class = "nav-icon")
        )
        menu_items <- c(list(mic_item, mic_dist_item), menu_items)
      }

      sidebarMenu(id = "tabs", menu_items)

    } else if (hasAnyData()) {
      # --- Reduced menu: only all-cultures data available ------------------------
      # AST-dependent tabs (Antibiogram, MDR, Explore, MIC tables) are hidden
      # because they can't do anything meaningful without susceptibility results.
      sidebarMenu(
        id = "tabs",
        menuItem(
          "Overview",
          tabName = "ovTab",
          icon = icon("magnifying-glass-chart", class = "nav-icon")
        ),
        menuItem(
          "Map",
          tabName = "mapTab",
          icon = icon("map-location-dot", class = "nav-icon")
        ),
        menuItem(
          "Trends",
          tabName = "trendsTab",
          icon = icon("chart-line", class = "nav-icon")
        )
      )

    } else {
      # --- No data yet -----------------------------------------------------------
      tagList(
        sidebarMenu(id = "tabs"),
        h6(
          em("Please import or select a data source to access additional tabs."),
          style = "color: #a7b6d4; margin:25px; text-align: center;"
        )
      )
    }
  })
  # ------------------------------------------------------------------------------
  # Observes
  # ------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------
  # Selectively hide header bar if tab = "Home"
  # ------------------------------------------------------------------------------
  observe({
    req(input$tabs)
    if (input$tabs == "homeTab") {
      js$hideHeader()
    } else {
      js$showHeader()
    }
  })

  # ------------------------------------------------------------------------------
  # Switch "i" (information) modal content based on current tab
  # ------------------------------------------------------------------------------
  observeEvent(input$info, {
    showModal(
      modalDialog(
        title = div(
          style = "text-align: center;",
          tags$img(
            src = "img/logoDark.png",
            height = "100px",
            style = "vertical-align: middle;"
          )
        ),

        switch(
          input$tabs,
          importTab = includeMarkdown("Documentation/data-import.md"),
          ovTab = includeMarkdown("Documentation/overview-plots.md"),
          micTab = includeMarkdown("Documentation/mic-tables.md"),
          #micDistTab = includeMarkdown("Documentation/mic-distributions.md"),
          abTab = includeMarkdown("Documentation/antibiograms.md"),
          mapTab = includeMarkdown("Documentation/maps.md"),
          trendsTab = includeMarkdown("Documentation/trends.md"),
          pathogenTab = includeMarkdown("Documentation/microguide.md"),
          mdrTab = includeMarkdown("Documentation/mdr-matrices.md"),
          exploreTab = includeMarkdown("Documentation/data-explore.md"),
          "Documentation Coming Soon." # Fallback message
        ),

        easyClose = TRUE,
        size = "l"
      )
    )
  })

  # ------------------------------------------------------------------------------
  # Initialize server functions for each tab module
  # ------------------------------------------------------------------------------

  # Overview tab
  observe({
    req(primaryData())
    overview_tab$server("overviewModule", primaryData())
  })

  # ------------------------------------------------------------------------------
  # End of main Server
  # ------------------------------------------------------------------------------
}
