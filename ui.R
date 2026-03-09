# ------------------------------------------------------------------------------
# Main UI script
# Author: Kurtis Sobkowich
# Description: Main UI layout for the AMR Visualizer.
# ------------------------------------------------------------------------------

dashboardPage(
  # ------------------------------------------------------------------------------
  # Header
  # ------------------------------------------------------------------------------
  dashboardHeader(
    title = "", # No visible title in the top navbar
    tags$li(
      class = "dropdown",
      actionButton("info", icon("circle-info"), class = "info") # Info/help button
    )
  ),

  # ------------------------------------------------------------------------------
  # Sidebar
  # ------------------------------------------------------------------------------
  dashboardSidebar(
    # Logo
    img(
      src = "img/logo.png",
      height = "100px",
      style = "margin-left: 50px"
    ),
    hr(style = "border-color:#a7b6d4; margin:20px"),

    # Navigation Menu
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "homeTab", icon = icon("house", class = "nav-icon")),
      menuItem("Import", tabName = "importTab", icon = icon("file-import", class = "nav-icon")),
      style = "margin-top:25px"
    ),

    # Dynamically generated menu (e.g., based on presence of data)
    uiOutput("menu"),
    
    tags$div(
      sidebarMenu(
        menuItem("About", tabName = "aboutTab", icon = icon("circle-question", class = "nav-icon"))
      ),
      style = "margin-top: auto;"
    )
  ),

  # ---------------------------------------------------------------------------
  # Main body
  # ---------------------------------------------------------------------------
  dashboardBody(
    # Load custom styling and scripts
    includeCSS("www/css/styles.css"),
    includeScript("www/js/script.js"),

    # Set global font using `bslib` and `thematic`
    use_googlefont("Carme"),
    use_theme(create_theme(
      theme = "default",
      bs_vars_font(family_sans_serif = "Carme")
    )),

    # Enable JavaScript extensions
    useShinyjs(),

    # Define custom JS functions to hide/show the header
    extendShinyjs(
      text = "
        shinyjs.hideHeader = function() { $('.navbar').hide(); };
        shinyjs.showHeader = function() { $('.navbar').show(); };
      ",
      functions = c("hideHeader", "showHeader")
    ),

    # Initial header visibility (hidden by default)
    tags$style(HTML(".navbar { display: none; }")),

    # -------------------------------------------------------------------------
    # Tab menu items
    # -------------------------------------------------------------------------
    tabItems(
      tabItem(tabName = "homeTab", home_tab$ui("home")),
      tabItem(tabName = "importTab", import_tab$ui("dataImport")),
      tabItem(tabName = "ovTab", overview_tab$ui("overviewModule")),
      tabItem(tabName = "micTab", mic_table_tab$ui("micModule")),
      tabItem(tabName = "micDistTab", mic_distribution_tab$ui("micDistModule")),
      tabItem(tabName = "abTab", antibiogram_tab$ui("antibiogramModule")),
      tabItem(tabName = "mapTab", map_tab$ui("mapModule")),
      tabItem(tabName = "trendsTab", trends_tab$ui("tsModule")),
      tabItem(tabName = "mdrTab", mdr_tab$ui("mdrModule")),
      tabItem(tabName = "exploreTab", explore_tab$ui("exModule")),
      tabItem(tabName = "aboutTab", about_tab$ui("aboutModule"))
    ),
    
    
    # Fixed footer
    tags$footer(
      class = "main-footer",
      tags$div(
        class = "footer-logos",
        img(src = "img/cornell-reduced-wordmark-red.png", alt = "Cornell University"),
        img(src = "img/OVC__GUELPH_FULLCOLOUR_WHITEBG_V1_1.5IN_H_300PPI.png", alt = "University of Guelph"), 
        img(src = "img/epi-logo.png", alt = "epi"),
        img(src = "img/usda.png", alt = "USDA")
      ),
      tags$p(
        class = "footer-text",
        "© 2025 AMR Visualizer | The AMR Visualizer was supported by the U.S. Department of Agriculture's Animal and Plant Health Inspection Service (APHIS). This work does not necessarily represent the views of the U.S. Department of Agriculture’s Animal and Plant Health Inspection Service (APHIS)."
      )
    )
    
  )

  # ---------------------------------------------------------------------------
  # End of main UI
  # ---------------------------------------------------------------------------
)
