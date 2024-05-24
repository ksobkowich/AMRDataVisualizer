
server <- function(input, output, session) {

# Get Clean Data ----------------------------------------------------------------
  
  clean <- importDataServer("dataImport")
  
# Determine if full sidebar should be shown -------------------------------
  showMenu <- reactiveVal(FALSE)
  dataPresent <- reactive({
    !is.null(clean()) && nrow(clean()) > 0
  })
  observe({
    showMenu(dataPresent())
  })
  output$menu <- renderUI({
    if (dataPresent()) {
      sidebarMenu(
        id = "tabs",
      menuItem("Overview", tabName = "ovTab", icon = icon("chart-simple", class = "nav-icon")),
      menuItem("Antibiogram", tabName = "abTab", icon = icon("braille", class = "nav-icon")),
      menuItem("Map", tabName = "mapTab", icon = icon("map-location-dot", class = "nav-icon")),
      menuItem("Trends", tabName = "trendsTab", icon = icon("chart-line", class = "nav-icon")),
      menuItem("Explore", tabName = "exploreTab", icon = icon("table-list", class = "nav-icon"))
      )
    } else {
      tagList(
      sidebarMenu(id = "tabs"),
      h6(em("Please import or select a data source to access additional tabs."), style = "color: #a7b6d4; margin:25px; text-align: center;")
      )
    }
  })
  

# Overview Page -----------------------------------------------------------
  observe({
    req(clean())
    ovPageServer("overviewModule", clean())
  })
  
  output$antibiogramUI <- renderUI({
    req(clean())
    abPageUI("antibiogramModule", clean())
  })
  
  observe({
    req(clean())
    abPageServer("antibiogramModule", clean())
  })
# Close server ------------------------------------------------------------
}