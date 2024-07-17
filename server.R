
server <- function(input, output, session) {
  
  observe({
    req(input$tabs)
    if (input$tabs == "homeTab") {
      js$hideHeader()
    } else {
      js$showHeader()
    }
  })
  
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
        menuItem("MicroGuide", tabName = "pathogenTab", icon = icon("bacteria", class = "nav-icon")),
        menuItem("MDR", tabName = "mdrTab", icon = icon("pills", class = "nav-icon")),
        menuItem("Explore", tabName = "exploreTab", icon = icon("table-list", class = "nav-icon"))
      )
    } else {
      tagList(
        sidebarMenu(id = "tabs"),
        h6(em("Please import or select a data source to access additional tabs."), style = "color: #a7b6d4; margin:25px; text-align: center;")
      )
    }
  })
  
  observeEvent(input$info, {
    showModal(modalDialog(
      title = div(
        style = "text-align: center;",
        tags$img(src = "logoDark.png", height = "100px", style = "vertical-align: middle;")
      ),
      h4("How to use this app"),
      
      HTML('<iframe width="100%" height = "300" src="https://www.youtube.com/embed/pd7jODRf4os" frameborder="0" allowfullscreen></iframe>'),
      easyClose = T
    ))
  })
  
  
  
  
  # Overview Page -----------------------------------------------------------
  observe({
    req(clean())
    ovPageServer("overviewModule", clean())
  })
  
  
  # Antibiogram Page --------------------------------------------------------
  output$antibiogramUI <- renderUI({
    req(clean())
    abPageUI("antibiogramModule", clean())
  })
  
  observe({
    req(clean())
    abPageServer("antibiogramModule", clean())
  })
  
  # Map Page ----------------------------------------------------------------
  output$mapUI <- renderUI({
    req(clean())
    mapPageUI("mapModule", clean())
  })
  
  observe({
    req(clean())
    mapPageServer("mapModule", clean())
  })
  
  
  # Time Series Page --------------------------------------------------------
  output$tsUI <- renderUI({
    req(clean())
    tsPageUI("tsModule", clean())
  })
  
  observe({
    req(clean())
    tsPageServer("tsModule", clean())
  })
  
  
  # Pathogen Page -----------------------------------------------------------
  output$pathogenUI <- renderUI({
    req(clean())
    pathogenPageUI("pathogenModule", clean())
  })
  
  observe({
    req(clean())
    pathogenPageServer("pathogenModule", clean())
  })
  
  # MDR Page --------------------------------------------------------
  output$mdrUI <- renderUI({
    req(clean())
    mdrPageUI("mdrModule", clean())
  })
  
  observe({
    req(clean())
    mdrPageServer("mdrModule", clean())
  })
  
  
  # Explore Page ------------------------------------------------------------
  
  output$exploreUI <- renderUI({
    req(clean())
    explorePageUI("exModule", clean())
  })
  
  observe({
    req(clean())
    explorePageServer("exModule", clean())
  })
  
  
  # Close server ------------------------------------------------------------
}