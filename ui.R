
dashboardPage(
  
  # Header ------------------------------------------------------------------
  
  dashboardHeader(title = "", tags$li(class = "dropdown", actionButton("info", icon("circle-info"), class = "info"))),
  
  # Sidebar -----------------------------------------------------------------
  
  dashboardSidebar(
    img(src = "logo.png", height = "100px", style = "margin-left: 50px"),
    hr(style = "border-color:#a7b6d4; margin:20px"),
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "homeTab", icon = icon("house", class = "nav-icon")),
      menuItem("Import", tabName = "importTab", icon = icon("file-import", class = "nav-icon")),
      style = "margin-top:25px"
    ),
    uiOutput("menu")
  ),

  # Body --------------------------------------------------------------------
  
  dashboardBody(
    includeCSS("styles.css"),
    use_googlefont("Carme"),
    use_theme(create_theme(
      theme = "default",
      bs_vars_font(
        family_sans_serif = "Carme"
      )
    )),
    useShinyjs(),
    
    tabItems(
      
      tabItem("homeTab", homePageUI("home")),
      
      tabItem("importTab", importDataUI("dataImport")),
      
      tabItem("ovTab", ovPageUI("overviewModule")),
      
      tabItem("abTab", uiOutput("antibiogramUI")),
      
      tabItem("mapTab", uiOutput("mapUI")),
      
      tabItem("trendsTab", uiOutput("tsUI")),
      
      tabItem("pathogenTab", pathogenPageUI("pathogen")),
      
      tabItem("mdrTab", mdrPageUI("mdrn")),
      
      tabItem("exploreTab", uiOutput("exploreUI"))
      
    )
  )

# Close UI ----------------------------------------------------------------

)