ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML(sprintf("
      #%s, #%s p, #%s li {
        color: #4a4a4a;
      }
    ", ns("about-container"), ns("about-container"), ns("about-container")))),
    
    tags$div(
      id = ns("about-container"),
      includeMarkdown("Documentation/about_processed.md")
    )
  )
}


server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}

about_tab <- list(
  ui = ui,
  server = server
)


