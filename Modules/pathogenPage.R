pathogenPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h5("Hello, Pathogen Page")
    
  )
  
}

pathogenPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Server goes here
    
  })
}
