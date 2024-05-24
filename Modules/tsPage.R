tsPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h5("Hello, Time Series Page")
    
  )
  
}

tsPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Server goes here
    
  })
}
