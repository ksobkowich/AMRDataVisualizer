mapPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h5("Hello, Map Page")
    
  )
  
}

mapPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Server goes here
    
  })
}
