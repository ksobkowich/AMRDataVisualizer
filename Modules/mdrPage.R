mdrPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h5("Hello, MDR Page")
    
  )
  
}

mdrPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Server goes here
    
  })
}
