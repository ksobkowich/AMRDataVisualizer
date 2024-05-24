explorePageUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h5("Hello, Explore Page")
    
  )
  
}

explorePageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Server goes here
    
  })
}
