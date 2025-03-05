homePageUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .homeContainer {
          width: calc(100vw - 229px);
          height: 100vh;
          margin: -15px;
          background-color: #fff;
          overflow: hidden;
          position: relative;
        }

        .homeHeader {
          width: 100%;
          height: 50%;
          position: relative;
          top: 0;
          left: 0;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          background-image: url('homeHeaderImg.jpg');
          background-size: cover;
          background-repeat: no-repeat;
          background-position: center;
          border-top-left-radius: 0;
          border-top-right-radius: 0;
          border-bottom-left-radius: 50% 20%;
          border-bottom-right-radius: 50% 20%;
        }

        .homeButtons {
          font-size: 30px;
          border-radius: 1000px;
          height: 7vw;
          width: 7vw;
          margin: 10px;
          color: #44CDC4;
          border-width: 5px;
          border-color: #44CDC4;
          position: relative;
          overflow: hidden;
        }

        .homeButtons::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: rgba(68, 205, 196, 0.5); /* Blue overlay with alpha */
          z-index: 1;
        }

        .homeButtons .fa {
          position: relative;
          z-index: 2;
        }
      "))
    ),
    div(class = "homeContainer",
        div(class = "homeHeader",
            img(src = "logoLight.png", width = "300px"),
            h3("Liberate your data into actionable insights.", style = "color: white; text-align: center; margin-top: 20px;"),
            fluidRow(
              actionButton(ns("howTo"), "How-to guide", class = "clearButton"),
              actionButton(ns("learn"), "About", class = "clearButton")
            )
        ),
        div(
          style = "margin-top: 20px; text-align: center;",
          actionButton(ns("getStarted"), "Get Started", class = "submitButton", style = "font-size: 24px"),
          hr(style = "width: 500px; border-color: #34435a"),
          br(),
          h3("What's inside?"),
          actionButton("antibiogram", "Antibiograms", class = "homeButtons", style = "background-image: url('antibiogram.jpg');"),
          actionButton("map", "Maps", class = "homeButtons", style = "background-image: url('map.jpg');"),
          actionButton("trends", "Trends", class = "homeButtons", style = "background-image: url('trends.jpg');"),
          actionButton("pathogens", "MicroGuide", class = "homeButtons", style = "background-image: url('homeHeaderImg.jpg');"),
          actionButton("mdr", "MDR", class = "homeButtons", style = "background-image: url('mdr.jpg');"),
          actionButton("summary", "Data Summary", class = "homeButtons", style = "background-image: url('summary.jpg');")
        )
    )
  )
}



homePageServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    
    
  })
}
