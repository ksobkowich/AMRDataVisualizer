# Load Libraries ----------------------------------------------------------
library(AMR)
library(arrow)
library(data.table)
library(DT)
library(fresh)
library(ggpattern)
library(plotly)
library(scales)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(tools)
library(tidyverse)
library(vroom)
library(typedjs)
library(readxl)
library(stringr)


# Source Files ------------------------------------------------------------
source("./Modules/homePage.R")
source("./Modules/ovPage.R")
source("./Modules/abPage.R")
source("./modules/mapPage.R")
source("./modules/tsPage.R")
source("./Modules/mdrPage.R")
source("./Modules/pathogenPage.R")
source("./modules/explorePage.R")
source("./Modules/importDataModule.R")

source("./Functions/dataCleaningFunction.R")
source("./Functions/columnDetectFunctions.R")
source("./Functions/regionMatching.R")

# Increase maximum data size ----------------------------------------------

options(shiny.maxRequestSize = 1000 * 1024^2)
