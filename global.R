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
library(foreach)
library(doParallel)
library(spacyr)
library(shinyBS)
library(jsonlite)
library(leaflet)
library(sf)
library(tigris)
library(stringdist)
library(colorspace)
library(HatchedPolygons)
library(tidyverse)
library(plotly)
library(zoo)
library(renv)
library(mapview)
library(nanoparquet)
library(shinycssloaders)
library(quarto)
library(webshot2)
library(chromote)

# renv::init()

# Run this line and select option 2 for dockerizing.
# renv::snapshot()

# renv::deactivate()

# Source Files ------------------------------------------------------------
#Modules
source("Modules/homePage.R")
source("Modules/ovPage.R")
source("Modules/abPage.R")
source("Modules/mapPage.R")
source("Modules/tsPage.R")
source("Modules/mdrPage.R")
source("Modules/pathogenPage.R")
source("Modules/explorePage.R")
source("Modules/importDataModule.R")
source("Modules/filterPanelModule.R")

#Functions
source("Functions/dataCleaningFunction.R")
source("Functions/columnDetectFunctions.R")
source("Functions/regionMatching.R")
source("Functions/parseWideColumns.R")

#Data
awareList <- read.csv("./Data/2023AwareClassifications.csv")

# Increase maximum data size ----------------------------------------------
options(shiny.maxRequestSize = 1000 * 1024^2)
