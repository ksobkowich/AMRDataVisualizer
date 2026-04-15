# ------------------------------------------------------------------------------
# Global initialization script
# Author: Kurtis Sobkowich
# Description: Initializes global environment (i.e., libraries, functions, etc.)
# ------------------------------------------------------------------------------

# Shiny and related
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(fresh)

# Data + data manipulation
library(AMR)
library(dplyr)
library(stringr)
library(stringdist)
library(tidyr)
library(purrr)
library(jsonlite)
library(sf)
library(tidyverse)
library(zoo)

# File reading/upload/download
library(zip)
library(openxlsx)
library(chromote)
library(nanoparquet)
library(quarto)
library(yaml)
library(readxl)
library(writexl)
library(webshot2)
library(vroom)
library(arrow)

# Visualization - Tables
library(data.table)
library(DT)
library(gt)

# Visualization - Mapping
library(leaflet)
library(mapview)
library(tigris)
library(sp)

# Visualization - Plots
library(plotly)
library(ggplot2)
library(ggpattern)

# Visualization - Other
library(scales)
library(colorspace)

# Performance
library(foreach)
library(doParallel)

# Other
library(renv)
library(lubridate)
library(spacyr)
library(typedjs)

# For renv setup see the "Managing Dependencies with `renv`" section in `Documentation/quick_start_guide.md`

# ------------------------------------------------------------------------------
# Source Tabs
# ------------------------------------------------------------------------------
source("source/tabs/home.R")
source("source/tabs/import/import.R")
source("source/tabs/mic_table.R")
source("source/tabs/mic_distribution.R")
source("source/tabs/overview.R")
source("source/tabs/antibiogram.R")
source("source/tabs/map/map.R")
source("source/tabs/trends.R")
source("source/tabs/mdr.R")
source("source/tabs/explore.R")
source("source/tabs/about.R")


# ------------------------------------------------------------------------------
# Source Modules (Shiny modules)
# ------------------------------------------------------------------------------
source("source/modules/filter_panel.R")
source("source/modules/change_log.R")


# ------------------------------------------------------------------------------
# Source Components (Non-Shiny modules)
# ------------------------------------------------------------------------------
source("source/components/ab_table.R")
source("source/components/mic_dist_plot.R")
source("source/components/mic_freq_table.R")

# ------------------------------------------------------------------------------
# Source Utilities
# ------------------------------------------------------------------------------
source("source/utils/utils.R")
source("source/utils/ui_utils.R")
source("source/utils/data_utils.R")
source("source/utils/plot_utils.R")
source("source/utils/report_utils.R")
source("source/utils/column_detection.R")


source("source/utils/version.R")
#add version info to docs
# Debug: Check if variables exist
cat("=== DEBUG INFO ===\n")
cat("APP_VERSION exists:", exists("APP_VERSION"), "\n")
cat("APP_RELEASE_DATE exists:", exists("APP_RELEASE_DATE"), "\n")

if (exists("APP_VERSION")) {
  cat("APP_VERSION value:", APP_VERSION, "\n")
} else {
  cat("APP_VERSION is missing!\n")
}

if (exists("APP_RELEASE_DATE")) {
  cat("APP_RELEASE_DATE value:", APP_RELEASE_DATE, "\n")
} else {
  cat("APP_RELEASE_DATE is missing!\n")
}

# Check if file exists
cat("about.md file exists:", file.exists("Documentation/about.md"), "\n")

# Preprocess about.md file with version info
if (file.exists("Documentation/about.md")) {
  cat("Processing about.md...\n")
  
  about_text <- readLines("Documentation/about.md", warn = FALSE)
  about_text <- paste(about_text, collapse = "\n")

  # Add this after reading the file:
# Debug: Check if placeholders exist in the text
cat("Text contains {{VERSION}}:", grepl("{{VERSION}}", about_text, fixed = TRUE), "\n")
cat("Text contains {{RELEASE_DATE}}:", grepl("{{RELEASE_DATE}}", about_text, fixed = TRUE), "\n")
cat("Text contains {{ACCESS_DATE}}:", grepl("{{ACCESS_DATE}}", about_text, fixed = TRUE), "\n")

# Show the citation section specifically
citation_section <- regmatches(about_text, regexpr("Sobkowich.*?Accessed.*", about_text))
if(length(citation_section) > 0) {
  cat("Citation section found:", citation_section, "\n")
} else {
  cat("Citation section NOT found\n")
}
  
  # Show original text (first 200 chars)
  cat("Original text snippet:", substr(about_text, 1, 200), "...\n")
  
  # Replace placeholders with debug info
  version_val <- if(exists("APP_VERSION")) APP_VERSION else "VERSION_MISSING"
  release_val <- if(exists("APP_RELEASE_DATE")) APP_RELEASE_DATE else "RELEASE_DATE_MISSING"
  access_val <- format(Sys.Date(), "%B %d, %Y")
  
  about_text <- gsub("{{VERSION}}", version_val, about_text, fixed = TRUE)
  about_text <- gsub("{{RELEASE_DATE}}", release_val, about_text, fixed = TRUE)
  about_text <- gsub("{{ACCESS_DATE}}", access_val, about_text, fixed = TRUE)
  
  # Show processed text (first 200 chars)
  cat("Processed text snippet:", substr(about_text, 1, 200), "...\n")
  
  # Write processed version
  writeLines(about_text, "Documentation/about_processed.md")
  cat("Written to about_processed.md\n")
} else {
  cat("about.md file not found!\n")
}

cat("=== END DEBUG ===\n")


# Tab-specific utils
source("source/tabs/import/utils.R")
source("source/tabs/map/utils.R")


# ------------------------------------------------------------------------------
# Source external data
# ------------------------------------------------------------------------------
awareList <- read.csv("./Data/2023AwareClassifications.csv")

# ------------------------------------------------------------------------------
# Define initial variables
# ------------------------------------------------------------------------------
#' Possible metadata columns that may appear in data.
#' Want to ignore these when trying the find antimicrobial columns.
#' !! These need to be in lower case !!
g_metadataCols <- c(
  "organism",
  "year",
  "beta_lac",
  "host_species",
  "sex",
  "breed",
  "age",
  "specimen",
  "source_group",
  "source",
  "organism_long",
  "host_species_long",
  "comment",
  "species",
  "row_id",
  "date",
  "day",
  "order",
  "country",
  "month",
  "state",
  "id",
  "county"
)

# Test mapping values
g_sir_keywords <- c("sir", "value", "interpretation", "resistance", "status", "result", "interp")
g_mic_keywords <- c("mic")
g_mic_sign_keywords <- c("mic sign", "sign")
g_mic_value_keywords <- c("mic value", "value", "mic", "concentration", "val")

g_test_mapping <- list(
  "mic_sign" = g_mic_sign_keywords,
  "mic_value" = g_mic_value_keywords,
  "mic" = g_mic_keywords,
  "interp" = g_sir_keywords
)


# Full df of AMR clinical breakpoints with ab and mo names added
g_fullClinicalBreakpoints <- getFullClinicalBps()

# ------------------------------------------------------------------------------
# Increase maximum allowable file upload
# ------------------------------------------------------------------------------
options(shiny.maxRequestSize = 1000 * 1024^2)





# ------------------------------------------------------------------------------
# ---- Headless Chrome initializer ----
# ------------------------------------------------------------------------------
init_headless_chrome <- local({
  .done <- FALSE
  function() {
    if (.done) return(invisible())
    .done <<- TRUE

    # Required in restricted containers (Connect Cloud)
    chromote::set_chrome_args(c(
      "--no-sandbox",             # bypass userns sandbox in containers
      "--disable-dev-shm-usage",  # avoid small /dev/shm crashes
      "--disable-gpu",            # standard for headless on some hosts
      c("--force-color-profile","srgb"),
      "--disable-extensions",
      "--mute-audio"
    ))

    # If new headless causes trouble in your environment, force the old mode
    options(chromote.headless = "old")  # or Sys.setenv(CHROMOTE_HEADLESS = "old")

    # Optional diagnostics: print the exact Chrome command to your logs
    options(chromote.launch.echo_cmd = TRUE)

    # Optional: longer launch timeout on slower cold starts
    options(chromote.timeout = 20)

    # Try to log Chrome/chromote info (non-fatal if it fails)
    try({
      info <- chromote::chromote_info()
      message("Chromote info: ", paste(capture.output(str(info)), collapse = " "))
    }, silent = TRUE)
  }
})
