# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# Ensure jsonlite is available (needed to read the lockfile)
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

# Read the lockfile directly from GitHub
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
lock <- jsonlite::fromJSON(lock_url)
pkgs <- names(lock$Packages)

# Identify missing packages
installed <- rownames(installed.packages())
missing <- setdiff(pkgs, installed)


# Install missing packages
options(install.packages.compile.from.source = "never")
if (length(missing) > 0) {
  message("Installing ", length(missing), " missing packages. This may take a while...")
  install.packages(missing)
} else {
  message("All required packages already installed.")
}

# Launch the app
message("Launching app...")
shiny::runGitHub("AMR-Visualizer/AMRDataVisualizer")