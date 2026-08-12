# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# 1. Install renv if it isn't already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# 2. Download the lockfile to a temporary file
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)

# 3. Restore all packages (CRAN & GitHub) into the active R library
message("Checking and installing dependencies from renv.lock...")
renv::restore(
  lockfile = tmp_lock,
  library = .libPaths()[1],
  prompt = FALSE
)

# Clean up temp file
unlink(tmp_lock)

# 4. Launch the app
message("Launching app...")
shiny::runGitHub("AMR-Visualizer/AMRDataVisualizer")