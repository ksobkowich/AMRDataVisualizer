# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# Disable staged installs and unused package cleanup for Windows compatibility
options(
  renv.config.install.staged = FALSE, # Ensures build dependencies (like tidyselect) are visible immediately
  renv.config.clean.unused = FALSE    # Prevents Windows "Access is denied" file-locking errors
)

# 1. Ensure renv is installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# 2. Download lockfile
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)

# 3. Restore dependencies directly to the active R library
message("Checking and installing dependencies (this may take a few minutes on a fresh install)...")
renv::restore(
  lockfile = tmp_lock,
  library = .libPaths()[1],
  prompt = FALSE,
  clean = FALSE
)

# Clean up temporary lockfile
unlink(tmp_lock)

# 4. Launch the app
message("Launching app...")
shiny::runGitHub("AMR-Visualizer/AMRDataVisualizer")