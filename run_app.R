# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# Ensure required setup tools are available
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite", repos = "https://cloud.r-project.org")

# Download lockfile
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)

# Read lockfile and find ONLY packages that aren't installed yet
lock <- jsonlite::fromJSON(tmp_lock)
required_pkgs <- names(lock$Packages)
installed_pkgs <- rownames(installed.packages())
missing_pkgs <- setdiff(required_pkgs, installed_pkgs)

# Only run restore if there are actually missing packages
if (length(missing_pkgs) > 0) {
  message("Installing ", length(missing_pkgs), " missing package(s)...")
  renv::restore(
    lockfile = tmp_lock,
    packages = missing_pkgs,
    library = .libPaths()[1],
    prompt = FALSE
  )
} else {
  message("All required packages are already installed.")
}

# Clean up temporary lockfile
unlink(tmp_lock)

# Launch the app
message("Launching app...")
shiny::runGitHub("AMR-Visualizer/AMRDataVisualizer")