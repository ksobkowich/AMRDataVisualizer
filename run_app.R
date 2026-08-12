# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

# Bootstrap script for AMR Data Visualizer
message("Setting up AMR Data Visualizer...")

# Install pak if missing (pak is a modern, fast package manager for R)
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cloud.r-project.org")
}

# Download the lockfile to a temporary location
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)

# pak reads the lockfile and handles CRAN vs GitHub automatically
message("Installing missing packages. This may take a moment...")
pak::lockfile_install(tmp_lock)

# Launch the app
message("Launching app...")
shiny::runGitHub("AMR-Visualizer/AMRDataVisualizer")