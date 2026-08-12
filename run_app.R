# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# Force modern HTTPS download handling and disable staged installs
options(
  download.file.method = "libcurl",
  renv.config.install.staged = FALSE,
  renv.config.clean.unused = FALSE
)

# 1. Ensure renv is installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# 2. Download lockfile
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)

# 3. Restore dependencies directly to active R library
message("Checking and installing dependencies...")
renv::restore(
  lockfile = tmp_lock,
  library = .libPaths()[1],
  prompt = FALSE,
  clean = FALSE
)

# Clean up temporary lockfile
unlink(tmp_lock)

# 4. Launch the app using explicit repository details and branch ref
message("Launching app...")
shiny::runGitHub(
  repo = "AMRDataVisualizer",
  username = "AMR-Visualizer",
  ref = "main"
)