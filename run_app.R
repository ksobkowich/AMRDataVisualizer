# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# 1. Configure R and renv to use pak for binary installs and disable source compilation
options(
  renv.config.pak.enabled = TRUE,                 # Delegate installation to pak (binary-first)
  renv.config.install.staged = FALSE,
  install.packages.compile.from.source = "never", # Strictly forbid source compilation
  pkgType = "binary"                              # Prefer binaries for Windows
)

# 2. Ensure pak and renv are installed
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# 3. Download lockfile
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)

# 4. Restore dependencies using pak
message("Checking and installing dependencies (downloading binaries)...")
renv::restore(
  lockfile = tmp_lock,
  library = .libPaths()[1],
  prompt = FALSE,
  clean = FALSE
)

# Clean up temporary lockfile
unlink(tmp_lock)

# 5. Launch the app
message("Launching app...")
shiny::runGitHub(
  repo = "AMRDataVisualizer",
  username = "AMR-Visualizer",
  ref = "main"
)