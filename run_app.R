# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# 1. Force R to only use pre-compiled binaries and never compile from C/C++ source
options(
  download.file.method = "libcurl",
  pkgType = "binary",                             # Force binary downloads on Windows
  install.packages.compile.from.source = "never", # Prevent R from attempting 'make' build steps
  renv.config.install.staged = FALSE,
  renv.config.clean.unused = FALSE
)

# 2. Use Posit Package Manager for access to pre-compiled Windows binaries
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

# 3. Ensure renv is installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", type = "binary")
}

# 4. Download lockfile
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)

# 5. Restore dependencies using binary packages
message("Checking and installing dependencies (downloading pre-compiled binaries)...")
renv::restore(
  lockfile = tmp_lock,
  library = .libPaths()[1],
  prompt = FALSE,
  clean = FALSE
)

# Clean up temporary lockfile
unlink(tmp_lock)

# 6. Launch the app
message("Launching app...")
shiny::runGitHub(
  repo = "AMRDataVisualizer",
  username = "AMR-Visualizer",
  ref = "main"
)