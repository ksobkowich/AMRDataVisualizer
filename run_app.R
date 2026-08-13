# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# 1. Ensure pak and jsonlite are available
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "https://cloud.r-project.org")
}

# 2. Download lockfile
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)
lock <- jsonlite::fromJSON(tmp_lock)
unlink(tmp_lock)

# 3. Identify missing packages and format GitHub/CRAN references
installed <- rownames(installed.packages())
pkg_refs <- c()

for (pkg_name in names(lock$Packages)) {
  # Skip packages that are already installed
  if (pkg_name %in% installed) next
 
  info <- lock$Packages[[pkg_name]]
 
  if (identical(info$Source, "GitHub")) {
    # Format GitHub repo as "username/repo"
    ref <- paste0(info$RemoteUsername, "/", info$RemoteRepo)
    pkg_refs <- c(pkg_refs, ref)
  } else {
    # CRAN package
    pkg_refs <- c(pkg_refs, pkg_name)
  }
}

# 4. Install missing packages using pak
if (length(pkg_refs) > 0) {
  message("Installing ", length(pkg_refs), " missing package(s) using pre-compiled binaries...")
  pak::pkg_install(pkg_refs, ask = FALSE)
} else {
  message("All required packages are already installed.")
}

# 5. Launch the app
message("Launching app...")
shiny::runGitHub(
  repo = "AMRDataVisualizer",
  username = "AMR-Visualizer",
  ref = "main"
)