# Bootstrap script for AMR Data Visualizer
# Usage: source("https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/run_app.R")

message("Setting up AMR Data Visualizer...")

# Force pre-compiled binaries for CRAN and modern HTTPS downloads
options(
  download.file.method = "libcurl",
  pkgType = "binary",
  install.packages.compile.from.source = "never"
)

# Ensure jsonlite and remotes are available
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "https://cloud.r-project.org", type = "binary")
}
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org", type = "binary")
}

# Download lockfile from GitHub
lock_url <- "https://raw.githubusercontent.com/AMR-Visualizer/AMRDataVisualizer/main/renv.lock"
tmp_lock <- tempfile(fileext = ".lock")
download.file(lock_url, tmp_lock, quiet = TRUE)
lock <- jsonlite::fromJSON(tmp_lock)
unlink(tmp_lock)

# Separate missing packages into CRAN vs GitHub
installed <- rownames(installed.packages())
cran_missing <- c()
github_missing <- c()

for (pkg_name in names(lock$Packages)) {
  if (pkg_name %in% installed) next
 
  info <- lock$Packages[[pkg_name]]
 
  if (identical(info$Source, "GitHub")) {
    owner <- if (!is.null(info$RemoteUsername)) info$RemoteUsername else info$RemoteOwner
    repo  <- if (!is.null(info$RemoteRepo)) info$RemoteRepo else pkg_name
    github_missing <- c(github_missing, paste0(owner, "/", repo))
  } else {
    cran_missing <- c(cran_missing, pkg_name)
  }
}

# 1. Install missing CRAN packages as pre-compiled binaries (no Rtools needed)
if (length(cran_missing) > 0) {
  message("Installing ", length(cran_missing), " missing CRAN package(s)...")
  install.packages(cran_missing, repos = "https://cloud.r-project.org", type = "binary")
}

# 2. Install missing GitHub packages directly without running R CMD build (no Rtools needed)
if (length(github_missing) > 0) {
  message("Installing ", length(github_missing), " missing GitHub package(s)...")
  for (gh_repo in github_missing) {
    remotes::install_github(gh_repo, build = FALSE, upgrade = "never", quiet = TRUE)
  }
}

if (length(cran_missing) == 0 && length(github_missing) == 0) {
  message("All required packages are already installed.")
}

# Launch the app
message("Launching app...")
shiny::runGitHub(
  repo = "AMRDataVisualizer",
  username = "AMR-Visualizer",
  ref = "main"
)
