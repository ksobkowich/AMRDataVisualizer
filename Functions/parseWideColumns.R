parseColSpec <- function(spec) {
  spec <- gsub("\\s+", "", spec)

  parts <- unlist(strsplit(spec, split = ","))
  
  all_cols <- numeric(0)
  
  for (part in parts) {

    if (grepl("-", part)) {
      rng <- strsplit(part, "-")[[1]]
      
      if (length(rng) == 2) {
        from <- as.numeric(rng[1])
        to   <- as.numeric(rng[2])

        if (!is.na(from) && !is.na(to)) {
          all_cols <- c(all_cols, seq(min(from, to), max(from, to)))
        }
      }
    } else {
      val <- as.numeric(part)
      if (!is.na(val)) {
        all_cols <- c(all_cols, val)
      }
    }
  }
  
  unique(all_cols)
}  