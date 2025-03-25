# Function to check column names
detectColumnByName <- function(data, keywords) {
  for (col_name in names(data)) {
    if (any(grepl(paste(keywords, collapse = "|"), col_name, ignore.case = TRUE))) {
      return(col_name)
    }
  }
  return(NULL)
}

# Detect ID Column
detectIdColumn <- function(data) {
  keywords <- c("id", "identification", "patient", "number")
  detectColumnByName(data, keywords)
}

# Detect Date Column
detectDateColumn <- function(data) {
  keywords <- c("date", "dt", "dte")
  detectColumnByName(data, keywords)
}

# Detect Year Column
detectYearColumn <- function(data) {
  keywords <- c("year", "yr")
  detectColumnByName(data, keywords)
}

# Detect Month Column
detectMonthColumn <- function(data) {
  keywords <- c("month", "mon")
  detectColumnByName(data, keywords)
}

# Detect Region Column
detectRegionColumn <- function(data) {
  keywords <- c("region", "state", "province", "territory")
  detectColumnByName(data, keywords)
}

# Detect Subregion Column
detectSubregionColumn <- function(data) {
  keywords <- c("subregion", "county", "district", "area")
  detectColumnByName(data, keywords)
}

# Detect Species Column
detectSpeciesColumn <- function(data) {
  keywords <- c("species", "host")
  detectColumnByName(data, keywords)
}

# Detect Infection Site Column
detectSourceColumn <- function(data) {
  keywords <- c("sample source", "infection site", "site", "source", "location")
  detectColumnByName(data, keywords)
}

# Detect Microorganism Column
detectMoColumn <- function(data) {
  keywords <- c("microorganism", "bacteria", "genus", "organism", "pathogen", "org")
  detectColumnByName(data, keywords)
}

# Detect Drug Column
detectDrugColumn <- function(data) {
  keywords <- c("drug", "antibiotic", "antimicrobial")
  detectColumnByName(data, keywords)
}

# Detect SIR Column
detectSIRColumn <- function(data) {
  keywords <- c("sir", "value", "interpretation", "resistance", "status", "result")
  detectColumnByName(data, keywords)
}