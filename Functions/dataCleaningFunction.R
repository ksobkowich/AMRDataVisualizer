library(foreach)
library(doParallel)
library(dplyr)
library(stringr)
library(AMR)

dataCleaner <- function(rawData, additionalCols = NULL) {
  
  # Precompute the mappings for antimicrobial and microorganism names and classes
  ab_name_data <- unique(rawData$Antimicrobial) %>%
    data.frame(Antimicrobial = ., ab_name = ab_name(.))  # Create a mapping for ab_name
  
  mo_name_data <- unique(rawData$Microorganism) %>%
    data.frame(Microorganism = ., mo_name = mo_name(.))  # Create a mapping for mo_name
  
  ab_class_data <- unique(rawData$Antimicrobial) %>%
    data.frame(Antimicrobial = ., ab_class = ab_group(.))  # Create a mapping for ab_group
  
  # Join these precomputed mappings to the rawData
  rawData <- rawData %>%
    left_join(ab_name_data, by = "Antimicrobial") %>%
    left_join(mo_name_data, by = "Microorganism") %>%
    left_join(ab_class_data, by = "Antimicrobial")
  
  # Optional additional columns
  if (!is.null(additionalCols)) {
    additionalCols <- as.character(additionalCols)
    additionalCols <- additionalCols[additionalCols %in% names(rawData)]
  }
  
  interpretation_map <- list(
    "S" = c("S", "SUS", "SUSC", "SUSCEPTIBLE", "SDD", "SD"),
    "I" = c("I", "INT", "INTERMEDIATE"),
    "R" = c("R", "RES", "RESISTANT")
  )
  
  normalize_interpretation <- function(value) {
    value <- toupper(str_trim(as.character(value)))  # Convert to uppercase & remove spaces
    
    for (key in names(interpretation_map)) {
      if (value %in% interpretation_map[[key]]) {
        return(key)
      }
    }
    
    for (key in names(interpretation_map)) {
      if (any(agrepl(value, interpretation_map[[key]], ignore.case = TRUE, max.distance = 0.2))) {
        return(key)
      }
    }
    
    return(NA) 
  }
  
  numCores <- detectCores() - 1
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Parallel processing with foreach
  cleanData <- foreach(chunk = split(rawData, cut(seq(nrow(rawData)), numCores, labels = FALSE)), 
                       .combine = bind_rows, .packages = c("dplyr", "stringr", "AMR")) %dopar% {
                         
                         additionalColsData <- if (!is.null(additionalCols)) {
                           chunk %>% select(all_of(additionalCols))
                         } else {
                           NULL
                         }
                         
                         # Replace microorganism and antimicrobial columns with precomputed values
                         cleanedChunk <- chunk %>%
                           mutate(
                             ID = as.character(ID),
                             Year = if ("Year" %in% names(.)) as.integer(Year) else NA_integer_,
                             
                             Month = if ("Month" %in% names(.)) {
                               ifelse(is.na(Month), 1, as.integer(Month))
                             } else {
                               1
                             },
                             
                             Date = if ("Date" %in% names(.)) {
                               lubridate::parse_date_time(
                                 Date,
                                 orders = c(
                                   "ymd", "mdy", "dmy",
                                   "m/d/y", "d/m/y",
                                   "B d, Y", "d B Y", "d-b-Y", "d-b-y"
                                 )
                               ) %>% as.Date()
                             } else {
                               as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")
                             },
                             
                             Region = str_to_title(Region),
                             Subregion = str_to_sentence(Subregion),
                             Species = str_to_sentence(Species),
                             Source = str_to_sentence(Source),
                             
                             # Replace antimicrobial and microorganism with precomputed names
                             Microorganism = mo_name,  # Use the precomputed mo_name
                             Antimicrobial = ab_name,  # Use the precomputed ab_name
                             Class = ab_class,         # Use the precomputed ab_class
                             
                             Interpretation = sapply(Interpretation, normalize_interpretation)
                           ) %>%
                           select(
                             ID, Date, Region, Subregion, Species, Source, 
                             Microorganism, Antimicrobial, Class, Interpretation
                           )
                         
                         # Bind additional columns if any
                         if (!is.null(additionalColsData)) {
                           cleanedChunk <- bind_cols(cleanedChunk, additionalColsData)
                         }
                         
                         # Filter based on Interpretation
                         cleanedChunk <- cleanedChunk %>%
                           filter(Interpretation %in% c("S", "R", "I", 0, 1)) %>%
                           mutate(Interpretation = ifelse(Interpretation == 0, "S", 
                                                          ifelse(Interpretation == 1, "R", Interpretation)))
                         
                         return(cleanedChunk)
                       }
  
  stopCluster(cl)
  
  return(cleanData)
}
