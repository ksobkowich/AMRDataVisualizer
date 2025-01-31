library(foreach)
library(doParallel)
library(dplyr)
library(stringr)
library(AMR)

dataCleaner <- function(rawData, additionalCols = NULL) {
  
  # Precompute the mappings for antimicrobial and microorganism names and classes
  ab_name_data <- unique(rawData$Antimicrobial) %>%
    data.frame(Antimicrobial = ., ab_name = ab_name(.))  # Create a mapping for ab_name
  
  ab_class_data <- unique(rawData$Antimicrobial) %>%
    data.frame(Antimicrobial = ., ab_class = ab_group(.))  # Create a mapping for ab_group
  
  mo_name_data <- unique(rawData$Microorganism) %>%
    data.frame(Microorganism = ., mo_name = mo_name(.))  # Create a mapping for mo_name
  
  mo_stain_data <- unique(rawData$Microorganism) %>%
    data.frame(Microorganism = ., mo_stain = mo_gramstain(.))  # Create a mapping for mo_stain
  
  # Join these precomputed mappings to the rawData
  rawData <- rawData %>%
    left_join(ab_name_data, by = "Antimicrobial") %>%
    left_join(ab_class_data, by = "Antimicrobial") %>%
    left_join(mo_name_data, by = "Microorganism") %>%
    left_join(mo_stain_data, by = "Microorganism")
  
  # Optional additional columns
  if (!is.null(additionalCols)) {
    additionalCols <- as.character(additionalCols)
    additionalCols <- additionalCols[additionalCols %in% names(rawData)]
  }
  
  numCores <- detectCores() - 1
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Parallel processing with foreach
  cleanData <- foreach(chunk = split(rawData, cut(seq(nrow(rawData)), numCores, labels = FALSE)),
                       .combine = bind_rows, .packages = c("dplyr", "stringr", "AMR")) %dopar% {
                         
                         # Optional additional columns
                         additionalColsData <- if (!is.null(additionalCols)) {
                           chunk %>% select(all_of(additionalCols))
                         } else {
                           NULL
                         }
                         
                         # Replace antimicrobial and microorganism columns with precomputed values
                         cleanedChunk <- chunk %>%
                           mutate(
                             ID = as.character(ID),
                             Year = as.integer(Year),
                             Month = ifelse(is.na(Month), 1, as.integer(Month)),
                             Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"),
                             Region = str_to_title(Region),
                             Subregion = str_to_sentence(Subregion),
                             Species = str_to_sentence(Species),
                             Source = str_to_sentence(Source),
                             # Replace antimicrobial and microorganism with precomputed names
                             Microorganism = mo_name,  # Use the precomputed mo_name
                             Stain = mo_stain,
                             Antimicrobial = ab_name,  # Use the precomputed ab_name
                             Class = ab_class         # Use the precomputed ab_class
                           ) %>%
                           select(
                             ID, Date, Region, Subregion, Species, Source,
                             Microorganism, Stain, Antimicrobial, Class, Interpretation
                           )
                         
                         # Bind additional columns if any
                         if (!is.null(additionalColsData)) {
                           cleanedChunk <- bind_cols(cleanedChunk, additionalColsData)
                         }
                         
                         # Filter based on Interpretation
                         cleanedChunk <- cleanedChunk %>%
                           filter(Interpretation %in% c("S", "R", "I"))
                         
                         return(cleanedChunk)
                       }
  
  stopCluster(cl)
  
  return(cleanData)
}
