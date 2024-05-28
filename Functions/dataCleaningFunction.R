library(AMR)

# Parallel Processing Libraries
library(dplyr)
library(stringr)
library(parallel)

dataCleaner <- function(rawData){
  
  numCores <- detectCores() - 1
  
  # Split the data into chunks
  chunks <- split(rawData, 1:numCores)
  
  # Original Function for Cleaning
  cleanChunk <- function(chunk) {
    chunk %>%
      mutate(
        Year = as.integer(Year),
        Month = ifelse(is.na(Month), 1, as.integer(Month)),
        Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"),
        Region = str_to_title(Region),
        Subregion = str_to_sentence(Subregion),
        Species = str_to_sentence(Species),
        Source = str_to_sentence(Source),
        Microorganism = mo_name(Microorganism),
        Antimicrobial = ab_name(Antimicrobial),
        Class = ab_group(Antimicrobial)
      ) %>% 
      select(Date, Region, Subregion, Species, Source, Microorganism, Antimicrobial, Class, Interpretation)
  }
  
  # Clean chunks in parallel
  cleanedChunks <- mclapply(chunks, cleanChunk, mc.cores = numCores)
  
  # Combine the cleaned chunks
  cleanData <- do.call(bind_rows, cleanedChunks)
  
  return(cleanData)
}
