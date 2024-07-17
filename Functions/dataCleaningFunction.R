# library(foreach)
# library(doParallel)

dataCleaner <- function(rawData){
  
  numCores <- detectCores() - 1
  
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  chunks <- split(rawData, 1:numCores)
  
  cleanChunk <- function(chunk) {
    chunk %>%
      mutate(
        ID = as.character(ID),
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
      select(ID, Date, Region, Subregion, Species, Source, Microorganism, Antimicrobial, Class, Interpretation) %>% 
      filter(Interpretation %in% c("S","R","I"))
  }
  
  cleanedChunks <- mclapply(chunks, cleanChunk, mc.cores = numCores)
  
  cleanData <- do.call(bind_rows, cleanedChunks)
  
  return(cleanData)
}
