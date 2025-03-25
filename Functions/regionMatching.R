preprocessMapData <- function(data) {
  
  uniqueRegions <- unique(data$Region)
  uniqueSubregions <- unique(data$Subregion)
  options(tigris_class = "generalized")
  
  if (all(is.na(uniqueSubregions) | uniqueSubregions == "")) {
    map <- st_read("./Data/mapFiles/USA/usa_state.shp") %>% 
      mutate(Subregion = NA_character_) %>%
      filter(Region %in% uniqueRegions) %>% 
      select(Region, Subregion, geometry)
    
  } else {
    
    if (all(grepl("^\\d{3}$", uniqueSubregions))){
      
      map <- st_read("./Data/mapFiles/USA/usa_zcta3.shp") %>%
        mutate(Region = NA_character_) %>% 
        filter(Subregion %in% uniqueSubregions) %>% 
        select(Region, Subregion, geometry)
      
    } else {
      
      map <- st_read("./Data/mapFiles/USA/usa_county.shp") %>%
        filter(Region %in% uniqueRegions) %>% 
        select(Region, Subregion, geometry)
      
    }
  }
  return(map)
  
}
  
preprocessPlotData <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  mapData <- data %>% 
    select(Region, Subregion, Interpretation) %>% 
    group_by(Region, Subregion) %>% 
    summarise(
      S = sum(Interpretation == "S"),
      I = sum(Interpretation == "I"),
      R = sum(Interpretation == "R"),
      .groups = "drop"
    ) %>% 
    mutate(Count = S + I + R,
           propS = S / Count,
           propI = I / Count,
           propR = R / Count,
           Subregion = tolower(gsub(" County", "", Subregion))) %>%
    select(Region, Subregion, propS, propI, propR, Count)
  
  return(mapData)
}

matchSubregions <- function(map, data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  
  numCores <- detectCores() - 1
  
  chunks <- split(data, rep(1:numCores, length.out = nrow(data)))
  
  extract_locations <- function(text) {
    if (is.na(text) || text == "") {
      return(character(0))  # Return empty character vector for missing values
    }
    
    text <- tolower(text)
    text <- gsub("[[:punct:]]", "", text)
    text <- gsub("\\s+", " ", text)
    parsed <- spacy_parse(text)
    
    if (nrow(parsed) == 0) {
      return(character(0))
    }
    
    locations <- parsed %>%
      filter(entity == 'GPE_B' | entity == 'GPE_I') %>%
      select(token)
    
    return(locations$token)
  }
  
  lookup <- sapply(data$Subregion, function(x) {
    if (is.na(x) || x == "") {  
      return(NA)  # Explicitly return NA for missing values
    }
    
    extracted_location <- extract_locations(x)  # Extract location using NER
    
    if (length(extracted_location) > 0) {
      return(tolower(extracted_location))
    } else {
      return(x)
    }
  })
  
  data$Subregion <- lookup  # Replace original column with mapped values
  
  mapData <- data %>%
    mutate(Subregion = as.character(Subregion)) %>%  # Ensure consistent type
    left_join(map, by = c("Region", "Subregion"))
  
  return(mapData)
}
