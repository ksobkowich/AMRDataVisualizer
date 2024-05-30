preprocessMapData <- function(data) {
  uniqueRegions <- unique(data$Region)
  options(tigris_class = "generalized")
  
  map <- counties(state = uniqueRegions, cb = TRUE, resolution = "20m") %>% 
    rename(Region = STATE_NAME, Subregion = NAME) %>%
    mutate(Subregion = tolower(gsub(" County", "", Subregion))) %>%
    select(Region, Subregion, geometry)
  
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
  lookup <- sapply(data$Subregion, function(x) {
    map$Subregion[which.min(stringdist(x, map$Subregion, method = "jw"))]
  })
  
  mapData <- data %>%
    mutate(Subregion = lookup[as.character(Subregion)]) %>% 
    mutate(Subregion = as.character(Subregion)) %>% 
    left_join(map, by = c("Region", "Subregion"))
  
  return(mapData)
}
