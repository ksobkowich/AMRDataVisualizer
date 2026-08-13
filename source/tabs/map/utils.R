#' This file contains utility functions used in the map tab.
#'
#' @keywords internal
NULL

#' Preprocess map shapefile data based on regions and subregions in the data.
#'
#' @param data  Dataframe with data used in the map plot.
#' @return      Simple features dataframe with map shapefile data.
preprocessMapData <- function(data) {
  uniqueRegions <- unique(data$Region)
  uniqueSubregions <- unique(data$Subregion)
  options(tigris_class = "generalized")

  if (all(is.na(uniqueSubregions) | uniqueSubregions == "")) {
    # No subregion data - check what's in Region
    
    # Check if Region contains ZIP codes (3 or 5 digits)
    is_zip_in_region <- all(grepl("^\\d{3,5}$", uniqueRegions[!is.na(uniqueRegions)]))
    
    if (is_zip_in_region) {
      # ZIP codes in Region column - load ZIP shapefile
      uniqueRegions_3digit <- unique(substr(as.character(uniqueRegions), 1, 3))
      
      map <- st_read("./Data/mapFiles/USA/usa_zcta3.shp") %>%
        rename(Region = Subregion) %>%  # ZIP shapefile has ZIPs in Subregion column
        mutate(
          Subregion = NA_character_,
          Region = as.character(Region)  # Ensure character type for joining
        ) %>%
        filter(Region %in% uniqueRegions_3digit) %>%
        select(Region, Subregion, geometry)
      
    } else {
      # State names or abbreviations in Region column - load state shapefile
      
      # Check if we have state abbreviations (2 letters, any case)
      is_state_abbrev <- all(grepl("^[A-Za-z]{2}$", uniqueRegions[!is.na(uniqueRegions)]))
      
      if (is_state_abbrev) {
        # Convert abbreviations to full names for matching
        state_lookup <- c(
          "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas",
          "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware",
          "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawaii", "ID" = "Idaho",
          "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa", "KS" = "Kansas",
          "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland",
          "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi",
          "MO" = "Missouri", "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada",
          "NH" = "New Hampshire", "NJ" = "New Jersey", "NM" = "New Mexico", "NY" = "New York",
          "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio", "OK" = "Oklahoma",
          "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island", "SC" = "South Carolina",
          "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah",
          "VT" = "Vermont", "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia",
          "WI" = "Wisconsin", "WY" = "Wyoming", "DC" = "District of Columbia"
        )
        
        # Normalize to uppercase for lookup
        uniqueRegions_upper <- toupper(uniqueRegions)
        uniqueRegions_full <- state_lookup[uniqueRegions_upper]
        
        # Create reverse lookup BEFORE loading shapefile
        reverse_lookup <- setNames(names(state_lookup), state_lookup)
        
        map <- st_read("./Data/mapFiles/USA/usa_state.shp") %>%
          mutate(Subregion = NA_character_) %>%
          filter(Region %in% uniqueRegions_full) %>%
          mutate(Region = reverse_lookup[Region]) %>%  # Convert BEFORE select
          select(Region, Subregion, geometry)
        
        # Convert back to original case
        # Create a mapping from uppercase to original case
        case_mapping <- setNames(uniqueRegions, uniqueRegions_upper)
        map <- map %>%
          mutate(Region = case_mapping[Region])
        
      } else {
        # Full state names - use as-is
        map <- st_read("./Data/mapFiles/USA/usa_state.shp") %>%
          mutate(Subregion = NA_character_) %>%
          filter(Region %in% uniqueRegions) %>%
          select(Region, Subregion, geometry)
      }
    }
    
  } else {
    # Has subregion data - existing logic
    # Check for both 3-digit and 5-digit ZIP codes
    is_zip <- all(grepl("^\\d{3,5}$", uniqueSubregions[!is.na(uniqueSubregions)]))
    
    if (is_zip) {
      # Truncate 5-digit ZIPs to 3-digit for ZCTA3 matching
      uniqueSubregions_3digit <- unique(substr(uniqueSubregions, 1, 3))
      
      map <- st_read("./Data/mapFiles/USA/usa_zcta3.shp") %>%
        mutate(Region = NA_character_) %>%
        filter(Subregion %in% uniqueSubregions_3digit) %>%
        select(Region, Subregion, geometry)
      
      print(paste("Sample Subregions from shapefile:", paste(head(unique(map$Subregion)), collapse = ", ")))
    } else {
      map <- st_read("./Data/mapFiles/USA/usa_county.shp") %>%
        filter(Region %in% uniqueRegions) %>%
        select(Region, Subregion, geometry)
    }
  }
  return(map)
}

#' Preprocess data for plotting on map.
#'
#' @param data  Dataframe with data used in the map plot.
#' @return      Dataframe processed for plotting on map.
preprocessPlotData <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Check if we have subregion data
  has_subregion <- any(!is.na(data$Subregion) & data$Subregion != "")
  
  if (has_subregion) {
    # Group by both Region and Subregion
    mapData <- data %>%
      select(Region, Subregion, Interpretation) %>%
      group_by(Region, Subregion) %>%
      # Need to remove NAs otherwise sum returns NA
      summarise(
        S = sum(Interpretation == "S", na.rm = TRUE),
        I = sum(Interpretation == "I", na.rm = TRUE),
        R = sum(Interpretation == "R", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Count = S + I + R,
        propS = S / Count,
        propI = I / Count,
        propR = R / Count,
        Subregion = tolower(gsub(" County", "", Subregion))
      ) %>%
      select(Region, Subregion, propS, propI, propR, Count)
  } else {
    # Group by Region only (for ZIP-in-Region or state-only data)
    mapData <- data %>%
      mutate(Region = as.character(Region)) %>%  # Ensure character type
      select(Region, Interpretation) %>%
      group_by(Region) %>%
      summarise(
        S = sum(Interpretation == "S", na.rm = TRUE),
        I = sum(Interpretation == "I", na.rm = TRUE),
        R = sum(Interpretation == "R", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Count = S + I + R,
        propS = S / Count,
        propI = I / Count,
        propR = R / Count,
        Subregion = NA_character_
      ) %>%
      select(Region, Subregion, propS, propI, propR, Count)
  }

  return(mapData)
}

#' Preprocess data for prevalence plotting on map.
#'
#' @param filteredData    Dataframe with filtered data (selected organisms).
#' @param unfilteredData  Dataframe with unfiltered data (for regional totals).
#' @param dateFilter      Optional date filter to apply to denominator (list with start_date and end_date).
#' @return                Dataframe processed for plotting prevalence on map.
preprocessPlotDataPrevalence <- function(filteredData, unfilteredData, dateFilter = NULL) {
  if (is.null(filteredData) || nrow(filteredData) == 0) {
    return(NULL)
  }
  
  # Check if we have subregion data
  has_subregion <- any(!is.na(filteredData$Subregion) & filteredData$Subregion != "")
  
  if (has_subregion) {
    # Numerator: count of selected organisms (from filtered data)
    numeratorData <- filteredData %>%
      select(Region, Subregion, Microorganism) %>%
      group_by(Region, Subregion) %>%
      summarise(Numerator = n(), .groups = "drop")
    
    # Denominator: total isolates per region (from unfiltered data)
    # Apply ONLY the Date filter if it exists
    totalsData <- unfilteredData
    
    if (!is.null(dateFilter) && "Date" %in% names(unfilteredData)) {
      if (!is.null(dateFilter$start_date) && !is.null(dateFilter$end_date)) {
        totalsData <- totalsData %>%
          filter(Date >= dateFilter$start_date & Date <= dateFilter$end_date)
      }
    }
    
    totals <- totalsData %>%
      select(Region, Subregion) %>%
      group_by(Region, Subregion) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Combine and calculate prevalence
    mapData <- totals %>%
      left_join(numeratorData, by = c("Region", "Subregion")) %>%
      mutate(
        Numerator = replace_na(Numerator, 0),  # Regions with no selected organisms get 0
        propPrevalence = Numerator / Count,
        Subregion = tolower(gsub(" County", "", Subregion))
      ) %>%
      select(Region, Subregion, propPrevalence, Numerator, Count)
    
  } else {
    # Group by Region only (for ZIP-in-Region or state-only data)
    
    # Numerator: count of selected organisms
    numeratorData <- filteredData %>%
      mutate(Region = as.character(Region)) %>%  # Ensure character type
      select(Region, Microorganism) %>%
      group_by(Region) %>%
      summarise(Numerator = n(), .groups = "drop")
    
    # Denominator: total isolates per region
    totalsData <- unfilteredData
    
    if (!is.null(dateFilter) && "Date" %in% names(unfilteredData)) {
      if (!is.null(dateFilter$start_date) && !is.null(dateFilter$end_date)) {
        totalsData <- totalsData %>%
          filter(Date >= dateFilter$start_date & Date <= dateFilter$end_date)
      }
    }
    
    totals <- totalsData %>%
      mutate(Region = as.character(Region)) %>%  # Ensure character type
      select(Region) %>%
      group_by(Region) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Combine and calculate prevalence
    mapData <- totals %>%
      left_join(numeratorData, by = "Region") %>%
      mutate(
        Numerator = replace_na(Numerator, 0),
        propPrevalence = Numerator / Count,
        Subregion = NA_character_
      ) %>%
      select(Region, Subregion, propPrevalence, Numerator, Count)
  }
  
  return(mapData)
}

#' TODO: Documentation
#' [Summary]
#'
#' @param map [Description]
#' @param data [Description]
#' @return [Description]
matchSubregions <- function(map, data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Check if Subregions are ZIP codes (all numeric, 3 or 5 digits)
  # If so, skip NLP processing entirely
  sample_subregions <- unique(data$Subregion[!is.na(data$Subregion)])
  
  # FIX: Only treat as ZIP if we actually have subregion data
  has_subregion <- length(sample_subregions) > 0
  is_zip_code <- has_subregion && all(grepl("^\\d{3,5}$", sample_subregions))
  
  if (is_zip_code) {
    # ZIP codes don't need NLP processing - use them directly
    # For 5-digit ZIPs, truncate to 3-digit for ZCTA3 matching
    data <- data %>%
      mutate(Subregion = ifelse(
        !is.na(Subregion) & nchar(Subregion) == 5,
        substr(Subregion, 1, 3),
        Subregion
      ))
    
    print(paste("Sample Subregions from data after truncation:", paste(head(unique(data$Subregion)), collapse = ", ")))
    
    # Join only on Subregion for ZIP codes (shapefile has Region = NA)
    mapData <- data %>%
      mutate(Subregion = as.character(Subregion)) %>%
      left_join(map, by = "Subregion")
    
    print(paste("Rows with geometry after join:", sum(!is.na(st_dimension(mapData$geometry)))))
    print(paste("Rows without geometry after join:", sum(is.na(st_dimension(mapData$geometry)))))
    
    return(mapData)
  }
  
  # FIX: Handle state-level only data (no subregions at all)
  if (!has_subregion) {
    # No subregion data - join on Region only
    mapData <- data %>%
      left_join(map, by = "Region")
    
    return(mapData)
  }

  # For non-ZIP subregions (counties, etc.), use spaCy NLP
  # This requires spaCy to be installed
  
  numCores <- detectCores() - 1
  chunks <- split(data, rep(1:numCores, length.out = nrow(data)))

  #' Extract location names using spaCy NER
  #'
  #' @param text Location text to parse
  #' @return Extracted location token(s)
  extract_locations <- function(text) {
    if (is.na(text) || text == "") {
      return(character(0))
    }

    text <- tolower(text)
    text <- gsub("[[:punct:]]", "", text)
    text <- gsub("\\s+", " ", text)
    
    # Check if spaCy is available
    if (!requireNamespace("spacyr", quietly = TRUE)) {
      warning("spacyr package not available. Returning original text.")
      return(text)
    }
    
    tryCatch({
      parsed <- spacy_parse(text)
      
      if (nrow(parsed) == 0) {
        return(character(0))
      }

      locations <- parsed %>%
        filter(entity == 'GPE_B' | entity == 'GPE_I') %>%
        select(token)

      return(locations$token)
    }, error = function(e) {
      warning("spaCy parsing failed: ", e$message, ". Returning original text.")
      return(text)
    })
  }

  lookup <- sapply(data$Subregion, function(x) {
    if (is.na(x) || x == "") {
      return(NA)
    }

    extracted_location <- extract_locations(x)

    if (length(extracted_location) > 0) {
      return(tolower(extracted_location))
    } else {
      return(tolower(x))
    }
  })

  data$Subregion <- lookup

  mapData <- data %>%
    mutate(Subregion = as.character(Subregion)) %>%
    left_join(map, by = c("Region", "Subregion"))

  return(mapData)
}


# Adapted from statnmap/HatchedPolygons-----------------------------------------

hatched.SpatialPolygons <-
  function(x,
           density = 10, angle = 45,
           fillOddEven = FALSE) {
    
    type <- NULL
    
    
    if (is(x, "SpatialPolygons")) {
      n <- length(slot(x, "polygons"))
      polys <- slot(x, "polygons")
      pO <- slot(x, "plotOrder")
      type <- "sp"
    } else if (st_is(x, c("POLYGON", "MULTIPOLYGON"))[1]) {
      # n <- length(x)
      # To do
      x <- as(x, "Spatial")
      n <- length(slot(x, "polygons"))
      polys <- slot(x, "polygons")
      pO <- slot(x, "plotOrder")
      type <- "sf"
    } else {
      stop("Not a sp::SpatialPolygons or sf::*POLYGON object")
    }
    
    
    if (length(density) != n)
      density <- rep(density, n, n)
    if (length(angle) != n)
      angle <- rep(angle, n, n)
    all.Lines <- list()
    all.Lines.ID <- numeric(0)
    
    for (j in pO) {
      all.Lines.tmp <- polygonRingHolesLines(
        polys[[j]],
        density = density[j], angle = angle[j],
        ID = polys[[j]]@ID,
        fillOddEven = fillOddEven
      )
      if(length(all.Lines.tmp)==0)
        next()
      
      all.Lines.ID <- c(all.Lines.ID, rep(polys[[j]]@ID, length(all.Lines.tmp)))
      all.Lines[length(all.Lines) + 1:length(all.Lines.tmp)] <- all.Lines.tmp
    }
    # Correct ID
    SpatialLinesDF <- SpatialLinesDataFrame(
      SpatialLines(all.Lines),
      data = data.frame(ID = all.Lines.ID),
      match.ID = FALSE)
    
    if (type == "sf") {
      SpatialLinesDF_sf <- st_as_sf(SpatialLinesDF)
      return(SpatialLinesDF_sf)
    } else {
      return(SpatialLinesDF)
    }
  }

polygonRingHolesLines <- function(Sr,
                                  density = 0.5,
                                  angle = 45,
                                  ID = 1,
                                  fillOddEven = FALSE) {
  if (!is(Sr, "Polygons"))
    stop("Not an Polygons object")
  
  if (!is.null(density)) hatch <- TRUE
  else hatch <- FALSE
  pO <- slot(Sr, "plotOrder")
  polys <- slot(Sr, "Polygons")
  
  if (hatch) {
    all.Lines <- list()
    for (i in pO) {
      if (!slot(polys[[i]], "hole")) {
        # Transform polygon as parallel lines
        lines.hatch <- polygon.fullhatch(slot(polys[[i]], "coords"),
                                         density = density, angle = angle, fillOddEven = fillOddEven)
        
        if(length(lines.hatch)==0)
        {
          warning("Polygon too small to contain any lines.  Consider increasing 'density'.")
          next()
        }
        
        # Transform as SpatialLines
        Lines.i <- SpatialLines(list(Lines(
          apply(lines.hatch, 1,
                function(x) Line(cbind(c(x[1], x[3]), c(x[2], x[4])))),
          ID = i)))
        
        # Clean Lines if over a "hole"
        #
        # Lines.i.holes <- rgeos::gIntersection(Lines.i, SpatialPolygons(list(Sr)),
        #                                       drop_lower_td = TRUE)
        Lines.i.holes <- st_intersection(
          Lines.i  |> st_as_sfc(), 
          SpatialPolygons(list(Sr)) |> st_as_sfc()
        ) |> as("Spatial")
        
        if (!is.null(Lines.i.holes)) {
          Lines.i.holes@lines[[1]]@ID <- paste0(ID, ".", i)
          all.Lines[[length(all.Lines) + 1]] <- Lines.i.holes@lines[[1]]
        }
      }
    }
  }
  return(all.Lines)
}

polygon.fullhatch <- function(x, y = NULL, density, angle, ..debug.hatch = FALSE,
                              fillOddEven = FALSE,
                              ...) {
  if (is.null(y)) {
    y <- x[,2]
    x <- x[,1]
  }
  if (x[1] != x[length(x)] | y[1] != y[length(y)]) {
    x <- c(x, x[1L])
    y <- c(y, y[1L])
  }
  angle <- angle%%180
  # if (par("xlog") || par("ylog")) {
  #   warning("cannot hatch with logarithmic scale active")
  #   return()
  # }
  # usr <- par("usr")
  # pin <- par("pin")
  # upi <- c(usr[2L] - usr[1L], usr[4L] - usr[3L])/pin
  #if (upi[1L] < 0)
  #  angle <- 180 - angle
  #if (upi[2L] < 0)
  #  angle <- 180 - angle
  # upi <- abs(upi)
  res <- NULL
  xd <- cos(angle/180 * pi) #* upi[1L]
  yd <- sin(angle/180 * pi) #* upi[2L]
  if (angle < 45 || angle > 135) {
    if (angle < 45) {
      first.x <- max(x)
      last.x <- min(x)
    }
    else {
      first.x <- min(x)
      last.x <- max(x)
    }
    # y.shift <- upi[2L]/density/abs(cos(angle/180 * pi))
    y.shift <- 1/density/abs(cos(angle/180 * pi))
    x0 <- 0
    y0 <- floor((min(y) - first.x * yd/xd)/y.shift) *
      y.shift
    y.end <- max(y) - last.x * yd/xd
    while (y0 < y.end) {
      res.tmp <- polygon.onehatch(x, y, x0, y0, xd, yd, ..debug.hatch = ..debug.hatch,
                                  fillOddEven = fillOddEven,
                                  ...)
      if (!is.null(res.tmp)) {
        res <- bind_rows(res, res.tmp)
      }
      y0 <- y0 + y.shift
    }
  } else {
    if (angle < 90) {
      first.y <- max(y)
      last.y <- min(y)
    } else {
      first.y <- min(y)
      last.y <- max(y)
    }
    # x.shift <- upi[1L]/density/abs(sin(angle/180 * pi))
    x.shift <- 1/density/abs(sin(angle/180 * pi))
    x0 <- floor((min(x) - first.y * xd/yd)/x.shift) * x.shift
    y0 <- 0
    x.end <- max(x) - last.y * xd/yd
    while (x0 < x.end) {
      # Get lines
      res.tmp <- polygon.onehatch(x, y, x0, y0, xd, yd, ..debug.hatch = ..debug.hatch,
                                  fillOddEven = fillOddEven,
                                  ...)
      if (!is.null(res.tmp)) {
        res <- bind_rows(res, res.tmp)
      }
      x0 <- x0 + x.shift
    }
    # arrows(res$lx1, res$ly1, res$lx2, res$ly2, col = "red", code = 0)
  }
  return(res)
}

polygon.onehatch <- function(x, y, x0, y0, xd, yd, ..debug.hatch = FALSE,
                             fillOddEven = FALSE,
                             ...) {
  if (..debug.hatch) {
    graphics::points(x0, y0)
    graphics::arrows(x0, y0, x0 + xd, y0 + yd)
  }
  halfplane <- as.integer(xd * (y - y0) - yd * (x -
                                                  x0) <= 0)
  cross <- halfplane[-1L] - halfplane[-length(halfplane)]
  does.cross <- cross != 0
  if (!any(does.cross))
    return()
  x1 <- x[-length(x)][does.cross]
  y1 <- y[-length(y)][does.cross]
  x2 <- x[-1L][does.cross]
  y2 <- y[-1L][does.cross]
  t <- (((x1 - x0) * (y2 - y1) - (y1 - y0) * (x2 -
                                                x1))/(xd * (y2 - y1) - yd * (x2 - x1)))
  o <- order(t)
  tsort <- t[o]
  crossings <- cumsum(cross[does.cross][o])
  if (fillOddEven)
    crossings <- crossings%%2
  drawline <- crossings != 0
  lx <- x0 + xd * tsort
  ly <- y0 + yd * tsort
  lx1 <- lx[-length(lx)][drawline]
  ly1 <- ly[-length(ly)][drawline]
  lx2 <- lx[-1L][drawline]
  ly2 <- ly[-1L][drawline]
  # segments(lx1, ly1, lx2, ly2, ...)
  # get lines
  data.frame(lx1 = lx1, ly1 = ly1, lx2 = lx2, ly2 = ly2)
}
