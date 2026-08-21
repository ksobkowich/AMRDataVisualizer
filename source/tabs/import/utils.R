#' This file contains utility functions used in the import tab.
#'
#' @keywords internal
NULL


#' Convert host/species codes to standardized AMR package host names.
#' 
#' This is a corrected implementation of the AMR package's convert_host() function,
#' which has a bug where it converts input to lowercase but then checks against uppercase codes.
#' 
#' @param x Vector of host/species codes or names
#' @return Vector of standardized host names compatible with AMR::clinical_breakpoints
#' @seealso AMR::clinical_breakpoints for valid host values
convert_host_fixed <- function(x) {
  x_original <- x
  x <- gsub("[^a-zA-Z ]", "", trimws(tolower(as.character(x))), perl = TRUE)
  x_out <- rep(NA_character_, length(x))
  
  # Special cases
  x_out[x == "human"] <- "human"
  x_out[x == "ecoff"] <- "ECOFF"
  
  # Veterinary breakpoints - matching clinical_breakpoints$host (type == "animal")
  # Check both uppercase abbreviations AND full names/patterns
  
  # Dogs
  x_out[is.na(x_out) & (
    toupper(x_original) == "CAN" | 
    x == "dog" | x == "dogs" | x == "canine" | 
    grepl("canis lupus", x, ignore.case = TRUE)
  )] <- "dogs"
  
  # Cattle
  x_out[is.na(x_out) & (
    toupper(x_original) == "BOV" | 
    x == "cattle" | x == "bovine" | 
    grepl("bos taurus", x, ignore.case = TRUE)
  )] <- "cattle"
  
  # Swine
  x_out[is.na(x_out) & (
    toupper(x_original) == "POR" | 
    x == "swine" | x == "pig" | x == "pigs" | x == "porcine" | 
    grepl("sus scrofa", x, ignore.case = TRUE)
  )] <- "swine"
  
  # Cats
  x_out[is.na(x_out) & (
    toupper(x_original) == "FEL" | 
    x == "cat" | x == "cats" | x == "feline" | 
    grepl("felis catus", x, ignore.case = TRUE)
  )] <- "cats"
  
  # Horse
  x_out[is.na(x_out) & (
    toupper(x_original) == "EQU" | 
    x == "horse" | x == "horses" | x == "equine" | 
    grepl("equus ferus", x, ignore.case = TRUE)
  )] <- "horse"
  
  # Poultry
  x_out[is.na(x_out) & (
    toupper(x_original) %in% c("POUL", "AVI") | 
    x == "bird" | x == "birds" | x == "chicken" | x == "poultry" | 
    grepl("gallus gallus", x, ignore.case = TRUE)
  )] <- "poultry"
  
  # Amphibian
  x_out[is.na(x_out) & (
    toupper(x_original) == "AMP" | 
    x == "amphibian" | x == "frog" | x == "toad"
  )] <- "amphibian"
  
  # Additional animals (less common)
  x_out[is.na(x_out) & (
    toupper(x_original) == "CAM" | 
    x == "camel" | x == "camels" | x == "camelid"
  )] <- "camels"
  
  x_out[is.na(x_out) & (
    toupper(x_original) == "CER" | 
    x == "deer" | x == "deers" | x == "cervine"
  )] <- "deer"
  
  x_out[is.na(x_out) & (
    toupper(x_original) == "CAP" | 
    x == "goat" | x == "goats" | x == "caprine"
  )] <- "goats"
  
  x_out[is.na(x_out) & (
    toupper(x_original) == "LAG" | 
    x == "rabbit" | x == "rabbits" | x == "leporine"
  )] <- "rabbits"
  
  x_out[is.na(x_out) & (
    toupper(x_original) == "OVI" | 
    x == "sheep" | x == "sheeps" | x == "ovine"
  )] <- "sheep"
  
  # Aquatic
  x_out[is.na(x_out) & x == "aquatic"] <- "aquatic"
  
  return(x_out)
}


#' TODO: Documentation
#' [Summary]
#'
#' @param rawData [Description]
#' @param additionalCols [Description]
#' @param breakpoint [Description]
#' @return [Description]
dataCleaner <- function(rawData, additionalCols = NULL, breakpoint = "CLSI") {
  # Precompute mappings
  ab_name_data <- unique(rawData$Antimicrobial) %>%
    data.frame(Antimicrobial = ., stringsAsFactors = FALSE) %>%
    mutate(ab_name = ab_name(Antimicrobial, minimum_matching_score = 0.751)) %>%
    mutate(ab_name = ifelse(ab_name == "(unknown name)", Antimicrobial, ab_name)) %>%
    arrange(Antimicrobial)

  ab_log <- ab_name_data

  mo_name_data <- unique(rawData$Microorganism) %>%
    data.frame(Microorganism = ., stringsAsFactors = FALSE) %>%
    mutate(mo_name = mo_name(Microorganism, minimum_matching_score = 0.675)) %>%
    mutate(mo_name = ifelse(mo_name == "(unknown name)", Microorganism, mo_name)) %>%
    arrange(Microorganism)

  mo_log <- mo_name_data

  ab_class_data <- unique(rawData$Antimicrobial) %>%
    data.frame(Antimicrobial = ., ab_class = ab_group(.))

  # Join mappings
  rawData <- rawData %>%
    left_join(ab_name_data, by = "Antimicrobial") %>%
    left_join(mo_name_data, by = "Microorganism") %>%
    left_join(ab_class_data, by = "Antimicrobial")

  # Clean extra columns
  if (!is.null(additionalCols)) {
    additionalCols <- as.character(additionalCols)
    additionalCols <- additionalCols[additionalCols %in% names(rawData)]
  }

  # Interpretation normalization
  normalized_interpretation_map <- c(
    "S" = "S",
    "SUS" = "S",
    "SUSC" = "S",
    "SUSCEPTIBLE" = "S",
    "SDD" = "S",
    "SD" = "S",
    "0" = "S",
    "I" = "I",
    "INT" = "I",
    "INTERMEDIATE" = "I",
    "R" = "R",
    "RES" = "R",
    "RESISTANT" = "R",
    "1" = "R"
  )

  #' Get the valid year from a vector of years.
  #'
  #' @param years Vector of years.
  #' @return      Vector of valid years.
  getValidYear <- function(years) {
    years <- as.integer(years)
    years[is.na(years) | is.null(years)] <- NA_integer_
    ifelse(!is.na(years) & years < 1900, 2000 + years, years)
  }

  #' Detect the date format from a vector of date strings.
  #'
  #' @param date_vector Vector of date strings.
  #' @return            Detected date format string or NULL if not detected.
  detect_date_format <- function(date_vector) {
    known_formats <- c(
      "%Y-%m-%d",
      "%d-%m-%Y",
      "%m-%d-%Y",
      "%Y/%m/%d",
      "%d/%m/%Y",
      "%m/%d/%Y",
      "%Y%m%d",
      "%d-%b-%Y",
      "%b %d, %Y",
      "%d %B %Y",
      "%Y-%m-%d %H:%M",
      "%d-%m-%Y %H:%M",
      "%m-%d-%Y %H:%M"
    )

    first_valid <- suppressWarnings(na.omit(date_vector)[1])

    if (is.null(first_valid) || is.na(first_valid)) {
      warning("No valid dates found to detect format.")
      return(NULL)
    }

    first_valid <- stringr::str_trim(first_valid)

    for (fmt in known_formats) {
      try_date <- as.Date(first_valid, format = fmt)
      if (!is.na(try_date)) return(fmt)
    }

    warning("Date format could not be detected; falling back to slow parser.")
    return(NULL)
  }

  #' TODO: Documentation
  #' [Summary]
  #'
  #' @param chunk [Description]
  #' @param additionalCols [Description]
  #' @return [Description]
  clean_chunk <- function(chunk, additionalCols = NULL) {
    additionalColsData <- if (!is.null(additionalCols)) {
      chunk %>% select(all_of(additionalCols))
    } else {
      NULL
    }

    date_format <- if ("Date" %in% names(chunk)) detect_date_format(chunk$Date) else NULL

    cleanedChunk <- chunk %>%
      mutate(
        InternalID = as.character(InternalID),
        ID = as.character(ID),
        Year = if ("Year" %in% names(.)) getValidYear(Year) else NA_integer_,

        Month = if ("Month" %in% names(.)) {
          ifelse(is.na(Month), 1, as.integer(Month))
        } else {
          1
        },

        Date = if ("Date" %in% names(.)) {
          if (!is.null(date_format)) {
            as.Date(Date, format = date_format)
          } else {
            lubridate::parse_date_time(Date, orders = c("ymd", "mdy", "dmy")) %>% as.Date()
          }
        } else {
          as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")
        },
        Region = str_to_title(Region),
        Subregion = str_to_sentence(Subregion),
    
        # Pre-standardize Species using our fixed conversion function
        Species = convert_host_fixed(Species),
    
        Source = str_to_sentence(Source),
        Microorganism = mo_name,
        Antimicrobial = ab_name,
        Class = ab_class,
        UTI = findUtiMatch(Source)
      )

    if ("Interpretation" %in% names(chunk)) {
      cleanedChunk <- cleanedChunk %>%
        mutate(
          Interpretation = recode(
            toupper(str_trim(as.character(Interpretation))),
            !!!normalized_interpretation_map,
            .default = NA_character_
          )
        )
    } else {
      cleanedChunk <- cleanedChunk %>%
        mutate(
          MIC_raw = case_when(
            !is.na(Sign) & !is.na(Value) ~ str_c(trimws(Sign), trimws(Value)),
            is.na(Sign) & !is.na(Value) ~ trimws(Value),
            TRUE ~ NA_character_
          ),
          MIC = as.mic(MIC_raw)
        )
    }

    if (!"Interpretation" %in% names(cleanedChunk)) {
      key_cols <- c("MIC", "Species", "UTI", "Microorganism", "Antimicrobial")

      #' MIC has been chosen.
      #' Calculate the S/I/R interpretation based on the MIC and other info.
      sir_map <- cleanedChunk %>%
        select(all_of(key_cols)) %>%
        distinct() %>%
        mutate(
          ab = as.ab(Antimicrobial),
          mo = as.mo(Microorganism),
          # host is already standardized in cleanedChunk, just use it directly
          Interpretation = MIC,
          MIC = as.character(MIC)
        ) |>
        mutate_if(
          is.mic,
          as.sir,
          mo = "mo",
          ab = "ab",
          guideline = breakpoint,
          uti = "UTI",
          host = "Species",
          breakpoint_type = getOption("AMR_breakpoint_type", "animal"),
          substitute_missing_r_breakpoint = getOption("AMR_substitute_missing_r_breakpoint", TRUE),
          capped_mic_handling = getOption("AMR_capped_mic_handling", "standard")
        ) |>
        mutate(
          Interpretation = as.character(Interpretation),
          MIC = as.mic(MIC)
        )

      cleanedChunk <- cleanedChunk %>%
        left_join(sir_map, by = key_cols)
    }
    cleanedChunk <- cleanedChunk %>%
      select(any_of(c(
        "InternalID",
        "ID",
        "Date",
        "Region",
        "Subregion",
        "Species",
        "Source",
        "Microorganism",
        "Antimicrobial",
        "Class",
        "MIC",
        "Interpretation",
        "UTI"
      )))

    if (!is.null(additionalColsData)) {
      cleanedChunk <- bind_cols(cleanedChunk, additionalColsData)
    }

    return(cleanedChunk)
  }

  cleanData <- clean_chunk(chunk = rawData, additionalCols = additionalCols)

  bp_log <- sir_interpretation_history()
  if (!is.null(bp_log) && nrow(bp_log) > 0) {
    bp_log <- bp_log %>%
      mutate(type = getOption("AMR_breakpoint_type", "animal")) %>% # Matches the default used in as.sir() above
      select(
        "Antimicrobial" = ab_given,
        "Microorganism" = mo_given,
        "Species" = host_given,
        "AB Used" = ab,
        "MO Used" = mo,
        "Host Used" = host,
        "UTI" = uti,
        "Guideline" = guideline,
        "Reference" = ref_table,
        "Breakpoint (S-R)" = breakpoint_S_R,
        "MIC" = input_given,
        "Interpretation" = outcome,
        #' These columns are used when downloading the used breakpoints but will not be
        #' shown in any preview of the bp log.
        method,
        type,
        site
      ) %>%
      mutate(
        Interpretation = as.character(Interpretation),
        Interpretation = replace_na(Interpretation, "Could not interpret")
      ) %>%
      distinct()

    # Update the Species in the cleaned data with the host that the AMR package converted it to
    host_map <- bp_log %>%
      select(host = Species, mapped_host = `Host Used`) %>%
      distinct()

    cleanData <- cleanData %>%
      left_join(host_map, by = c("Species" = "host")) %>%
      mutate(Species = coalesce(Species, mapped_host)) %>%
      select(-mapped_host)
  }
  uniqueSources <- unique(cleanData$Source)

  uti_log <- data.frame(
    "Original Source" = uniqueSources,
    "Is UTI?" = findUtiMatch(uniqueSources),
    check.names = FALSE
  )

  return(list(
    mo_log = mo_log,
    ab_log = ab_log,
    bp_log = bp_log,
    uti_log = uti_log,
    cleaned_data = cleanData
  ))
}

#' Get the MIC sign and value from the MIC column.
#'
#' If the MIC column cannot be found, a warning is issued and the original data is returned.
#'
#' @param data  The data frame get the MIC sign and value from.
#' @param sep   The separator used to split the MIC column values into the MIC sign and value.
#'    (I.e. "<= 16" -> "<=" and 16) Default is " ".
#' @returns Data frame with MIC sign and value columns if they are found.
.getMICDataColumns <- function(data, sep = " ") {
  micColumn <- detectColumnByName(data, c("mic", "mic_value", "mic_sign"), all_matches = TRUE)

  if (is.null(micColumn)) {
    warning("MIC column not found. Cannot extract MIC sign and value.")
    return(data)
  }

  if (all(c("MIC_Sign", "MIC_Value") %in% colnames(data))) {
    # MIC sign and value columns already exist no need to extract them again.
    return(data)
  }
  if (length(micColumn) > 1 && !("MIC" %in% micColumn)) {
    warning("Multiple MIC columns found. Only the first one will be used.")
    micColumn <- micColumn[1]
  } else if (length(micColumn) > 1 && ("MIC" %in% micColumn)) {
    # There is a MIC column, use that one.
    micColumn <- "MIC"
  }

  # Could use `AMR::as.mic()` to convert the MIC values, but this would not extract the sign.
  if (!"MIC_Sign" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(MIC_Sign = stringr::str_extract(!!sym(micColumn), "^([<>=]=?|=)"))
  }
  if (!"MIC_Value" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(MIC_Value = stringr::str_remove(!!sym(micColumn), "^([<>=]=?|=)\\s*"))
  }
  return(data)
}

#' Check if the data from a column is a SIR test.
#'
#' @param data  The data to check.
#' @returns "INTERP" if the data is a SIR test, "unknown" if it is not.
.isSirTest <- function(data) {
  isSir <- suppressWarnings(AMR::as.sir(data))
  if (all(is.na(unique(isSir)))) {
    # This column is not a SIR test.
    return("unknown")
  } else {
    return("INTERP")
  }
}

#' Check if the data from a column is a MIC test.
#'
#' @param data  The data to check.
#' @returns If not a MIC test, returns "unknown".
#'          If it is a MIC test, returns:
#'           - "MIC_Sign" if the data only contains signs (e.g., "<", ">", "=").
#'           - "MIC" if the data contains signs and values (e.g., "<4", ">8", "=16").
#'           - "MIC_Value" if the data only contains values (e.g., "4", "8", "16").
.isMicTest <- function(data) {
  isMic <- unique(suppressWarnings(AMR::as.mic(data)))
  if (any(grepl(">|=|<", data))) {
    # Either a sign+value column or just a sign column
    if (all(is.na(isMic))) {
      return("MIC_Sign")
    }
    return("MIC")
  }
  # Only value, no signs - or a non MIC test.
  if (!all(is.na(isMic))) {
    return("MIC_Value")
  }
  return("unknown")
}

# Only returns the first match.
#' Get the test type from a column name and a sample of the data.
#'
#' @param abbreviation  The abbreviation of the test type (e.g., "SIR", "MIC").
#' @param data          A sample of the data in the column.
#' @returns A string representing the test type:
#'          - "INTERP":   SIR tests.
#'         - "MIC_Sign":  MIC tests - only signs.
#'         - "MIC_Value": MIC tests - only values.
#'         - "MIC":       MIC tests with signs and values.
#'         - "unknown":   If the test type cannot be determined.
#' @seealso {@link{.isSirTest}} and {@link{.isMicTest}} for checking the test type.
.getTestTypeFromColumn <- function(abbreviation, data) {
  abbreviation <- tolower(abbreviation)

  non_empty_data <- data[!is.na(data) & data != ""]

  # If there is no data, return unknown.
  if (length(non_empty_data) == 0) {
    warning(
      "!! empty data for abbreviation:",
      abbreviation,
      "!!\n--This will be filtered out--"
    )
    return("unknown")
  }

  # Values that indicate MIC sign only (no numbers).
  mic_symbol_pattern <- "^([<>=]+)$"
  is_symbol_only <- grepl(mic_symbol_pattern, non_empty_data)

  # First check if the column data is MIC symbols only.
  if (all(is_symbol_only)) {
    return("MIC_Sign")
  }

  # Try as.sir() without mo (for literal S/I/R, works), else try as.mic()
  sir_attempt <- tryCatch(
    {
      result <- AMR::as.sir(non_empty_data)
      any(!is.na(result)) # If all are NA, no error was thrown but it's not a SIR test.
    },
    error = function(e) FALSE
  )

  if (sir_attempt) {
    return("SIR")
  } else {
    # Try as.mic
    mic_attempt <- tryCatch(
      {
        result <- AMR::as.mic(non_empty_data)
        any(!is.na(result)) # If all are NA, no error was thrown but it's not a MIC test.
      },
      error = function(e) FALSE
    )
    if (mic_attempt) {
      has_symbol_and_number <- grepl("<|=|>", non_empty_data)
      if (any(has_symbol_and_number)) {
        return("MIC")
      }
      return("MIC_Value")
    }
  }
  warning(
    "!! unknown test type for abbreviation:",
    abbreviation,
    "!!\n--This will be filtered out--"
  )
  return("unknown")
}


#' Get the column information from a data frame.
#'
#' This function takes a data frame and creates a df with the following columns:
#' - `index`: Original column index.
#' - `original_col_name`: The original column name.
#' - `ab_name`: The antibiotic abbreviation in the column name or the metadata column name.
#' - `test_name`: The name of the test (if applicable).
#'                (dervied from the column name - assumes it will be split by "_" if present).
#' - `is_ab`: Whether the column is an antibiotic or metadata column.
#' - `matched_ab_name`: The matched antibiotic name from `AMR::ab_name` (if applicable).
#'
#' @param data  The data frame to get the column information from.
#' @returns A data frame with the the columns described above.
#' @seealso `AMR::ab_name` for matching antibiotic names.
#' @seealso {@link{.getTestTypeFromColumn}} for determining the test type.
.getColumnInfo <- function(data, testColumns) {
  #' Separate the column names by '_' (if present). This will split the column names into
  #' the 'first' and 'second' columns.
  #' If there is no '_' in the column name, the 'second' column will be NA.
  columnInfo <- data.frame(
    index = seq_len(ncol(data)),
    original_col_name = colnames(data),
    column_name = colnames(data),
    is_ab = seq_along(colnames(data)) %in% testColumns,
    stringsAsFactors = FALSE
  ) %>%
    tidyr::separate_wider_delim(
      column_name,
      delim = "_",
      names = c("first", "second"),
      too_few = "align_start",
      too_many = "merge"
    )

  # If a metadata column set `ab_name` to the original column name.
  columnInfo$first <- ifelse(
    !columnInfo$is_ab,
    columnInfo$original_col_name,
    columnInfo$first
  )

  # Remove metadata columns when trying to determine the antibiotic names / tests performed.
  abOnly <- columnInfo %>%
    filter(is_ab)

  #' Check if the first or second column is more populated with `ab_name` matches.
  #' Rename these columns to `ab_name` and `test_name` and set the matches values
  #' to the `matched_ab_name` column.
  firstAbMatches <- suppressWarnings(AMR::ab_name(
    abOnly$first,
    minimum_matching_score = 0.75
  ))
  secondAbMatches <- suppressWarnings(AMR::ab_name(
    abOnly$second,
    minimum_matching_score = 0.75
  ))
  abColName <- "first"
  testColName <- "second"
  abOnly$matched_ab_name <- firstAbMatches
  if (sum(!is.na(firstAbMatches)) < sum(!is.na(secondAbMatches))) {
    abColName <- "second"
    testColName <- "first"
    abOnly$matched_ab_name <- secondAbMatches
  }
  # Set all non matches and non ab columns to NA in the `matched_ab_name` column.
  abOnly <- abOnly |>
    rowwise() |>
    mutate(matched_ab_name = ifelse(grepl("unknown name", matched_ab_name), NA, matched_ab_name))

  # Rename the column with the most ab matches to "ab_name"
  abOnly <- abOnly %>%
    rename(ab_name = !!sym(abColName), test_name = !!sym(testColName))

  columnInfo <- columnInfo %>%
    rename(ab_name = !!sym(abColName), test_name = !!sym(testColName))

  # Get all the unique test names to check.
  testAbbreviations <- unique(abOnly$test_name)
  testAbbreviations <- testAbbreviations[!testAbbreviations %in% c("", NA, "unknown")]

  if (length(testAbbreviations) == 0) {
    #' A single test per antibiotic (the column names were just antimicrobial abbreviations).
    #' No test abbreviations found, set to unknown to determine the test below.
    abOnly$test_name <- "unknown"
    testAbbreviations <- "unknown"
  }

  #' Keep track of the test types that have been found.
  #' This is to avoid duplicates in the test names.
  #' e.g., if both "SIR" and "INTERP" are found, we only want to keep one of them.
  foundTests <- c()

  #' Iterate through the test abbreviations and determine the test type.
  #' Based on the column name and a sample of the data in columns with that test name.
  for (test in testAbbreviations) {
    #' Get all the columns that match the test name.
    #' Get a small portion of the (unique) data to confirm the test type.
    testCols <- abOnly %>%
      filter(test_name == test) %>%
      pull(original_col_name)
    testData <- data %>%
      select(all_of(testCols)) %>%
      as.matrix() %>%
      as.vector() %>%
      unique() %>%
      head(20)
    testType <- .getTestTypeFromColumn(test, testData)

    if (testType %in% foundTests) {
      # If the test type has already been found, skip it otherwise duplicates are introduced.
      next
    }
    if (testType != "unknown") {
      foundTests <- c(foundTests, testType)
    } else {
      # abOnly$is_ab[abOnly$test_name == test] <- FALSE
    }
    abOnly$test_name[abOnly$test_name == test] <- testType
  }

  #' Join the metadata back to the ab data.
  #' Make sure that the metadata columns have not test names.
  columnInfo %>%
    filter(!is_ab) %>%
    mutate(test_name = NA, matched_ab_name = NA) %>%
    rbind(abOnly %>% arrange(is_ab))
}

#' Detect the drug column in a data frame.
#'
#' @param columnValues  The column values to check for drug names.
#' @reutrns             Boolean indicating whether the column contains drug names.
.containsDrugNames <- function(columnValues) {
  testNames <- unique(columnValues) %>% head(20)

  nameMatches <- as.ab(testNames)
  nameMatches <- nameMatches[!is.na(nameMatches)]

  # Require at least 75% of the names to match.
  if (length(nameMatches) > length(testNames) * 0.75) {
    return(TRUE)
  }
  return(FALSE)
}

#' Check if the data is in long or wide format.
#'
#' The data is determined to be in long format if:
#' - There is a column that contains drug names.
#'
#' @param data  The data frame to check.
#' @returns     TRUE if the data is in long format, FALSE otherwise.
#' @seealso {@link{detectDrugColumn}} for detecting drug columns.
#' @seealso {@link{.containsDrugNames}} for checking if a column contains drug names.
.isLongData <- function(data) {
  drugColumn <- detectDrugColumn(data, all_matches = TRUE)
  if (length(drugColumn) > 0) {
    # There is at least one drug name column found.
    # Iterate through them and check that the values are drug names before returning TRUE.
    for (colName in drugColumn) {
      isDrugCol <- .containsDrugNames(data[[drugColumn]])
      if (isDrugCol) {
        return(TRUE)
      }
    }
  }
  message("No drug name columns found in the data. Assuming the data is in wide format.")
  return(FALSE)
}

#' Get the long form data from a dataframe.
#'
#' If the data is already in long form, it will be returned as is.
#'
#' Otherwise:
#'
#' Wide data cases that have been covered:
#' - Single test column names (e.g., "AZITHR")
#' - Single test column names with a test type (e.g., "AZITHR_INTERP")
#' - Multiple test column names with a test type (e.g., "AZITHR_INTERP", "AZITHR_MIC", "AZITHR_OTHERTEST")
#' - MIC columns split into sign and value (e.g., "AZITHR_SIGN", "AZITHR_VALUE" becomes "AZITHR_MIC_Sign" and "AZITHR_MIC_Value")
#'
#' Breakdown:
#' - Get the column information from the data.
#' - Use that information to iterate through the unique test types found.
#' - For each test type pivot the appropriate columns to long format and add to the long data.
#'
#' @param data          The data frame to be transformed.
#' @param testColumns   The indices of the columns that are tests (selected by user).
#' @param isWideFormat  Logical indicating whether the data is in wide format (selected by user).
#' @returns A long form data frame.
#' @seealso {@link{.getColumnInfo}} for getting the column information.
#' @seealso {@link{.getMICDataColumns}} for extracting MIC sign and value columns.
getLongData <- function(data, testColumns, isWideFormat = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    warning("Input data is NULL or empty. Not running getLongData.")
    return(data)
  }
  if (!isWideFormat) {
    message("Data is already in long format. Returning original data.")
    return(data)
  }
  data$row_id <- seq_len(nrow(data))

  # Get the column information.
  columnInfo <- .getColumnInfo(data, testColumns)

  # Unique tests found in the datas column names.
  tests <- unique(columnInfo$test_name)
  tests <- tests[!is.na(tests) & tests != ""]

  # The expected final columns in the long data.
  finalColumns <- columnInfo %>%
    filter(!is_ab) %>%
    pull(ab_name)
  finalColumns <- c(finalColumns, "ab_name", "Antimicrobial", tests)

  testType <- unique(columnInfo %>% filter(is_ab) %>% pull(test_name)) # e.g., "sir"
  # Columns that are metadata columns.
  metadataCols <- columnInfo %>% filter(!is_ab) %>% pull(original_col_name)

  longData <- data %>%
    select(all_of(metadataCols))

  # Iterate through rach test type and add to the long data.
  for (test in testType) {
    # All original columns names for the current test.
    testCols <- columnInfo %>% filter(is_ab, test_name == test) %>% pull(original_col_name)

    if (length(testCols) <= 0) {
      next
    }

    # If the columns are not all the same class, convert them to character.
    colClasses <- sapply(data[testCols], class)
    if (length(unique(colClasses)) > 1) {
      nonCharClasses <- colClasses[colClasses != "character"]
      data <- data %>% mutate(across(all_of(names(nonCharClasses)), as.character))
    }

    # Pivot the test columns to long format.
    testData <- data %>%
      select(all_of(c("row_id", testCols))) %>%
      pivot_longer(
        cols = all_of(testCols),
        names_to = "original_col_name",
        values_to = test
      ) %>%
      left_join(
        columnInfo %>%
          select(original_col_name, ab_name, Antimicrobial = matched_ab_name),
        by = "original_col_name"
      ) %>%
      select(row_id, ab_name, Antimicrobial, !!sym(test))

    if (!"Antimicrobial" %in% colnames(longData)) {
      # If this is the first test we add, join on row_id only.
      longData <- left_join(longData, testData, by = "row_id")
    } else {
      # This is not the first test we add.
      # ab_name and Antimicrobial are already in longData, so we need to join on those too.
      longData <- left_join(
        longData,
        testData,
        by = c("row_id", "ab_name", "Antimicrobial")
      )
    }
  }
  finalColumns <- finalColumns[finalColumns %in% colnames(longData)]

  longData <- select(longData, all_of(finalColumns))

  # If the data has a `MIC` column it need to be split into sign and value columns.
  if ("MIC" %in% colnames(longData)) {
    longData <- .getMICDataColumns(longData)
  }
  if ("SIR" %in% colnames(longData)) {
    #' Want to clean the interpretation history so that we only have the relevant
    #' interpretations from this file + only SIR columns.
    #' This avoids including interpretations from when checking what test type a column is.
    hist <- AMR::sir_interpretation_history(clean = TRUE)
    longData <- longData %>%
      rename(Interpretation = SIR) %>%
      mutate(Interpretation = AMR::as.sir(Interpretation))
  }
  return(longData)
}

#' TODO: Documentation
#' [Summary]
#'
#' @param spec [Description]
#' @return [Description]
parseColSpec <- function(spec) {
  spec <- gsub("\\s+", "", spec)

  parts <- unlist(strsplit(spec, split = ","))

  all_cols <- numeric(0)

  for (part in parts) {
    if (grepl("-", part)) {
      rng <- strsplit(part, "-")[[1]]

      if (length(rng) == 2) {
        from <- as.numeric(rng[1])
        to <- as.numeric(rng[2])

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
