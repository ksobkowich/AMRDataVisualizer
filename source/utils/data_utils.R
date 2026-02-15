#' This file contains utility functions for data manipulation and analysis.
#'
#' @keywords internal
NULL


#' Find UTI matches in the sources.
#'
#' @param sources The sources to check for UTI matches.
#' @returns       A logical vector indicating whether each source matches UTI criteria.
findUtiMatch <- function(sources) {
  str_detect(tolower(sources), "urin|ureth|freecatch|cysto")
}

#' Retrieve full clinical breakpoints with antibiotic and microorganism names.
#'
#' @return A data frame of clinical breakpoints with added antibiotic and microorganism names.
getFullClinicalBps <- function() {
  .ab_mapping <- data.frame(ab = unique(AMR::clinical_breakpoints$ab)) %>%
    mutate(ab_name = AMR::ab_name(ab))

  .mo_mapping <- data.frame(mo = unique(AMR::clinical_breakpoints$mo)) %>%
    mutate(mo_name = AMR::mo_name(mo))

  #' `AMR::clinical_breakpoints` have the `ab` and `mo` columns as abbreviations.
  #' Add the actual names for easier searching in the table.
  full_bps <- AMR::clinical_breakpoints %>%
    left_join(.ab_mapping, by = "ab") %>%
    left_join(.mo_mapping, by = "mo")
  return(full_bps)
}

#' Retrieve clinical breakpoints with optional filtering.
#'
#' @param full_clinical_bps A data frame of clinical breakpoints, typically obtained from `getFullClinicalBps()`.
#' @param filters           A named list of filters to apply to the clinical breakpoints data.
#' @return                  A data frame of clinical breakpoints after applying the filters.
#' @examples
#' # Get all clinical breakpoints for E. coli
#' get_clinical_bps(getFullClinicalBps(), filters = list(mo_name = "Escherichia coli"))
#' # Get clinical breakpoints with multiple filters
#' get_clinical_bps(getFullClinicalBps(), filters = list(mo_name = "Staphylococcus aureus", ab = c("AMX", "CIP")))
#' @seealso AMR::clinical_breakpoints, AMR::as.ab, AMR::as.mo
get_clinical_bps <- function(full_clinical_bps, filters = list()) {
  data <- full_clinical_bps

  for (filter in names(filters)) {
    if (filter %in% colnames(data) && !is.null(filters[[filter]])) {
      values <- unique(filters[[filter]])

      # Convert to appropriate class if needed
      if (filter == "ab" && !inherits(values, "ab")) {
        values <- AMR::as.ab(values)
      }
      if (filter == "mo" && !inherits(values, "mo")) {
        values <- AMR::as.mo(values)
      }

      data <- data %>%
        filter(!!sym(filter) %in% values)
    }
  }
  data <- data %>%
    select(-ab, -mo) %>%
    rename(ab = ab_name, mo = mo_name) %>%
    select(all_of(colnames(AMR::clinical_breakpoints))) # Get the original column order
  return(data)
}

#' Add percentage signs to specified columns in a data frame.
#'
#' @param data  The data frame to modify.
#' @param cols  A character vector of column names to which percentage signs should be added
#' @return      The modified data frame with percentage signs added to the specified columns.
add_percentage_to_cols <- function(data, cols) {
  data %>%
    mutate(across(dplyr::any_of(cols), ~ ifelse(is.na(.) | . == "NA", ., paste0(., "%"))))
}
