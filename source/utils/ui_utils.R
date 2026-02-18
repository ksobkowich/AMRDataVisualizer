#' This file contains UI utility functions.
#'
#' @keywords internal
NULL

#' Get the label for breakpoints used in the app.
#'
#' @param bp_s      The S breakpoint value.
#' @param bp_r      The R breakpoint value.
#' @param guideline The guideline name.
#' @return          A string containing the formatted label for breakpoints.
getBpLabel <- function(bp_s, bp_r, guideline) {
  miss <- function(x) is.null(x) || length(x) == 0 || all(is.na(x))

  gl <- if (miss(guideline)) "Custom" else as.character(guideline)[1]

  if (miss(bp_s) || miss(bp_r)) {
    return(paste0("No MIC breakpoints available (", gl, ")."))
  }

  paste0(
    "Breakpoints applied: S \u2264 ",
    as.numeric(bp_s)[1],
    ", R \u2265 ",
    as.numeric(bp_r)[1],
    " (",
    gl,
    ")"
  )
}


#' Get a message displaying the breakpoints.
#'
#' @param breakpoint The breakpoint data frame containing S and R values.
#' @param isCustom   Boolean indicating if the breakpoints are custom values.
#' @return            A `div` containing the message with breakpoint information.
getBreakpointsMessage <- function(bp_row, isCustom = FALSE) {
  miss <- function(x) is.null(x) || length(x) == 0 || all(is.na(x))

  if (is.null(bp_row) || (is.data.frame(bp_row) && nrow(bp_row) == 0)) {
    s <- NA_real_
    r <- NA_real_
    gl <- "User selected breakpoint"
  } else {
    s <- suppressWarnings(as.numeric(bp_row$breakpoint_S[1]))
    r <- suppressWarnings(as.numeric(bp_row$breakpoint_R[1]))
    gl <- bp_row$guideline
    if (miss(gl)) gl <- "User selected breakpoint" else gl <- as.character(gl)[1]
  }

  lbl <- getBpLabel(s, r, gl)

  htmltools::div(
    htmltools::strong(lbl),
    if (isTRUE(isCustom)) htmltools::tags$em(" Custom breakpoints in use.")
  )
}


#' Items for the classic antibiogram legend.
.classic_legend_items <- list(
  list(
    icon_class = "far fa-square",
    colour = "grey",
    label = "Too few observations (<30)"
  ),
  list(
    icon_class = "fas fa-solid fa-square",
    colour = "var(--red)",
    label = "Low susceptibility (<70%)"
  ),
  list(
    icon_class = "fas fa-solid fa-square",
    colour = "var(--yellow)",
    label = "Moderate susceptibility (70 - 90%)"
  ),
  list(
    icon_class = "fas fa-solid fa-square",
    colour = "var(--primary-blue)",
    label = "High susceptibility (>90%)"
  )
)

#' Helper to create a classic legend item.
#'
#' @param icon_class  CSS class for the legend icon.
#' @param label       Label text for the legend item.
#' @param colour      Colour for the legend icon.
#' @return            A `div` element representing the legend item.
.get_classic_legend_item <- function(icon_class, label, colour) {
  return(div(
    class = "legend-item",
    tags$i(
      class = icon_class,
      style = sprintf("font-size: 20px; margin-left: 5px; color: %s;", colour)
    ),
    span(label, class = "legend-label", style = "margin-left: 15px;")
  ))
}

#' Get the classic antibiogram legend.
#'
#' @return A `div` element containing the classic legend.
get_classic_ab_legend <- function() {
  return(div(
    class = "legend-section classic-legend",
    lapply(.classic_legend_items, function(item) {
      .get_classic_legend_item(item$icon_class, item$label, item$colour)
    })
  ))
}

#' Generate a clinical breakpoints data table.
#'
#' Used on the MIC Tables page and in the Antibiogram quarto report.
#'
#' @param data A data frame containing clinical breakpoints information.
#' @return     A DT datatable object displaying the clinical breakpoints.
get_clinical_bps_table <- function(data) {
  table_data <- data %>%
    select(-any_of(c("method", "rank_index", "disk_dose", "is_SDD"))) %>%
    rename_with(~ str_replace_all(., "_", " ")) %>%
    rename_with(~ str_to_title(.)) %>%
    rename_with(
      ~ str_replace_all(
        .,
        c(
          "\\bMo\\b" = "Organism",
          "\\bAb\\b" = "Antimicrobial",
          "\\bUti\\b" = "UTI",
          "\\bRef Tbl\\b" = "Reference Table",
          "\\bBreakpoint S\\b" = "Breakpoint (S)",
          "\\bBreakpoint R\\b" = "Breakpoint (R)"
        )
      )
    )
  return(DT::datatable(
    table_data,
    rownames = FALSE,
    filter = "top",
    options = list(autoWidth = TRUE)
  ))
}
