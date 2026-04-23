#' This file contains utility functions for plots.
#'
#' @keywords internal
NULL

#' Get items for the antibiogram plot/table.
#'
#' Create the table (for classic antibiogram).
#' If the `splitGram` control is TRUE and the yVar is "Microorganism", two tables are
#' returned: one for Gram-positive and one for Gram-negative microorganisms.
#'
#' @param plotData                Data frame containing the data for the plot/table.
#' @param controls                List of control parameters for the plot/table.
#' @param staticData              Optional static data frame for additional information.
#' @param table_type              Type of table to create: "percentage", "isolate", or "ci".
#' @param show_n_col              Logical indicating whether to include the "Median n" column in the table.
#' @param clopper_pearson_ci_data Optional data frame containing Clopper-Pearson confidence intervals.
#' @return                        A list containing the plot and/or table items for the antibiogram.
getAntibiogramPlotItems <- function(
  plotData,
  controls = list(
    yVar = "Microorganism",
    sortBy = "Frequency",
    lowCounts = "Include",
    maxRows = 15,
    showColors = TRUE,
    aggByGenus = FALSE,
    splitGram = FALSE
  ),
  staticData = NULL,
  table_type = "percentage",
  show_n_col = FALSE,
  clopper_pearson_ci_data = NULL,
  ab_cell_width = 50
) {
  if (is.null(plotData) || nrow(plotData) == 0) {
    return(NULL)
  }

  # If the CP CI data is provided, join it to the main plot data.
  if (!is.null(clopper_pearson_ci_data)) {
    plotData <- plotData %>%
      left_join(
        clopper_pearson_ci_data,
        by = c(controls$yVar, "Antimicrobial")
      )
  }

  # Apply low count filtering BEFORE maxRows limiting
  if (controls$lowCounts == "Exclude") {
    plotData <- plotData %>%
      filter(obs >= 30)
  }

  # Only keep the top `maxRows` most frequent microorganisms/source if needed.
  if (controls$maxRows < length(unique(plotData[[controls$yVar]]))) {
    
    # Get top N organisms/sources by total observations
    top_y_vars <- plotData %>%
      group_by(!!sym(controls$yVar)) %>%
      summarise(total_obs = sum(obs), .groups = 'drop') %>%
      {
        if (controls$sortBy == "Frequency") {
          arrange(., desc(total_obs))
        } else if (controls$sortBy == "Alphabetical") {
          arrange(., !!sym(controls$yVar))
        } else if (controls$sortBy == "Gram Stain" && controls$yVar == "Microorganism") {
          mutate(., gram_stain = mo_gramstain(!!sym(controls$yVar))) %>%
          arrange(gram_stain, desc(total_obs))
        } else {
          arrange(., desc(total_obs))
        }
      } %>%
      slice_head(n = controls$maxRows) %>%
      pull(!!sym(controls$yVar))
    
    # Filter data to only include the selected organisms/sources
    plotData <- plotData %>%
      filter(!!sym(controls$yVar) %in% top_y_vars)
  }

  # If there is no colour column, add it based on prop and obs.
  if (!"colour" %in% colnames(plotData)) {
    plotData <- add_ab_cell_colours(plotData)
  }

  df_wide <- plotData %>%
    select(any_of(c(
      controls$yVar,
      "Antimicrobial",
      "prop",
      "obs",
      "colour",
      "num_susceptible",
      "ci"
    ))) %>%
    mutate(prop = round(prop * 100, 0)) %>%
    pivot_wider(
      id_cols = !!sym(controls$yVar),
      names_from = Antimicrobial,
      values_from = any_of(c("prop", "obs", "colour", "ci")),
      names_sep = "_"
    )

  if (show_n_col) {
    #' Want the "Median n" column for both "isolate" and "percentage" table types.
    #' For "percentage" we want it to show in the app, but it needs to be removed
    #' later in the report download.
    #' For "isolate" we want it to show always.
    df_wide <- df_wide %>%
      rowwise() %>%
      mutate(
        `Median n` = round(median(c_across(starts_with("obs_")), na.rm = TRUE), 0)
      ) %>%
      ungroup() %>%
      select(!!sym(controls$yVar), `Median n`, everything())
  } else {
    df_wide <- df_wide %>%
      ungroup() %>%
      select(!!sym(controls$yVar), everything())
  }

  # If having the "Median n" column there are 2 fixed column, otherwise only 1 fixed column.
  drug_start <- ifelse(show_n_col, 2, 1)

  n_drug <- length(unique(plotData$Antimicrobial))

  colnames(df_wide) <- gsub("prop_", "", colnames(df_wide))

  drug_names <- names(df_wide)[(drug_start + 1):(n_drug + drug_start)]
  drug_classes <- AMR::ab_group(drug_names)
  drug_group_list <- split(seq_along(drug_names), drug_classes)
  drug_class_starts <- sapply(drug_group_list, function(x) min(x))

  if (drug_start == 2) {
    #' We are including the "Median n" column.
    #' So we need to shift the indices by 1.
    drug_class_starts <- drug_class_starts + 1
  }

  obs_cols <- which(grepl("obs_", names(df_wide)))

  drug_targets <- drug_start:(n_drug + drug_start)

  df_wide <- switch(
    controls$sortBy,
    "Frequency" = df_wide %>%
      rowwise() %>%
      mutate(total_obs = sum(c_across(starts_with("obs_")), na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(total_obs)) %>%
      select(-total_obs),
    "Alphabetical" = df_wide %>% arrange(!!sym(controls$yVar)),
    "GramStain" = df_wide %>%
      mutate(gram = AMR::mo_gramstain(controls$yVar)) %>%
      arrange(gram, !!sym(controls$yVar)),
    df_wide
  )

  if (controls$splitGram == TRUE && controls$yVar == "Microorganism") {
    df_wide_neg <- df_wide %>%
      mutate(gram = AMR::mo_gramstain(Microorganism)) %>%
      filter(gram == "Gram-negative") %>%
      select(-gram)

    df_wide_pos <- df_wide %>%
      mutate(gram = AMR::mo_gramstain(Microorganism)) %>%
      filter(gram == "Gram-positive") %>%
      select(-gram)

    negTable <- classicAB(
      data = df_wide_neg,
      obs_cols = obs_cols,
      drug_targets = drug_targets,
      showColors = controls$showColors,
      drug_class_starts = drug_class_starts,
      table_type = table_type,
      ab_cell_width = ab_cell_width
    )

    posTable <- classicAB(
      data = df_wide_pos,
      obs_cols = obs_cols,
      drug_targets = drug_targets,
      showColors = controls$showColors,
      drug_class_starts = drug_class_starts,
      table_type = table_type,
      ab_cell_width = ab_cell_width
    )

    return(list(
      negTable = negTable,
      posTable = posTable,
      df_wide_neg = df_wide_neg,
      df_wide_pos = df_wide_pos
    ))
  } else {
    plot <- classicAB(
      data = df_wide,
      obs_cols = obs_cols,
      drug_targets = drug_targets,
      showColors = controls$showColors,
      drug_class_starts = drug_class_starts,
      table_type = table_type,
      ab_cell_width = ab_cell_width
    )

    return(list(
      data = df_wide,
      table = plot
    ))
  }
}

#' Add cell colours to antibiogram data based on proportions and observations.
#'
#' @param data  Data frame containing 'prop' and 'obs' columns.
#' @return      Data frame with an additional 'colour' column.
add_ab_cell_colours <- function(data) {
  if (is.null(data)) {
    return(data)
  }
  data <- data %>%
    mutate(
      colour = case_when(
        obs < 30 ~ "white",
        prop < 0.7 ~ "#D73027",
        prop >= 0.7 & prop <= 0.9 ~ "#FEE08B",
        prop > 0.9 ~ "#44CDC4",
        TRUE ~ "white"
      )
    )
  return(data)
}
