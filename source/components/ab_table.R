#' Get a classic antibiogram gt table.
#'
#' @param data A data frame containing the antibiogram data.
#' @param obs_cols Numeric vector indicating the columns with observed data.
#' @param drug_targets Numeric vector indicating the columns with drug target data.
#' @param showColors Logical indicating whether to apply color coding.
#' @param table_type Type of table to generate: "percentage", "isolate", or "ci".
#' @param drug_class_starts Numeric vector indicating where drug class separators should be placed.
#' @param ab_cell_width Numeric value for the width of antibiotic columns in pixels.
#' @return A gt table formatted as a classic antibiogram.
classicAB <- function(
  data,
  obs_cols,
  drug_targets,
  showColors = TRUE,
  table_type = "percentage", # Can be "percentage", "isolate", or "ci"
  drug_class_starts = NULL,
  ab_cell_width = 50
) {
  fixed_cols <- 1

  if ("Median n" %in% colnames(data)) {
    fixed_cols <- 1:2
  }

  abs <- colnames(data)[unique(c(obs_cols, drug_targets + 1))]

  abs <- unique(gsub("^obs_", "", abs))
  abs <- abs[!abs %in% c(colnames(data)[fixed_cols])]

  # The target column prefix based on table type.
  # "percentage" tables have no prefix (just ab name).
  column_prefix <- case_when(
    table_type == "isolate" ~ "obs_",
    table_type == "ci" ~ "ci_",
    TRUE ~ ""
  )

  data <- data %>%
    mutate(across(everything(), ~ replace(., is.na(.), "")))

  if (table_type != "percentage") {
    #' Remove percentages columns.
    #' Make sure the the target table type columns are first after fixedCols.
    #' E.g., for table_type = "ci", move ci_<ab> columns next to fixedCols.
    data <- data %>%
      dplyr::select(-dplyr::any_of(abs)) %>%
      dplyr::select(
        any_of(c(colnames(data)[fixed_cols], paste0(column_prefix, abs))),
        everything()
      )
    obs_cols <- which(colnames(data) %in% paste0("obs_", abs))
    drug_targets <- which(colnames(data) %in% paste0(column_prefix, abs)) - 1
  } else {
    # Add percentage signs to the appropriate columns
    data <- data %>%
      add_percentage_to_cols(abs)

    # Create the tooltip for the table displayed on the ab tab
    for (ab in abs) {
      prop_col <- ab
      obs_col <- paste0("obs_", ab)
      ci_col <- paste0("ci_", ab)

      if (all(c(prop_col, obs_col, ci_col) %in% colnames(data))) {
        data[[prop_col]] <- mapply(
          ab_tooltip,
          value = data[[prop_col]],
          obs = data[[obs_col]],
          ci = data[[ci_col]],
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
      }
    }
  }

  plt <- data %>%
    mutate(dplyr::across(dplyr::all_of(fixed_cols), as.character)) %>%
    gt::gt(id = "classic-ab-table")

  data <- data %>%
    mutate(across(everything(), ~ replace(., is.na(.), 0)))

  if (showColors) {
    max_count <- data %>%
      dplyr::select(dplyr::all_of(drug_targets + 1)) %>%
      unlist() %>%
      max(na.rm = TRUE)

    for (ab in abs) {
      prop_col <- paste0(column_prefix, ab)
      colour_col <- paste0("colour_", ab)
      obs_col <- paste0("obs_", ab)

      if (!colour_col %in% colnames(data)) {
        # Add empty colour_col if it doesn't exist to stop errors being thrown below
        data[[colour_col]] <- NULL
      }

      plt <- plt %>%
        gt::data_color(columns = prop_col, fn = function(x) {
          # Use the matching colour column for each cell
          color_vec <- as.character(data[[colour_col]])
          # Replace invalid colors with "transparent"
          color_vec[is.na(color_vec) | color_vec %in% c("NA", "0", "")] <- "transparent"
          color_vec
        })
    }
  }
  # String for searching columns with the target prefix
  search_column_prefix <- paste0("^", column_prefix)

  target_ab_cols <- grep(search_column_prefix, colnames(data), value = TRUE)
  label_map <- setNames(gsub(search_column_prefix, "", target_ab_cols), target_ab_cols)

  hide_columns <- colnames(data)[
    !colnames(data) %in% c(colnames(data)[fixed_cols], paste0(column_prefix, abs))
  ]

  plt <- plt %>%
    gt::cols_hide(columns = hide_columns) %>%
    gt::cols_label(.list = label_map) # Remove prefix from ab column labels

  if (!is.null(drug_class_starts)) {
    width_list <- Map(
      function(col, width) as.formula(paste0("`", col, "` ~ gt::px(", width, ")")),
      target_ab_cols,
      ab_cell_width
    )

    # Add the fixed column widths
    width_list <- c(
      list(gt::matches("^Median n") ~ gt::px(100)),
      list(1 ~ gt::px(180)),
      width_list
    )

    plt <- plt %>%
      #' This adds the `.gt_center` class to the fixed columns and
      #' `.gt_right` to the drug columns.
      #' Easier to style with CSS below with `opt_css`.
      gt::cols_align(
        align = "center",
        columns = fixed_cols
      ) %>%
      gt::cols_align(
        align = "left",
        columns = drug_targets + 1
      ) %>%
      # Styling for all drug column labels
      gt::tab_style(
        style = gt::cell_text(v_align = "middle", weight = "bold"),
        locations = gt::cells_column_labels(columns = drug_targets + 1)
      ) %>%
      # Styling for all column labels
      gt::tab_style(
        style = gt::cell_text(weight = "bold", v_align = "bottom"),
        locations = gt::cells_column_labels(columns = fixed_cols)
      ) %>%
      # Styling for all table cells
      gt::tab_style(
        style = list(
          gt::cell_borders(
            sides = c("left", "right"),
            color = "grey",
            weight = gt::px(1)
          ),
          gt::cell_text(align = "center")
        ),
        locations = gt::cells_body(columns = everything())
      ) %>%
      # Styling for drug class separator lines
      gt::tab_style(
        style = gt::cell_borders(
          sides = "left",
          style = "dashed",
          color = "black",
          weight = gt::px(5)
        ),
        locations = gt::cells_body(columns = unname(drug_class_starts + 1))
      ) %>%
      gt::cols_width(.list = width_list) %>%
      # Rotate drug column labels
      gt::opt_css(
        css = "
            #classic-ab-table .gt_col_heading.gt_left,
            #classic-ab-table .rt-thead .rt-th.rt-align-left {
              writing-mode: vertical-rl;
              transform: scale(-1);
            }
            #classic-ab-table .rt-table,
            #classic-ab-table .rt-thead .rt-th.rt-align-center,
            #classic-ab-table .gt_col_headings {
              border-top-color: white !important;
            }
            #classic-ab-table .rt-thead .rt-th.rt-align-left {
              border-bottom-color: white !important;
              border-top: 2px solid rgb(211, 211, 211);
            }
            /** Make sure fixed columns don't move */
            #classic-ab-table .gt_center,
            #classic-ab-table .rt-align-center {
              position: sticky !important;
              background-color: white;
              z-index: 2;
            }
            #classic-ab-table .rt-thead .rt-align-center .rt-th-inner {
              display: flex;
              align-items: end;
              justify-content: center;
            }
            #classic-ab-table .rt-thead .rt-th.rt-align-left .rt-th-inner {
              display: flex;
              align-items: center;
            }
            #classic-ab-table .rt-tbody .rt-td:first-child.rt-align-center,
            #classic-ab-table .gt_table_body tr td:first-child.gt_center,
            #classic-ab-table .rt-thead .rt-th:first-child.rt-align-center,
            #classic-ab-table .gt_col_headings th:first-child.gt_center {
              left: 0;
            }
            #classic-ab-table .rt-tbody .rt-td:nth-child(2).rt-align-center,
            #classic-ab-table .gt_table_body tr td:nth-child(2).gt_center,
            #classic-ab-table .rt-thead .rt-th:nth-child(2).rt-align-center,
            #classic-ab-table .gt_col_headings th:nth-child(2).gt_center {
              left: 180px;
            }
            #classic-ab-table * {
              font-family: 'Carme', sans-serif;
            }
            #classic-ab-table .rt-thead .rt-th {
              font-weight: bold;
            }
            "
      ) %>%
      gt::opt_interactive(
        use_pagination = FALSE,
        use_pagination_info = FALSE,
        use_sorting = FALSE,
        use_search = FALSE
      )
  }
  return(plt)
}

#' Helper function to create the tooltip for the percentage table in the ab tab.
#' Displays the observed count and confidence interval for each cell.
#'
#' @param value The percentage value to display in the cell.
#' @param obs The observed count for the cell.
#' @param ci The confidence interval for the cell.
#' @return A HTML div element with the tooltip information.
ab_tooltip <- function(value, obs, ci) {
  tags$div(
    title = paste0("n = ", obs, ", CI = ", ci),
    value
  ) %>%
    as.character()
}
