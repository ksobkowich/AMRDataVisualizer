#' UI for the antibiogram tab module.
#'
#' @param id  Module ID.
#' @return    Module UI.
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # Main Content ------------------------------------------------------------

      column(
        9,
        tagList(
          uiOutput(ns("has_data")),
          uiOutput(ns("has_no_data"))
        )
      ),

      # Side menus -----------------------------------------------------------------

      column(
        3,

        # Filters -----------------------------------------------------------------

        filter_panel$ui(ns("filters")),

        # Plot controls -----------------------------------------------------------
        uiOutput(ns("controls")),

        # Legend ------------------------------------------------------------------

        uiOutput(ns("legend"))
      )
    )
  )
}

#' Server logic for the antibiogram tab module.
#'
#' @param id                    Module id.
#' @param reactiveData          Reactive cleaned df (including any custom breakpoints).
#' @param customBreakpoints     Reactive df containing any custom breakpoints.
#' @param mic_or_sir            Reactive value indicating if data is MIC or SIR.
#' @param bp_log                Reactive df containing all breakpoints used in the data.
#' @return                      None.
server <- function(id, reactiveData, customBreakpoints, mic_or_sir, bp_log) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------

    filters <- filter_panel$server(
      "filters",
      reactiveData,
      default_filters = c("Microorganism", "Suppress Antimicrobials", "Species", "Source", "Date"),
      auto_populate = list()
    )

    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------

    # Cell widths for the drug columns in the tables
    AB_PERCENT_CELL_WIDTH <- 65
    AB_ISOLATE_CELL_WIDTH <- 60
    AB_CI_CELL_WIDTH <- 70

    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    plotData <- reactive({
      data <- filters$filteredData()

      if ("Interpretation" %in% colnames(data)) {
        data$Interpretation <- AMR::as.sir(data$Interpretation)
      }
      if (aggByGenus()) {
        unique_mos <- unique(data$Microorganism)
        # Replace each MO with its genus
        for (mo in unique_mos) {
          genus <- mo_genus(mo)
          data$Microorganism[data$Microorganism == mo] <- genus
        }
      }
      return(data)
    })

    activeFilters <- reactive({
      filters$activeFilters()
    })

    showColors <- reactiveVal(TRUE)
    aggByGenus <- reactiveVal(FALSE)
    lowCounts <- reactiveVal("Include")
    yVar <- reactiveVal("Microorganism")
    sortBy <- reactiveVal("Frequency")
    maxRows <- reactiveVal(15)
    splitGram <- reactiveVal(FALSE)
    splitData <- reactiveVal()

    # Group all the inputs in the controls panel into a single reactive
    controls <- reactive({
      return(list(
        yVar = yVar(),
        sortBy = sortBy(),
        lowCounts = lowCounts(), # Handle low counts
        maxRows = maxRows(),
        showColors = showColors(),
        aggByGenus = aggByGenus(),
        splitGram = splitGram()
      ))
    })

    clopper_pearson_ci <- reactive({
      data <- plotData()
      req("Interpretation" %in% colnames(data))
      ab_cols <- as.ab(unique(data$Antimicrobial))

      #' Much faster than using AMR::sir_confidence_interval() but gets the same results
      data <- data %>%
        filter(Interpretation %in% c("S", "I", "R")) %>%
        mutate(
          ab = as.ab(Antimicrobial),
          Interpretation = AMR::as.sir(Interpretation)
        ) %>%
        group_by(!!sym(yVar()), ab) %>%
        summarise(
          Antimicrobial = first(Antimicrobial),
          n_s = sum(Interpretation == "S", na.rm = TRUE),
          n = sum(!is.na(Interpretation)),
          .groups = "drop"
        ) %>%
        mutate(
          ci = map2(
            n_s,
            n,
            ~ if (.y > 0) stats::binom.test(.x, .y)$conf.int else c(NA_real_, NA_real_)
          ),
          ci_min = map_dbl(ci, 1),
          ci_max = map_dbl(ci, 2)
        ) %>%
        select(!!sym(yVar()), Antimicrobial, ci_min, ci_max) %>%
        # For percentages
        # mutate(ci_min = 100 * ci_min, ci_max = 100 * ci_max) %>%
        # mutate(ci = paste0("(", round(ci_min, 0), ", ", round(ci_max, 0), ")"))
        mutate(ci = paste0("(", round(ci_min, 2), ", ", round(ci_max, 2), ")"))

      return(data)
    })

    # First filter the data to include in the plot
    filteredData <- reactive({
      yVar <- yVar()

      data <- plotData() %>%
        filter(Interpretation %in% c("S", "R", "I")) %>%
        mutate(
          Interp = Interpretation,
          Interpretation = ifelse(Interpretation == "S", 1, 0),
          Microorganism = if (aggByGenus()) mo_genus(Microorganism) else Microorganism
        ) %>%
        group_by(!!sym(yVar), Antimicrobial, Class) %>%
        summarise(
          Interp = first(Interp),
          num_susceptible = sum(Interpretation == 1),
          obs = n(),
          prop = round(mean(Interpretation == 1), 3),
          .groups = 'drop'
        ) %>%
        mutate(size = cut(prop, breaks = c(0, 0.7, 0.9, 1), labels = c("s", "m", "l"))) %>%
        {
          if (yVar == "Microorganism") {
            mutate(., short_form = shorten_bacteria_names(.[[yVar]]))
          } else {
            mutate(
              .,
              short_form = ifelse(
                str_length(.[[yVar]]) > 15,
                str_c(str_sub(.[[yVar]], 1, 15), "..."),
                .[[yVar]]
              )
            )
          }
        } %>%
        arrange(Class, Antimicrobial) %>%
        add_ab_cell_colours()
      return(data)
    })

    #' The type of output to generate.
    #'
    #' Similified: plotly plot
    #' Classic: gt table (if gram split then 2 tables)
    outputType <- reactive({
      if (splitGram() == TRUE && yVar() == "Microorganism") {
        return("classic_split")
      }
      return("classic")
    })

    #' A reactive to hold all the items needed to create the plot/tables.
    outputItems <- reactive({
      data <- filteredData()
      if (nrow(data) == 0) {
        return(NULL)
      }
      return(getAntibiogramPlotItems(
        data,
        controls = controls(),
        staticData = reactiveData(),
        show_n_col = TRUE,
        ab_cell_width = AB_PERCENT_CELL_WIDTH,
        clopper_pearson_ci_data = clopper_pearson_ci()
      ))
    })

    #' Holds the first table for the classic AB output.
    #' This table will differ based on if the user selected gram split or not.
    classicAbTable <- reactive({
      req(outputType() %in% c("classic", "classic_split"))
      if (outputType() == "classic") {
        return(outputItems()$table)
      }
      return(outputItems()$negTable)
    })

    #' Holds the first table data for the classic AB output.
    #' This data will differ based on if the user selected gram split or not.
    classicAbTableData <- reactive({
      req(outputType() %in% c("classic", "classic_split"))
      if (outputType() == "classic") {
        return(outputItems()$data)
      }
      return(outputItems()$df_wide_neg)
    })

    #' Holds the second table for the classic AB output when the user
    #' has selected gram split = TRUE.
    classicAbTable2 <- reactive({
      req(outputType() == "classic_split")
      return(outputItems()$posTable)
    })

    #' Holds the second table data for the classic AB output when the user
    #' has selected gram split = TRUE.
    classicAbTableData2 <- reactive({
      req(outputType() == "classic_split")
      return(outputItems()$df_wide_pos)
    })

    # Is there data to plot?
    hasData <- reactive({
      if (is.null(filteredData())) {
        return(FALSE)
      }
      nrow(filteredData()) > 0
    })

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    # Render Legend
    output$legend <- renderUI({
      return(wellPanel(
        h4("Legend", class = "legend-title"),
        h5("Color", class = "legend-section"),
        get_classic_ab_legend(),
        h6("Vertical divisions represent antimicrobial class."),
        h6("Horizontal divisions represent microorganism gram stain."),
        h6("Hover over a cell for more details."),
        class = "legendWell"
      ))
    })

    # Render Controls
    output$controls <- renderUI({
      tagList(
        bsCollapse(
          id = "collapsePanel",
          open = NULL,
          multiple = T,
          bsCollapsePanel(
            HTML(
              "Controls <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
            style='float: right; color: #aaa;'></span>"
            ),
            selectizeInput(
              ns("yVar"),
              "Y-axis variable",
              selected = "Microorganism",
              choices = c("Microorganism", "Source")
            ),

            conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'Microorganism'",
                ns("yVar")
              ),
              selectizeInput(
                ns("sortBy"),
                "Sort by",
                selected = "Frequency",
                choices = c("Alphabetical", "Frequency", "Gram Stain")
              ),
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Source'", ns("yVar")),
              selectizeInput(
                ns("sortBy"),
                "Sort by",
                selected = "Frequency",
                choices = c("Alphabetical", "Frequency")
              ),
            ),
            radioGroupButtons(
              ns("handleLowCount"),
              label = "Handle low-count (<30) results",
              selected = "Include",
              choices = c("Include", "Exclude")
            ),
            numericInput(ns("maxRows"), "Maximum Rows", value = 15, step = 1, min = 1, max = 30),

            materialSwitch(ns("abColors"), label = "Show colors", value = TRUE),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Microorganism'", ns("yVar")),
              materialSwitch(ns("aggGenus"), label = "Aggregate by Genus", value = F)
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Microorganism'", ns("yVar")),
              materialSwitch(ns("splitGram"), label = "Split by Gram Stain", value = F)
            ),

            actionButton(ns("applyControl"), "Apply", class = "submitButton")
          )
        )
      )
    })

    # If there is data, show the plot
    output$has_data <- renderUI({
      req(hasData())

      tagList(
        wellPanel(
          style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh; min-width: 500px;",
          div(
            id = "ab-page-table-wrapper",
            class = "ab-table-wrapper",
            uiOutput(ns("classicAbContainer")), # Classic with no Gram Split
            uiOutput(ns("classicGramSplit")) # Classic with Gram Split
          ),
          class = "contentWell"
        ),
        div(
          class = "align-end-row",
          uiOutput(ns("bp_download")),
          if (mic_or_sir() == "SIR") {
            downloadButton(ns("save_report_html"), "Save Report", class = "plotSaveButton")
          } else {
            downloadButton(ns("save_report_zip"), "Save Report", class = "plotSaveButton")
          },
          downloadButton(ns("save_table"), "Save Data", class = "plotSaveButton")
        )
      )
    })

    output$classicGramSplit <- renderUI({
      req(outputType() == "classic_split")
      return(tagList(
        hr(),
        h4("Gram Positive"),
        fluidRow(
          column(
            align = "center",
            width = 12,
            withSpinner(gt::gt_output(ns("classicAB2")), type = 4, color = "#44CDC4")
          )
        )
      ))
    })

    output$classicAbContainer <- renderUI({
      req(outputType() %in% c("classic", "classic_split"))
      return(tagList(
        if (outputType() == "classic_split") {
          h4("Gram Negative")
        },
        fluidRow(
          column(
            align = "center",
            width = 12,
            withSpinner(gt::gt_output(ns("classicAB")), type = 4, color = "#44CDC4")
          )
        )
      ))
    })

    # Show the custom breakpoint download only if custom breakpoints are present
    output$bp_download <- renderUI({
      req(customBreakpoints())
      req(nrow(customBreakpoints()) > 0)
      downloadButton(ns("save_breakpoints"), "Save Custom Breakpoints", class = "plotSaveButton")
    })

    # If there is no data, show message to user
    output$has_no_data <- renderUI({
      req(!hasData())

      wellPanel(
        style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
        div(
          style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
          div(
            style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
            icon("disease", style = "font-size:100px; color: #44CDC4"),
            h4("Oops... looks like there isn't enough data for this plot."),
            h6(
              "Try reducing the number of filters applied or adjust your data in the 'Import' tab."
            )
          )
        ),
        class = "contentWell"
      )
    })

    # Show a gt table for classic AB (both split and not split)
    output$classicAB <- gt::render_gt({
      req(outputType() %in% c("classic", "classic_split"), !is.null(classicAbTable()))
      classicAbTable()
    })

    # Show a gt table for classic AB gram negative (only split)
    output$classicAB2 <- gt::render_gt({
      req(outputType() == "classic_split", !is.null(classicAbTable2()))
      classicAbTable2()
    })

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------

    #' TODO: Documentation
    #' [Summary]
    #'
    #' @param names Bacteria names to shorten.
    #' @return      Shortened bacteria names.
    shorten_bacteria_names <- function(names) {
      str_replace(
        names,
        pattern = "\\b(\\w)\\w*\\s(\\w+)",
        replacement = "\\1. \\2"
      )
    }

    #' Get the number of visible columns in the table.
    #' This is used to calculate the width of the html table and the
    #' font size for the table text.
    #'
    #' @param tableData The data used to create the table.
    #' @return          The number of visible columns.
    getVisibleCols <- function(tableData) {
      tableData <- tableData %>%
        select(-which(grepl("^obs_", names(tableData))))
      return(ncol(tableData))
    }

    #' Get the width for the html table based on number of columns.
    #' This is used to set the width for webshot when saving the report.
    #'
    #' @param tableData     The data used to create the table.
    #' @param ab_cell_width The width of the antimicrobial cells in the table.
    #' @param has_n_col     Whether the table has the "Median n" column (which is wider than drug columns).
    #' @return              The width in pixels.
    #' @seealso {@link{getVisibleCols}}
    getHtmlTableWidth <- function(tableData, ab_cell_width, has_n_col = FALSE) {
      numCols <- getVisibleCols(tableData)

      #' The fixed columns (first 2) are wider than the rest and have
      #' a fixed width. Calculate the total width based on this.
      fixedColsWidth <- ifelse("Median n" %in% colnames(tableData) || has_n_col, 280, 180)
      vwidth <- fixedColsWidth + (numCols - 2) * (ab_cell_width + 2) + 100
      return(vwidth)
    }

    #' Add custom CSS styling to the html table.
    #' Includes the Carme font from Google Fonts.
    #'
    #' @param tableHtml The html of the table.
    #' @param tableData The data used to create the table.
    #' @return          The html with the CSS added.
    #' @seealso {@link{getVisibleCols}}
    addStylingToHtml <- function(tableHtml, tableData) {
      font_size <- max(12, 16 - 0.3 * getVisibleCols(tableData))

      css <- sprintf(
        '
      <link href="https://fonts.googleapis.com/css2?family=Carme&display=swap" rel="stylesheet">
      <style>
        body, table, td, th {
          font-family: "Carme", sans-serif !important;
          font-size: %dpx !important;
        }
        .dataTables_wrapper {
          overflow-x: visible !important;
        }
        table {
          table-layout: fixed;
        }
      </style>',
        round(font_size)
      )

      return(sub("</head>", paste0(css, "\n</head>"), tableHtml))
    }

    #' Save the visualisation as a png image.
    #'
    #' @param visualisation The visualisation to save (plotly or gt).
    #' @param table_data    The data used to create the visualisation.
    #' @param filename      The filename (without extension).
    #' @param ab_cell_width The width of the antimicrobial cells in the table.
    #' @param has_n_col     Whether the table has the "Median n" column (which is wider than drug columns).
    #' @return              None.
    #' @seealso {@link{getHtmlTableWidth}}, {@link{addStylingToHtml}}
    saveVisualisationPng <- function(
      visualisation,
      table_data,
      filename,
      ab_cell_width,
      has_n_col = FALSE
    ) {
      htmlName <- paste0(filename, ".html")
      pngName <- paste0(filename, ".png")

      htmltools::save_html(visualisation, htmlName)
      html_lines <- readLines(htmlName)

      html_lines <- addStylingToHtml(html_lines, table_data)
      writeLines(html_lines, htmlName)

      pngWidth <- getHtmlTableWidth(table_data, ab_cell_width, has_n_col)

      if (file.exists(pngName)) {
        file.remove(pngName)
      }

      webshot2::webshot(
        url = htmlName,
        file = pngName,
        vwidth = pngWidth,
        zoom = 2 # For higher resolution
      )
    }

    #' Download the Antibiogram report and an HTML file.
    #'
    #' Used in both download handlers (SIR - just HTML and MIC - ZIP with HTML and XLSX).
    #' Saves the report as an HTLM file in the `tmp` directory as `Antibiogram.html`.
    #'
    #' @param tmp   Temporary directory to use for report generation.
    #' @return      None.
    download_html_report <- function(tmp) {
      src <- normalizePath(".")
      # tmp <- tempdir()
      unlink(list.files(tmp, full.names = TRUE), recursive = TRUE, force = TRUE)

      owd <- setwd(tmp)
      on.exit({
        setwd(owd)
        unlink(
          c(
            "antibiogram_table.html",
            "antibiogram_table2.html",
            "antibiogram_table.png",
            "antibiogram_table2.png",
            "Antibiogram.qmd",
            "report.css",
            "logo.png",
            "report-bp-example.png",
            "cornell-reduced-wordmark-red.png",
            "OVC__GUELPH_FULLCOLOUR_WHITEBG_V1_1.5IN_H_300PPI.png",
            "epi-logo.png",                
            "usda.png",                             
            "isolate_table.html",
            "isolate_table2.html",
            "isolate_table.png",
            "isolate_table2.png",
            "ci_table.html",
            "ci_table2.html",
            "ci_table.png",
            "ci_table2.png",
            "low_counts_flag.txt",
            "ui_utils.R",
            "report_utils.R",
            "general_utils.R"
          ),
          recursive = TRUE
        )
      })
      normalizedReport <- normalizePath(file.path(src, "Reports", "Antibiogram.qmd"))
      stylesPath <- normalizePath(file.path(src, "www", "css", "report.css"))
      logoPath <- normalizePath(file.path(src, "www", "img", "logoDark.png"))
      bpImgPath <- normalizePath(file.path(src, "www", "img", "report-bp-example.png"))
      
      cornellLogoPath <- normalizePath(file.path(src, "www", "img", "cornell-reduced-wordmark-red.png"))
      guelphLogoPath <- normalizePath(file.path(src, "www", "img", "OVC__GUELPH_FULLCOLOUR_WHITEBG_V1_1.5IN_H_300PPI.png"))
      epiLogoPath <- normalizePath(file.path(src, "www", "img", "epi-logo.png"))
      usdaLogoPath <- normalizePath(file.path(src, "www", "img", "usda.png"))

      uiUtilsPath <- normalizePath(file.path(src, "source", "utils", "ui_utils.R"))
      reportUtilsPath <- normalizePath(file.path(src, "source", "utils", "report_utils.R"))
      generalUtilsPath <- normalizePath(file.path(src, "source", "utils", "utils.R"))

      file.copy(normalizedReport, "Antibiogram.qmd", overwrite = TRUE)
      file.copy(stylesPath, "report.css", overwrite = TRUE)
      file.copy(logoPath, "logo.png", overwrite = TRUE)
      file.copy(bpImgPath, "report-bp-example.png", overwrite = TRUE)

      file.copy(cornellLogoPath, "cornell-reduced-wordmark-red.png", overwrite = TRUE)
      file.copy(guelphLogoPath, "OVC__GUELPH_FULLCOLOUR_WHITEBG_V1_1.5IN_H_300PPI.png", overwrite = TRUE)
      file.copy(epiLogoPath, "epi-logo.png", overwrite = TRUE)
      file.copy(usdaLogoPath, "usda.png", overwrite = TRUE)

      file.copy(uiUtilsPath, "ui_utils.R", overwrite = TRUE)
      file.copy(reportUtilsPath, "report_utils.R", overwrite = TRUE)
      file.copy(generalUtilsPath, "general_utils.R", overwrite = TRUE)

      filtered_data <- filteredData()
      current_controls <- controls()
      reactive_data <- reactiveData()

      currentVisualisation <- classicAbTable()

      ab_table_data <- currentVisualisation$`_data` %>%
        select(any_of(c(yVar(), unique(filtered_data$Antimicrobial))))

      saveVisualisationPng(
        currentVisualisation,
        ab_table_data,
        "antibiogram_table",
        ab_cell_width = AB_PERCENT_CELL_WIDTH,
        has_n_col = TRUE
      )

      # Number of Isolates Images
      isolate_items <- getAntibiogramPlotItems(
        plotData = filtered_data,
        controls = current_controls,
        staticData = reactive_data,
        table_type = "isolate",
        ab_cell_width = AB_ISOLATE_CELL_WIDTH
      )
      # Get the table data for image width calculation
      isolate_table_data <- if (splitGram()) {
        isolate_items$df_wide_neg
      } else {
        isolate_items$data
      }
      isolate_table_data <- isolate_table_data %>%
        select(any_of(c(yVar(), "Median n", unique(filtered_data$Antimicrobial))))

      # Number of Isolates Images
      ci_items <- getAntibiogramPlotItems(
        plotData = filtered_data,
        controls = current_controls,
        staticData = reactive_data,
        clopper_pearson_ci_data = clopper_pearson_ci(),
        table_type = "ci",
        ab_cell_width = AB_CI_CELL_WIDTH
      )
      # Get the table data for image width calculation
      ci_table_data <- if (splitGram()) {
        ci_items$df_wide_neg
      } else {
        ci_items$data
      }
      ci_table_data <- ci_table_data %>%
        select(any_of(c(yVar(), unique(filtered_data$Antimicrobial))))

      if (splitGram()) {
        visualisation2 <- classicAbTable2()
        saveVisualisationPng(
          visualisation2,
          ab_table_data,
          "antibiogram_table2",
          ab_cell_width = AB_PERCENT_CELL_WIDTH,
          has_n_col = TRUE
        )

        # Number of Isolates Images
        saveVisualisationPng(
          isolate_items$negTable,
          isolate_table_data,
          "isolate_table",
          ab_cell_width = AB_ISOLATE_CELL_WIDTH
        )
        saveVisualisationPng(
          isolate_items$posTable,
          isolate_table_data,
          "isolate_table2",
          ab_cell_width = AB_ISOLATE_CELL_WIDTH
        )

        # Clopper Pearson CI Images
        saveVisualisationPng(
          ci_items$negTable,
          ci_table_data,
          "ci_table",
          ab_cell_width = AB_CI_CELL_WIDTH
        )
        saveVisualisationPng(
          ci_items$posTable,
          ci_table_data,
          "ci_table2",
          ab_cell_width = AB_CI_CELL_WIDTH
        )
      } else {
        saveVisualisationPng(
          isolate_items$table,
          isolate_table_data,
          "isolate_table",
          ab_cell_width = AB_ISOLATE_CELL_WIDTH
        )
        saveVisualisationPng(
          ci_items$table,
          ci_table_data,
          "ci_table",
          ab_cell_width = AB_CI_CELL_WIDTH
        )
      }

      ab_table_data <- ab_table_data %>%
        mutate(dplyr::across(dplyr::all_of(colnames(ab_table_data)), as.character))

      filters <- activeFilters()

      writeLines(lowCounts(), "low_counts_flag.txt")

      plot_data <- plotData()
      custom_breakpoints <- customBreakpoints()

      all_filter_values <- list(
        Species = sort(unique(plot_data$Species)),
        Microorganism = sort(unique(plot_data$Microorganism)),
        Source = sort(unique(plot_data$Source))
      )

      filters$guideline <- reactive_data$Guideline[1]
      if (mic_or_sir() == "SIR") {
        filters$guideline <- "Not applicable - Interpretations Provided"
      }
      if (!is.null(custom_breakpoints) && nrow(custom_breakpoints) > 0) {
        filters$guideline <- "Custom"
      }
      filters <- lapply(filters, as.character)

      quarto::quarto_render(
        input = "Antibiogram.qmd",
        output_format = "html",
        output_file = "Antibiogram.html",
        execute_params = list(
          vwidth = getHtmlTableWidth(ab_table_data, AB_CI_CELL_WIDTH),
          filters = filters,
          allFilterValues = all_filter_values,
          tableRows = length(unique(filtered_data[[yVar()]])),
          controls = current_controls
        )
      )
    }

    #' Get the custom breakpoints data for download.
    #'
    #' Want the downloaded format to be in the same format that is needed for
    #' custom breakpoints uploading.
    #'
    #' @param include_all_used  Whether to include all breakpoints used (or just the custom ones).
    #' @return                  A data frame with the breakpoints to download.
    get_custom_bp_download_data <- function(include_all_used = TRUE) {
      custom_breakpoints <- customBreakpoints()
      download_cols <- c(
        "guideline",
        "type",
        "method",
        "site",
        "mo",
        "ab",
        "host",
        "ref_tbl",
        "breakpoint_S",
        "breakpoint_R",
        "uti"
      )

      if (!include_all_used) {
        if (is.null(custom_breakpoints) || nrow(custom_breakpoints) == 0) {
          return(data.frame())
        }
        download_cols <- download_cols[!download_cols %in% c("guideline", "ref_tbl")]
        custom_breakpoints <- custom_breakpoints %>%
          select(all_of(download_cols))
        return(custom_breakpoints)
      }

      #' Get the bp log from processing.
      #' Here we are trying to match the `AMR::clinical_breakpoints` format
      #' so that users can easily re-import them if needed.
      used_bps <- bp_log() %>%
        # Split the `Breakpoint (S-R)` column into S and R columns
        separate(`Breakpoint (S-R)`, into = c("breakpoint_S", "breakpoint_R"), sep = "-") %>%
        select(
          guideline = Guideline,
          type,
          method,
          site,
          mo = `MO Used`,
          ab = `AB Used`,
          host = `Host Used`,
          ref_tbl = Reference,
          breakpoint_S,
          breakpoint_R,
          uti = UTI
        ) %>%
        distinct()

      # Override any custom breakpoints used
      if (nrow(custom_breakpoints) > 0) {
        #' For each custom breakpoint, replace it's default values with the custom ones.
        for (i in 1:nrow(custom_breakpoints)) {
          row <- custom_breakpoints[i, ]
          new_bp <- row %>%
            select(all_of(download_cols))

          #' First filter out any existing breakpoints that match the custom one.
          #' Then add the custom one.
          used_bps <- used_bps %>%
            filter(
              !((is.na(uti) & is.na(row$uti) | uti == row$uti) &
                (is.na(site) & is.na(row$site) | site == row$site) &
                (is.na(method) & is.na(row$method) | method == row$method) &
                (is.na(host) & is.na(row$host) | host == row$host) &
                (is.na(type) & is.na(row$type) | type == row$type) &
                (is.na(mo) & is.na(row$mo) | mo == row$mo) &
                (is.na(ab) & is.na(row$ab) | ab == row$ab))
            ) %>%
            rbind(new_bp)
        }
      }
      return(used_bps)
    }

    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    observeEvent(input$applyControl, {
      showColors(input$abColors)
      aggByGenus(input$aggGenus)
      lowCounts(input$handleLowCount)
      yVar(input$yVar)
      sortBy(input$sortBy)
      maxRows(input$maxRows)
      splitGram(input$splitGram)
    })

    # ------------------------------------------------------------------------------
    # Download Handlers
    # ------------------------------------------------------------------------------

    # Save all custom breakpoints used in the app
    output$save_breakpoints <- downloadHandler(
      filename = function() {
        paste("Custom_Breakpoints.csv")
      },
      content = function(file) {
        write.csv(get_custom_bp_download_data(include_all_used = FALSE), file, row.names = FALSE)
      }
    )

    # Save HTML Report (data was processed as SIR) ----------------------------------
    output$save_report_html <- downloadHandler(
      filename = paste0("Antibiogram.html"),

      content = function(file) {
        withProgress(message = 'Rendering, please wait!', {
          tmp <- tempdir()
          download_html_report(tmp)
          file.rename("Antibiogram.html", file)
          tmp_html <- file.path(tmp, "Antibiogram.html")
          if (file.exists(tmp_html)) {
            file.copy(tmp_html, file, overwrite = TRUE)
          } else {
            stop("Antibiogram.html was not created.")
          }
        })
      }
    )

    # Save HTML Report + xlsx with used breakpoints as a ZIP (data was processed as MIC) -------------------------------------------------------------
    output$save_report_zip <- downloadHandler(
      filename = paste0("Antibiogram.zip"),

      content = function(file) {
        withProgress(message = 'Rendering, please wait!', {
          tmp <- tempdir()
          download_html_report(tmp)

          tmp_html <- file.path(tmp, "Antibiogram.html")
          tmp_xlsx <- file.path(tmp, "Breakpoints_Used.xlsx")

          #' We want to ZIP the report and breakpoints used.
          openxlsx::write.xlsx(
            get_custom_bp_download_data(),
            file = tmp_xlsx,
            asTable = TRUE,
            overwrite = TRUE
          )

          zip::zip(
            zipfile = file,
            files = c(tmp_html, tmp_xlsx),
            mode = "cherry-pick"
          )
        })
      }
    )

    #' Prepare the data for download in the Excel file.
    #' Removes the colour and obs columns and adds percentage values to the antimicrobial columns.
    #'
    #' @param data The data to prepare.
    #' @return     The prepared data.
    prepare_percentage_data <- function(data) {
      fixed_cols <- c(yVar(), "Median n")
      drug_cols <- colnames(data)
      drug_cols <- drug_cols[
        !grepl("obs_", drug_cols) &
          !grepl("colour_", drug_cols) &
          !grepl("ci_", drug_cols) &
          !drug_cols %in% fixed_cols
      ]

      data %>%
        select(-starts_with("colour_"), -starts_with("obs_"), , -starts_with("ci_")) |>
        add_percentage_to_cols(drug_cols)
    }

    #' Prepare the data for download in the Excel file.
    #' Selects only the columns with the number of isolates and renames them to match the
    #' antimicrobial columns.
    #'
    #' @param data The data to prepare.
    #' @return     The prepared data.
    prepare_isolate_data <- function(data) {
      data %>%
        select(1, starts_with("obs_")) %>%
        rename_with(~ str_replace(., "obs_", ""))
    }

    # Save Data ---------------------------------------------------------------
    output$save_table <- downloadHandler(
      filename = function() {
        paste("Antibiogram.xlsx")
      },
      content = function(file) {
        sheets <- list()

        if (outputType() == "classic") {
          full_df <- classicAbTableData()

          sheets[["Antibiogram"]] <- prepare_percentage_data(full_df)

          sample_size_df <- prepare_isolate_data(full_df)
          if (ncol(sample_size_df) > 1) {
            sheets[["Sample Size"]] <- sample_size_df
          }
        } else if (outputType() == "classic_split") {
          full_df_neg <- classicAbTableData()
          full_df_pos <- classicAbTableData2()

          sheets[["Gram Negative"]] <- prepare_percentage_data(full_df_neg)
          sheets[["Gram Positive"]] <- prepare_percentage_data(full_df_pos)

          sample_size_neg <- prepare_isolate_data(full_df_neg)
          if (ncol(sample_size_neg) > 1) {
            sheets[["Gram Negative Sample Size"]] <- sample_size_neg
          }

          sample_size_pos <- prepare_isolate_data(full_df_pos)
          if (ncol(sample_size_pos) > 1) {
            sheets[["Gram Positive Sample Size"]] <- sample_size_pos
          }
        }

        ci_df <- clopper_pearson_ci()
        if (ncol(ci_df) > 0) {
          sheets[["Confidence Intervals"]] <- ci_df
        }

        writexl::write_xlsx(sheets, path = file)
      }
    )
  })
}

antibiogram_tab <- list(
  ui = ui,
  server = server
)
