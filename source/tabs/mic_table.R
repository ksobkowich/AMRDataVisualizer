#' UI for the MIC table tab module.
#'
#' @param id  Module ID.
#' @return    Module UI.
ui <- function(id, data) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Main Content ------------------------------------------------------------

      column(9, uiOutput(ns("content"))),

      # Filters -----------------------------------------------------------------

      column(
        3,
        wellPanel(
          h4("Filters"),
          uiOutput(ns("filters")),
          hr(),
          selectizeInput(
            ns("groupingVar"),
            label = "Grouping Variable",
            choices = c("Month", "Region", "Source", "Species", "Subregion", "Year"),
            selected = "Year",
            multiple = F
          ),
          br(),
          uiOutput(ns("customBreakpointsUI")),
          class = "contentWell"
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(
          id = "custom-bp-upload-container",
          fileInput(
            ns("custom_bp_upload"),
            label = "Upload Custom Breakpoints (csv/xlsx)",
            accept = c(".csv", ".xlsx")
          ),
          actionButton(ns("file_info"), icon("circle-info"), class = "info")
        )
      )
    ),
    fluidRow(
      column(
        12,
        bsCollapse(
          id = ns("clinicalBpTableContainer"),
          open = NULL,
          bsCollapsePanel(
            title = actionButton(
              ns("clinicalBpTableToggle"),
              label = HTML(
                "AMR Clinical Breakpoints Lookup Table <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
          style='float: right; color: #aaa;'></span>"
              ),
              class = "collapse-btn"
            ),
            value = "bpTable",
            DT::dataTableOutput(ns("clinicalBpTable"), width = "100%")
          )
        )
      )
    )
  )
}

#' Server logic for the MIC table tab module.
#'
#' @param id                  The ID of the module.
#' @param reactiveData        A reactive that returns the cleaned data to be explored.
#' @param processedGuideline  A reactive that returns the guideline selected in data processing.
#' @param bp_log              A reactive that returns the breakpoint log data.
#' @return                    List of reactives relating to custom breakpoints and data with custom breakpoints applied.
server <- function(id, reactiveData, processedGuideline, bp_log) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------

    # A custom guideline name to use in the clinical breakpoints df.
    customBreakpointName <- getCustomGuidelineName()

    # The expected format of an empty clinical breakpoints df.
    emptyCustomBp <- AMR::clinical_breakpoints[0, ] %>%
      #' These columns will match the values found in the filtered data
      #' rather than the clinical breakpoints.
      #' Saves us from mutating over the whole filtered data df to format
      #' the values as expected for the custom breakpoints.
      mutate(
        Species = character(0),
        Source = character(0),
        Microorganism = character(0),
        Antimicrobial = character(0)
      )

    #' Expected format of uploaded custom breakpoints.
    expected_format_df <- data.frame(
      type = c("animal", "human"),
      host = c("canine", "human"),
      mo = c("Escherichia coli", "B_ESCHR_COLI"),
      ab = c("Amoxicillin-clavulanic acid", "AMX"),
      breakpoint_S = c(8, 16),
      breakpoint_R = c(32, 32),
      uti = c(TRUE, TRUE)
    )

    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    #' Keep track of whether the clinical breakpoints table is open or closed.
    bpTableOpen <- reactiveVal(FALSE)

    #' A df to hold all the custom breakpoints created by the user.
    #' This is returned as used in the rest of the app.
    #'
    #' Starts off with just the expected columns but no rows.
    allCustomBreakpoints <- reactiveVal(emptyCustomBp)

    # Input values for custom breakpoints
    breakpointInputs <- reactive({
      # req(input$addCustomBreakpoints, input$deleteCustomBreakpoints)
      req(input$moFilter, input$abFilter, input$typeFilter)
      bp <- list(s = NULL, r = NULL)
      if (!is.null(input$customSBreakpoint) && !is.na(input$customSBreakpoint)) {
        bp$s <- as.numeric(input$customSBreakpoint)
      }
      if (!is.null(input$customRBreakpoint) && !is.na(input$customRBreakpoint)) {
        bp$r <- as.numeric(input$customRBreakpoint)
      }
      bp
    }) %>%
      bindEvent(
        input$addCustomBreakpoints,
        input$deleteCustomBreakpoints,
        input$moFilter,
        input$abFilter,
        input$typeFilter
      )

    # Boolean. Require that both breakpoint inputs are set to create custom breakpoints
    canCreateCustomBreakpoints <- reactive({
      all(!is.null(c(input$customSBreakpoint, input$customRBreakpoint))) &&
        all(!is.na(c(input$customSBreakpoint, input$customRBreakpoint)))
    })

    # Boolean. Are all the necessary inputs rendered and not null?
    allInputsReady <- reactive({
      inputs <- c(input$moFilter, input$abFilter, input$typeFilter, selectedSpecies())
      all(!is.null(inputs)) && length(inputs) == 4
    })

    #' The default guideline for the guideline selected by the user in data processing
    #' taking into account the selected microorganism, antimicrobial, type, and species.
    defaultGuideline <- reactive({
      bp <- AMR::clinical_breakpoints %>%
        filter_by_filters() %>%
        filter(guideline == processedGuideline()) %>%
        slice(1)

      if (nrow(bp) == 0) {
        return(AMR::clinical_breakpoints[0, ])
      }
      bp
    }) %>%
      bindEvent(input$moFilter, input$abFilter, input$typeFilter, selectedSpecies())

    #' The guideline to use in the table generation.
    #' If custom breakpoints are set, use those.
    selectedBreakpoints <- reactive({
      req(allInputsReady())

      sel <- filter_by_filters(customRefData())

      # If no custom breakpoints are set, use the default ones
      if (customBreakpointName %in% sel$guideline) {
        sel <- dplyr::filter(sel, guideline == customBreakpointName)
      } else {
        sel <- dplyr::filter(sel, guideline == processedGuideline())
      }
      sel <- dplyr::slice(sel, 1)
      return(sel)
    })

    #' Reference data for the table generation.
    #' If custom breakpoints are set, append them to the clinical breakpoints.
    customRefData <- reactive({
      data <- allCustomBreakpoints() %>%
        select(all_of(colnames(AMR::clinical_breakpoints))) %>%
        bind_rows(AMR::clinical_breakpoints)
      return(data)
    })

    # The selected species from the filter.
    selectedSpecies <- reactive({
      req(!is.null(reactiveData()))
      species <- input$speciesFilter
      if (is.null(species) && !is.null(reactiveData()$Species)) {
        species <- reactiveData()$Species[[1]]
      }
      return(species)
    })

    #' The mapped host name for the selected species.
    #' Since Species is already standardized in the cleaned data,
    #' we can use it directly.
    selected_host <- reactive({
      req(selectedSpecies())
      return(selectedSpecies())
    })

    appliedBpForDisplay <- reactive({
      req(input$moFilter, input$abFilter, input$typeFilter, selectedSpecies())
      uti <- identical(input$typeFilter, "Urinary")
      host <- selectedSpecies()

      bp <- selectedBreakpoints()

      s_user <- if (nrow(bp) > 0) suppressWarnings(as.numeric(bp$breakpoint_S[1])) else NA_real_
      r_user <- if (nrow(bp) > 0) suppressWarnings(as.numeric(bp$breakpoint_R[1])) else NA_real_

      guide <- if (nrow(bp) > 0 && !is.na(bp$guideline[1]) && nzchar(bp$guideline[1])) {
        as.character(bp$guideline[1])
      } else {
        processedGuideline()
      }

      if (is.na(s_user) || is.na(r_user)) {
        AMR::sir_interpretation_history(clean = TRUE)
        dummy <- AMR::as.mic(2)
        try(
          {
            invisible(AMR::as.sir(
              dummy,
              mo = AMR::as.mo(input$moFilter),
              ab = AMR::as.ab(input$abFilter),
              guideline = guide,
              reference_data = customRefData(),
              host = host,
              uti = uti
            ))
          },
          silent = TRUE
        )

        hist <- AMR::sir_interpretation_history(clean = FALSE)
        if (nrow(hist) > 0) {
          hist <- tidyr::separate(
            hist,
            breakpoint_S_R,
            into = c("bp_s", "bp_r"),
            sep = "-",
            fill = "right",
            remove = FALSE
          )
          s_app <- suppressWarnings(as.numeric(hist$bp_s[1]))
          r_app <- suppressWarnings(as.numeric(hist$bp_r[1]))
          guide <- if (!is.na(hist$guideline[1]) && nzchar(hist$guideline[1])) {
            hist$guideline[1]
          } else {
            guide
          }
          applied_mo <- hist$mo[1]
        } else {
          s_app <- NA_real_
          r_app <- NA_real_
          applied_mo <- NA_character_
        }
      } else {
        s_app <- s_user
        r_app <- r_user
        applied_mo <- AMR::as.mo(input$moFilter)
      }
      list(s = s_app, r = r_app, guideline = guide, applied_mo = applied_mo)
    })

    # Table data based on filters.
    table_result <- reactive({
      req(input$moFilter, input$abFilter, input$typeFilter, input$groupingVar)

      sp <- selectedSpecies()
      ap <- appliedBpForDisplay()

      table <- create_mic_frequency_tables(
        data = reactiveData(),
        group_by_var = input$groupingVar,
        ab = input$abFilter,
        mo = input$moFilter,
        type = input$typeFilter,
        species = sp,
        guideline = ap$guideline,
        s_bp = ap$s,
        r_bp = ap$r,
        reference_data = customRefData(),
        use_single_bp_as_both = TRUE
      )
      return(table)
    })

    # Boolean. Whether to show the error panel or the success panel.
    showErrorPanel <- reactive({
      nrow(reactiveData()) < 1 || is.null(table_result())
    })

    #' The cleaned data with custom breakpoints applied.
    #' Return this data to be used in the rest of the app.
    dataWithCustomBreakpoints <- reactive({
      data <- reactiveData()
      if (is.null(data)) {
        return(NULL)
      }

      data$Guideline <- processedGuideline()

      # If no custom breakpoints are set, return the data as is
      if (is.null(allCustomBreakpoints()) || nrow(allCustomBreakpoints()) == 0) {
        return(data)
      }

      # Merge custom breakpoints into the data

      sourceValues <- unique(data$Source)
      utiMatches <- findUtiMatch(sourceValues)

      for (i in seq_len(nrow(allCustomBreakpoints()))) {
        customBp <- allCustomBreakpoints()[i, ]
        customGuideline <- customBp %>%
          select(all_of(colnames(AMR::clinical_breakpoints)))

        if (customBp$uti) {
          utiValues <- sourceValues[utiMatches]
          isUti <- TRUE
        } else {
          utiValues <- sourceValues[!utiMatches]
          isUti <- FALSE
        }
        sel_host <- selected_host()
        allowed_values <- unique(c(
          tolower(customBp$Species),
          tolower(customBp$host),
          sel_host
        ))

        data <- data %>%
          mutate(
            mo = AMR::as.mo(Microorganism),
            genus = AMR::mo_genus(mo)
          )
        
        # Data which matches the custom breakpoint criteria
        bp_mo      <- AMR::as.mo(customBp$Microorganism)
        bp_genus   <- AMR::mo_genus(bp_mo)
        bp_ab      <- AMR::ab_name(customBp$Antimicrobial)
        
        matchingData <- data %>%
          dplyr::filter(
            Species %in% allowed_values,
            Antimicrobial == bp_ab,
            UTI == isUti,
            mo == bp_mo | genus == bp_genus   # species OR genus match
          )

        if (nrow(matchingData) == 0) {
          shiny::showNotification(
            paste0(
              "Custom breakpoint ignored: no matching isolates found for ",
              customBp$Microorganism,
              " + ",
              customBp$Antimicrobial,
              if (customBp$uti) " (UTI = TRUE)." else " (UTI = FALSE)."
            ),
            type = "warning",
            duration = 8
          )
          next # skip this breakpoint
        }
        
        # Re-calculate MIC interpretations based on custom breakpoints
        newInterpretations <- data.frame(
          MIC = unique(matchingData$MIC)
        )

        newInterpretations$Interpretation <- as.sir(
          x = AMR::as.mic(newInterpretations$MIC),
          mo = customGuideline$mo,
          ab = customGuideline$ab,
          guideline = customGuideline$guideline,
          reference_data = customGuideline,
          host = customGuideline$host,
          uti = customGuideline$uti,
          breakpoint_type = customGuideline$type
        )

        # ------------------------------------------------------------------------------
        # TEMPORARY FUNCTION TO CORRECT BUG IN AMR PACKAGE
        # ------------------------------------------------------------------------------
        bp_S <- customGuideline$breakpoint_S
        bp_R <- customGuideline$breakpoint_R

        #' TODO: Documentation
        #' [Summary]
        #'
        #' @param x [Description]
        #'
        #' @return [Description]
        parse_mic_interval <- function(x) {
          op <- str_match(x, "^(<=|>=|<|>|=)?\\s*([0-9.]+)$")[, 2]
          val <- as.numeric(str_match(x, "^(?:<=|>=|<|>|=)?\\s*([0-9.]+)$")[, 2])
          op[is.na(op)] <- "="
          tibble(
            low = dplyr::case_when(op %in% c("<", "<=") ~ -Inf, TRUE ~ val),
            low_inc = dplyr::case_when(
              op == ">=" ~ TRUE,
              op %in% c(">", "<", "<=") ~ FALSE,
              TRUE ~ TRUE
            ),
            high = dplyr::case_when(op %in% c(">", " >=") ~ Inf, op %in% c(">=") ~ Inf, TRUE ~ val),
            high_inc = dplyr::case_when(
              op == "<=" ~ TRUE,
              op %in% c("<", ">", ">=") ~ FALSE,
              TRUE ~ TRUE
            )
          )
        }

        #' TODO: Documentation
        #' [Summary]
        #'
        #' @param low [Description]
        #' @param low_inc [Description]
        #' @param high [Description]
        #' @param high_inc [Description]
        #' @param S [Description]
        #' @param R [Description]
        #'
        #' @return [Description]
        classify_interval <- function(low, low_inc, high, high_inc, S, R) {
          entirely_S <- (high < S) | (high == S)
          entirely_R <- (low > R) | (low == R)
          entirely_I <- ((low > S) | (low == S & !low_inc)) &
            ((high < R) | (high == R & !high_inc))
          dplyr::case_when(
            entirely_S ~ "S",
            entirely_R ~ "R",
            entirely_I ~ "I",
            TRUE ~ "NI"
          )
        }

        newInterpretations <- newInterpretations %>%
          bind_cols(parse_mic_interval(as.character(.$MIC))) %>%
          mutate(
            Interpretation = classify_interval(low, low_inc, high, high_inc, bp_S, bp_R)
          ) %>%
          select(-low, -low_inc, -high, -high_inc)
        # ------------------------------------------------------------------------------
        # END OF TEMPORARY FIX
        # ------------------------------------------------------------------------------

        matchingData <- matchingData %>%
          select(-Interpretation, -Guideline) %>%
          left_join(newInterpretations, by = "MIC")
        matchingData$Guideline <- customBp$guideline

        data <- data %>%
          filter(!InternalID %in% matchingData$InternalID) %>%
          bind_rows(matchingData)
      }

      data %>%
        arrange(InternalID)
    })

    #' The clinical breakpoints filtered based on the selected antimicrobial.
    #' Use a `bandCache` to avoid re-filtering unnecessarily.
    ab_clinical_breakpoints <- reactive({
      return(get_clinical_bps(g_fullClinicalBreakpoints, list(ab = AMR::as.ab(input$abFilter))))
    }) %>%
      bindCache(input$abFilter)

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    output$filters <- renderUI({
      data <- reactiveData()
      req(data)
      tagList(
        selectizeInput(
          ns("moFilter"),
          label = "Microorganism",
          choices = sort(unique(data$Microorganism), na.last = NA),
          selected = names(sort(table(data$Microorganism), decreasing = TRUE))[1],
          multiple = FALSE
        ),

        selectizeInput(
          ns("abFilter"),
          label = "Antimicrobial",
          choices = sort(unique(data$Antimicrobial), na.last = NA),
          selected = names(sort(table(data$Antimicrobial), decreasing = TRUE))[1],
          multiple = FALSE
        ),

        selectizeInput(
          ns("typeFilter"),
          label = "Type",
          choices = c("Urinary", "Non-urinary"),
          selected = "Urinary",
          multiple = FALSE
        ),

        if (!is.null(data$Species) && length(unique(na.omit(data$Species))) > 1) {
          selectizeInput(
            ns("speciesFilter"),
            label = "Species",
            choices = sort(unique(data$Species), na.last = NA),
            selected = names(sort(table(data$Species), decreasing = TRUE))[1],
            multiple = FALSE
          )
        }
      )
    })

    # Show either the error panel or the success panel based on whether data is available
    output$content <- renderUI({
      req(reactiveData(), input$moFilter, input$abFilter, input$typeFilter, input$groupingVar)
      tagList(
        uiOutput(ns("errorPanel")),
        uiOutput(ns("successPanel"))
      )
    })

    # Only show the error panel if there is no data to show
    output$errorPanel <- renderUI({
      req(showErrorPanel())
      wellPanel(
        style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
        div(
          style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
          uiOutput(ns("errorHandling"))
        ),
        class = "contentWell"
      )
    })

    # Only show the success panel (actual page content) if there is data to show
    output$successPanel <- renderUI({
      req(!showErrorPanel())
      tagList(
        wellPanel(
          style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh; min-width: 500px; position: relative;",
          uiOutput(ns("breaksMessage")),
          div(
            class = "ab-table-wrapper",
            withSpinner(gt::gt_output(ns("plot")), type = 4, color = "#44CDC4")
          ),
          class = "contentWell"
        ),
        div(
          style = "display: flex; justify-content: end;",
          downloadButton(ns("save_table"), "Save Data", class = "plotSaveButton")
        )
      )
    })

    # Table showing all clinical breakpoints for the selected antimicrobial in collapse panel
    output$clinicalBpTable <- DT::renderDataTable({
      req(bpTableOpen())
      req(ab_clinical_breakpoints())
      clinical_bps <- ab_clinical_breakpoints()
      data <- allCustomBreakpoints() %>%
        select(all_of(colnames(clinical_bps))) %>%
        mutate(mo = AMR::mo_name(mo), ab = AMR::ab_name(ab)) %>%
        rbind(clinical_bps)

      return(get_clinical_bps_table(data))
    })
    outputOptions(output, "clinicalBpTable", suspendWhenHidden = FALSE)

    # Inputs for user to select custom breakpoints
    output$customBreakpointsUI <- renderUI({
      req(!showErrorPanel())
      req(input$moFilter, input$abFilter, input$typeFilter, input$groupingVar)
      
      bp <- allCustomBreakpoints()
      
      if (nrow(bp) == 0) {
        return(getCustomBpInputs())
      }
      
      input_mo    <- AMR::as.mo(input$moFilter)
      input_genus <- AMR::mo_genus(input_mo)
      
      existingBp <- bp %>%
        dplyr::mutate(
          mo_code = if ("mo" %in% names(.)) mo else AMR::as.mo(Microorganism),
          genus   = AMR::mo_genus(mo_code)
        ) %>%
        dplyr::filter(
          # species match OR mo code match OR genus match
          (Microorganism == input$moFilter |
             mo_code == input_mo |
             genus == input_genus),
          (Antimicrobial == input$abFilter | ab == AMR::as.ab(input$abFilter)),
          host == selected_host()
        )
      
      if (nrow(existingBp) == 0) {
        return(getCustomBpInputs())
      }
      
      getCustomBpInputs(
        as.numeric(existingBp$breakpoint_S[1]),
        as.numeric(existingBp$breakpoint_R[1])
      )
    })
    

    # Only show the custom breakpoints message if there is a single microorganism selected
    output$breaksMessage <- renderUI({
      req(!showErrorPanel())
      ap <- appliedBpForDisplay()
      mo_input <- AMR::as.mo(input$moFilter)
      used_label <- if (
        !is.null(ap$applied_mo) &&
          length(ap$applied_mo) == 1 &&
          !is.na(ap$applied_mo) &&
          ap$applied_mo != mo_input
      ) {
        paste0(" Inferred using ", AMR::mo_name(ap$applied_mo, language = "en"), " breakpoints.")
      } else {
        ""
      }

      htmltools::div(
        htmltools::strong(getBpLabel(ap$s, ap$r, ap$guideline)),
        htmltools::tags$small(used_label)
      )
    })

    output$errorHandling <- renderUI({
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
        icon("disease", style = "font-size:100px; color: #44CDC4"),
        h4("Oops... looks like there isn't enough data for this plot."),
        h6("This combination of filters does not appear in your data.")
      )
    })

    output$plot <- gt::render_gt({
      req(table_result())
      table_result()
    })

    # Example format for custom breakpoint uploads.
    output$expectedFormatTable <- DT::renderDataTable({
      return(DT::datatable(
        expected_format_df,
        rownames = FALSE,
        options = list(dom = '', ordering = FALSE)
      ))
    })

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------

    #' Filter breakpoint data based on selected filters.
    #'
    #' @param data  The clinical breakpoints data to filter.
    #' @return      The filtered clinical breakpoints data.
    filter_by_filters <- function(data) {
      is_uti   <- input$typeFilter == "Urinary"
      mo_val   <- AMR::as.mo(input$moFilter)
      genus_val <- AMR::mo_genus(mo_val)
      ab_val   <- AMR::as.ab(input$abFilter)
      host_val <- selected_host()
      
      base <- data %>%
        dplyr::filter(
          method == "MIC",
          uti == is_uti,
          host == host_val,
          ab == ab_val
        )
      
      # Try exact species match
      exact <- base %>% dplyr::filter(mo == mo_val)
      if (nrow(exact) > 0) {
        return(exact)
      }
      
      # Fallback to genu-level
      genus_matched <- base %>%
        dplyr::mutate(genus = AMR::mo_genus(mo)) %>%
        dplyr::filter(genus == genus_val)
      
      if (nrow(genus_matched) > 0) {
        return(genus_matched)
      }
      
      # Nothing found -> return empty with same cols
      base[0, , drop = FALSE]
    }
    

    #' Get the UI inputs for custom breakpoints.
    #'
    #' @param sValue  Initial numeric value for the S breakpoint input (default is NULL).
    #' @param rValue  Initial numeric value for the R breakpoint input (default is NULL).
    #' @return        A `div` containing the UI for custom breakpoints.
    getCustomBpInputs <- function(sValue = NULL, rValue = NULL) {
      return(div(
        class = "custom-breakpoints-container",
        tags$label("Custom breakpoints", class = "shiny-input-container"),
        div(
          class = "align-end-row",
          column(
            6,
            numericInput(
              ns("customSBreakpoint"),
              label = "S",
              value = sValue
            )
          ),
          column(
            6,
            numericInput(
              ns("customRBreakpoint"),
              label = "R",
              value = rValue
            )
          )
        ),
        div(
          class = "align-end-row",
          actionButton(
            ns("deleteCustomBreakpoints"),
            label = "Delete",
            class = "white-btn"
          ),
          actionButton(
            ns("addCustomBreakpoints"),
            label = "Add",
            class = "blue-btn"
          )
        )
      ))
    }

    #' Enable multiple inputs by their IDs.
    #'
    #' @param inputIds A vector of input IDs to enable.
    #' @return         None. This function modifies the UI state.
    enableInputs <- function(inputIds) {
      lapply(inputIds, function(id) {
        shinyjs::enable(id)
      })
    }

    #' Disable multiple inputs by their IDs.
    #'
    #' @param inputIds A vector of input IDs to disable.
    #' @return         None. This function modifies the UI state.
    disableInputs <- function(inputIds) {
      lapply(inputIds, function(id) {
        shinyjs::disable(id)
      })
    }

    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    #' Handles a click on the file info button.
    #' Opens a modal with information about the expected format of custom breakpoint files.
    observe({
      showModal(modalDialog(
        title = "Custom Breakpoints Upload Information",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        p("Only CSV and XLSX files are supported for upload."),
        p("The following format is what is expected for upload files (extra columns are ignored)."),
        p("Please ensure that the column names match exactly, including case sensitivity."),
        br(),
        p("Please pay attention to the following rules:"),
        tags$ul(
          tags$li("type: Must be either \"ECOFF\", \"animal\", or \"human\"."),
          tags$li(
            "host: Must be either \"ECOFF\", \"aquatic\", \"cats\", \"cattle\", \"dogs\", \"horse\", \"human\", \"poultry\", or \"swine\"."
          ),
        ),
        br(),
        p(
          "For more information on column formatting, please refer to the ",
          tags$a(
            href = "https://amr-for-r.org/reference/clinical_breakpoints.html?q=ref_tbl#format",
            target = "_blank",
            "AMR package documentation"
          )
        ),
        DT::dataTableOutput(ns("expectedFormatTable"))
      ))
    }) %>%
      bindEvent(input$file_info)

    #' Handles the upload of any custom breakpoints from a csv or xlsx file.
    observe({
      req(input$custom_bp_upload)

      file <- input$custom_bp_upload
      ext <- tools::file_ext(file$datapath)

      if (!ext %in% c("csv", "xlsx")) {
        showNotification(
          ui = "Invalid file type. Please upload a CSV or XLSX file.",
          type = "error",
          duration = 5
        )
        shinyjs::reset("custom_bp_upload")
        req(FALSE)
      }
      uploaded_data <- tryCatch(
        {
          if (ext == "csv") {
            read.csv(file$datapath, stringsAsFactors = FALSE)
          } else if (ext == "xlsx") {
            readxl::read_excel(file$datapath)
          }
        },
        error = function(e) {
          showNotification(
            ui = paste("Error reading file:", e$message),
            type = "error",
            duration = 5
          )
          shinyjs::reset("custom_bp_upload")
          req(FALSE)
        }
      )

      expected_colnames <- colnames(expected_format_df)
      uploaded_colnames <- colnames(uploaded_data)

      if (!all(expected_colnames %in% uploaded_colnames)) {
        showNotification(
          ui = "Uploaded file is missing required columns. Please click the info button for the expected format.",
          type = "error",
          duration = 5
        )
        shinyjs::reset("custom_bp_upload")
        req(FALSE)
      }

      # Check if any non-custom bps are being uploaded
      amr_data <- AMR::clinical_breakpoints %>%
        select(all_of(expected_colnames))
      formatted_upload_data <- uploaded_data %>%
        select(all_of(expected_colnames))

      matching <- merge(amr_data, formatted_upload_data)
      if (nrow(matching) > 0) {
        showNotification(
          ui = "Uploaded file contains non-custom breakpoints that already exist in the AMR package. Please remove these and try again.",
          type = "error",
          duration = 5
        )
        shinyjs::reset("custom_bp_upload")
        req(FALSE)
      }

      current_custom_bps <- allCustomBreakpoints()
      # Make sure that the ab and mo columns are of class ab and mo
      new_custom_bps <- uploaded_data %>%
        rowwise() %>%
        mutate(
          guideline = customBreakpointName,
          method = "MIC",
          rank_index = if ("rank_index" %in% uploaded_colnames) {
            rank_index
          } else {
            NA_real_
          },
          ref_tbl = if ("ref_tbl" %in% uploaded_colnames) {
            ref_tbl
          } else {
            NA_character_
          },
          disk_dose = if ("disk_dose" %in% uploaded_colnames) {
            disk_dose
          } else {
            NA_character_
          },
          is_SDD = if ("is_SDD" %in% uploaded_colnames) {
            is_SDD
          } else {
            FALSE # TODO: Check if this is the right default
          },
          site = if ("site" %in% uploaded_colnames) {
            site
          } else {
            NA_character_
          },
          Species = host,
          Source = if (uti) {
            "Urinary"
          } else {
            "Non-urinary"
          },
          # These should be the original values provided by the user
          Microorganism = AMR::mo_name(mo),
          Antimicrobial = AMR::ab_name(ab),
          ab = AMR::as.ab(ab),
          mo = AMR::as.mo(mo),
          # Make sure these columns are logical
          uti = as.logical(uti),
          # Make sure these columns are doubles
          breakpoint_S = as.numeric(breakpoint_S),
          breakpoint_R = as.numeric(breakpoint_R),
          # Make sure these columns are character
          site = as.character(site),
          disk_dose = as.character(disk_dose),
          ref_tbl = as.character(ref_tbl)
        ) %>%
        select(all_of(colnames(current_custom_bps)))

      bp_cols <- c("breakpoint_S", "breakpoint_R")
      expected_colnames_not_bp <- expected_colnames[!expected_colnames %in% bp_cols]

      # If there are already custom breakpoints set, show a notification saying they will be overriden
      matching <- current_custom_bps %>%
        ungroup() %>% # For rowwise
        # Filter out any rows that match on all columns except the breakpoints
        semi_join(new_custom_bps, by = expected_colnames_not_bp) %>%
        # Then filter out rows that also match on bp cols
        filter(!do.call(paste0, .[bp_cols]) %in% do.call(paste0, new_custom_bps[bp_cols]))

      if (nrow(matching) > 0) {
        showNotification(
          ui = "Uploaded file contains custom breakpoints that already exist. These will be overridden.",
          type = "warning",
          duration = 5
        )
        # Remove any old custom bps that match the uploaded ones
        current_custom_bps <- current_custom_bps %>%
          anti_join(matching, by = expected_colnames_not_bp)
      }

      new_custom_bps <- new_custom_bps %>%
        rbind(current_custom_bps) %>%
        distinct()

      allCustomBreakpoints(new_custom_bps)
    }) %>%
      bindEvent(input$custom_bp_upload)

    #' Controls the enabling/disabling of the custom breakpoint inputs and buttons.
    #'
    #' If a custom breakpoint is already set (for the specific grouping), then disable
    #' everything expect the delete button.
    #' If no custom breakpoint is set, and the inputs are both filled, disabled the
    #' delete button.
    #' If no custom breakpoint is set, and the inputs are not both filled, disable
    #' both the add and delete buttons.
    observe({
      req(allInputsReady())
      req(input$moFilter, input$abFilter, input$typeFilter, selectedSpecies())
      # Include all these inputs so that it is triggered when anything changes
      input$addCustomBreakpoints
      input$deleteCustomBreakpoints
      input$customSBreakpoint
      input$customRBreakpoint

      req(selectedBreakpoints())
      selectedBp <- selectedBreakpoints()

      if (customBreakpointName %in% selectedBp$guideline) {
        enableInputs("deleteCustomBreakpoints")
        disableInputs(c("addCustomBreakpoints", "customSBreakpoint", "customRBreakpoint"))
        req(FALSE)
      } else {
        disableInputs("deleteCustomBreakpoints")
      }

      # Only reaches here if no custom breakpoints are set (Add has not been clicked yet)
      enableInputs(c("customSBreakpoint", "customRBreakpoint"))
      if (canCreateCustomBreakpoints()) {
        enableInputs("addCustomBreakpoints")
      } else {
        disableInputs("addCustomBreakpoints")
      }
    })

    #' Add the custom breakpoints to a df containing all custom breakpoints.
    #' This df is used throughout the app.
    observe({
      newCustomBp <- defaultGuideline()

      if (nrow(newCustomBp) == 0) {
        #' A default bp is not found in `AMR::clinical_breakpoints` that meet the criteria.
        #' Create a new row with the necessary values.
        selected_host <- selected_host()
        newCustomBp <- add_row(emptyCustomBp) |>
          mutate(
            mo = as.mo(input$moFilter),
            ab = as.ab(input$abFilter),
            method = "MIC",
            type = "animal",
            host = selected_host,
            uti = input$typeFilter == "Urinary",
            is_SDD = FALSE
          )
      }

      newCustomBp <- newCustomBp |>
        mutate(
          Microorganism = input$moFilter,
          Antimicrobial = input$abFilter,
          Species = selectedSpecies(),
          Source = input$typeFilter,
          guideline = customBreakpointName,
          breakpoint_S = as.numeric(input$customSBreakpoint),
          breakpoint_R = as.numeric(input$customRBreakpoint)
        )

      newCustomBps <- allCustomBreakpoints() %>%
        rbind(newCustomBp) %>%
        distinct()
      allCustomBreakpoints(newCustomBps)
    }) %>%
      bindEvent(input$addCustomBreakpoints)

    # Delete the saved custom breakpoints
    observe({
      customBpRemoved <- allCustomBreakpoints() %>%
        filter(
          !(guideline == customBreakpointName &
            mo == AMR::as.mo(input$moFilter) &
            ab == AMR::as.ab(input$abFilter) &
            uti == (input$typeFilter == "Urinary") &
            host == selected_host())
        )
      allCustomBreakpoints(customBpRemoved)
    }) %>%
      bindEvent(input$deleteCustomBreakpoints)

    # Toggle the clinical breakpoints table visibility
    observe({
      bpTableOpen(!bpTableOpen())
    }) %>%
      bindEvent(input$clinicalBpTableToggle)

    # ------------------------------------------------------------------------------
    # Module return
    # ------------------------------------------------------------------------------

    # Return the cleaned data with custom breakpoints applied and all custom breakpoints.
    return(list(
      dataWithCustomBreakpoints = dataWithCustomBreakpoints,
      customBreakpoints = allCustomBreakpoints,
      all_breakpoints_used = selectedBreakpoints
    ))
  })
}

mic_table_tab <- list(
  ui = ui,
  server = server
)
