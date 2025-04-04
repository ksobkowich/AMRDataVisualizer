filterPanelUI <- function(id, data) {
  ns <- NS(id)
  bsCollapse(
    id = "collapsePanel",
    open = "Filters",
    multiple = T,
    bsCollapsePanel(
      HTML("Filters <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
            style='float: right; color: #aaa;'></span>"),
      div(
        style = "display: flex; justify-content: center; align-items: center; position: relative;",
        actionButton(
          ns("editFilters"),
          label = "",
          icon = icon("pencil-alt", lib = "font-awesome"),
          class = "btn btn-default",
          style = "position: absolute; right: 10px;cbackground-color: #f0f0f0; color: grey; border: none;"
        ),
        style = "margin-top: 2px;"
      ),
      uiOutput(ns("filters")),
      actionButton(ns("applyFilter"), "Apply", class = "submitButton")
    )
  )
}


filterPanelServer <- function(id, data, default_filters, auto_populate = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_filters <- reactiveValues(columns = default_filters)
    
    applyDefaultFilters <- function(data, filters, auto_populate) {
      filtered <- data
      for (col in filters) {
        if (col == "Date") {
          filtered <- filtered[filtered$Date >= min(data$Date, na.rm = TRUE) &
                                 filtered$Date <= max(data$Date, na.rm = TRUE), ]
        } else if (isTRUE(auto_populate[[col]])) {
          most_common_value <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
          filtered <- filtered[filtered[[col]] == most_common_value, ]
        }
      }
      return(filtered)
    }
    
    filteredData <- reactiveVal({
      applyDefaultFilters(data, default_filters, auto_populate)
    })

    output$filters <- renderUI({
      updateUI <- list()
      
      fixed_order <- c("Microorganism", "Antimicrobial", "Source", "Species", "Date")
      
      other_filters <- setdiff(selected_filters$columns, fixed_order)
      other_filters <- sort(other_filters)
      
      ordered_filters <- c(fixed_order, other_filters)
      
      for (col in ordered_filters) {
        
        if (col %in% selected_filters$columns) {
          if (col == "Date") {
            
            updateUI[[ns("timeFilter")]] <- dateRangeInput(ns("timeFilter"), "Timeframe", 
                                                           min = min(data$Date, na.rm = TRUE), 
                                                           max = max(data$Date, na.rm = TRUE), 
                                                           start = min(data$Date, na.rm = TRUE), 
                                                           end = max(data$Date, na.rm = TRUE))
            
            updateUI[[ns("timeButtons")]] <- div(
              actionButton(ns("last3Months"), "3 mo", class = "quickDateButton"),
              actionButton(ns("last6Months"), "6 mo", class = "quickDateButton"),
              actionButton(ns("pastYear"), "1 yr", class = "quickDateButton"),
              actionButton(ns("allData"), "All", class = "quickDateButton"),
              class = "quickDateButtonDiv"
            )
            
          } else if (col == "Resistant to:") {
            
            updateUI[[ns("resistanceFilter")]] <- selectizeInput(
              ns("resistanceFilter"),
              label = "Resistant to:",
              choices = sort(unique(data$Antimicrobial), na.last = NA),
              selected = NULL,
              multiple = TRUE
            )
            
          } else if (col == "WHO AWaRe Class:") {
            
            updateUI[[ns("awareFilter")]] <- selectizeInput(
              ns("awareFilter"),
              label = "WHO AWaRe Class:",
              choices = c("Access", "Reserve", "Watch"),
              selected = NULL,
              multiple = TRUE
            )
            
          } else {
            
            default_value <- if (isTRUE(auto_populate[[col]])) {
              names(sort(table(data[[col]]), decreasing = TRUE))[1]
            } else {
              NULL
            }
            
            updateUI[[ns(paste0(col, "Filter"))]] <- selectizeInput(
              ns(paste0(col, "Filter")), 
              label = col, 
              choices = sort(unique(data[[col]]), na.last = NA), 
              selected = default_value,
              multiple = TRUE
            )
          }
        }
      }
      
      tagList(
        do.call(tagList, updateUI)
      )
    })
    
    
    
    observeEvent(input$editFilters, {
      showModal(modalDialog(
        title = "Select Filters",
        checkboxGroupInput(ns("selectedFilters"), "Choose Filters", 
                           choices = sort(c(colnames(data), 
                                            "Resistant to:", 
                                            "WHO AWaRe Class:")), 
                           selected = selected_filters$columns),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("applyModalFilters"), "Apply")
        )
      ))
    })
    
    observeEvent(input$applyModalFilters, {
      selected_filters$columns <- input$selectedFilters
      removeModal()
    })
    
    observeEvent(input$applyFilter, {
      
      filtered <- data
      
      if (!is.null(input$resistanceFilter) && input$resistanceFilter != "") {
        resistant_rows <- data %>%
          filter(Antimicrobial == input$resistanceFilter & Interpretation == "R")
        
        resistant_ids <- resistant_rows %>%
          select(ID, Date, Region, Subregion, Species, Source, Microorganism) %>%
          distinct()
        
        filtered <- data %>%
          inner_join(resistant_ids, by = colnames(resistant_ids))
      }
      
      if (!is.null(input$awareFilter) && length(input$awareFilter) > 0) {
        
        relevantAntimicrobials <- awareList %>%
          filter(awareGroup %in% input$awareFilter) %>%
          pull(Antimicrobial)
        
        filtered <- data %>%
          filter(Antimicrobial %in% relevantAntimicrobials)
        
      }
      
      for (col in selected_filters$columns) {
        
        if (col == "Date" && !is.null(input$timeFilter)) {
          filtered <- filtered[filtered$Date >= input$timeFilter[1] & filtered$Date <= input$timeFilter[2], ]
          
        } else if (!is.null(input[[paste0(col, "Filter")]])) {
          
          selected_vals <- input[[paste0(col, "Filter")]]
          
          if (length(selected_vals) > 0) {
            
            filtered <- filtered[filtered[[col]] %in% selected_vals, ]
            
          }
        }
      }
      
      if (!is.null(input$resistanceFilter) && input$resistanceFilter != "") {
        conflicting_rows <- filtered %>%
          filter(Antimicrobial == input$resistanceFilter & Interpretation == "S")
        
        if (nrow(conflicting_rows) > 0) {
          
          conflicting_bacteria <- conflicting_rows %>%
            group_by(Microorganism) %>%
            summarise(count = n()) %>%
            arrange(desc(count)) %>%
            pull(Microorganism)
          
          bacteria_list <- paste("<ul>", 
                                 paste("<li>", conflicting_bacteria, "</li>", collapse = ""),
                                 "</ul>")
          
          shinyalert(
            title = "Warning: Conflicting Data",
            text = paste("There appear to be conflicting duplicates in your data for the selected antimicrobial, with indistiguishable isolates marked as susceptible ('S') and others as resistant ('R'). We recommend reviewing your data to identify and resolve these discrepancies manually.<br> The following bacteria have conflicting interpretations:<br>", bacteria_list),
            type = "warning",
            html = TRUE,
            closeOnClickOutside = TRUE,
            className = 'alert'
          )
        }
      }
      
      filteredData(filtered)
    })
    
    
    observeEvent(input$last3Months, {
      updateDateRangeInput(session, "timeFilter", 
                           min = min(data$Date), max = max(data$Date), 
                           start = max(data$Date) %m-% months(3), end = max(data$Date))
    })
    
    observeEvent(input$last6Months, {
      updateDateRangeInput(session, "timeFilter", 
                           min = min(data$Date), max = max(data$Date), 
                           start = max(data$Date) %m-% months(6), end = max(data$Date))
    })
    
    observeEvent(input$pastYear, {
      updateDateRangeInput(session, "timeFilter", 
                           min = min(data$Date), max = max(data$Date), 
                           start = max(data$Date) %m-% years(1), end = max(data$Date))
    })
    
    observeEvent(input$allData, {
      updateDateRangeInput(session, "timeFilter", 
                           min = min(data$Date), max = max(data$Date), 
                           start = min(data$Date), end = max(data$Date))
    })
    
    #---
    
    return(list(
      filteredData = filteredData,
      activeFilters = reactive({
        filters <- list()
        
        if (!is.null(input$resistanceFilter) && length(input$resistanceFilter) > 0) {
          filters[["Resistant to:"]] <- input$resistanceFilter
        }
        
        if (!is.null(input$awareFilter) && length(input$awareFilter) > 0) {
          filters[["WHO AWaRe Class:"]] <- input$awareFilter
        }
        
        for (col in selected_filters$columns) {
          if (col == "Date" && !is.null(input$timeFilter)) {
            filters[["Date"]] <- input$timeFilter
          } else {
            val <- input[[paste0(col, "Filter")]]
            if (!is.null(val) && length(val) > 0) {
              filters[[col]] <- val
            }
          }
        }
        
        return(filters)
      })
    ))
    
    
    
    #---
  })
}




