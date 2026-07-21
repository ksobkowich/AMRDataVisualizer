#' UI for the change log module.
#'
#' @param id  Module ID.
#' @return    Module UI.
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "changelog-table",
    div(
      style = "display: flex; justify-content: end; align-items: center; gap: 1rem;",
      actionButton(ns('reset'), 'Reset', class = "changeLogButton white"),
      actionButton(ns('save'), 'Save', class = "changeLogButton")
    ),
    DT::dataTableOutput(ns("table"))
  )
}

#' Server logic for the change log module.
#'
#' @param id              The ID of the module.
#' @param changeLogData   The change log data to be displayed in the table.
#' @param cleanedData     The cleaned data to be updated when the table is edited.
#' @param availableData   The available data to be used for filtering the cleaned data.
#' @param type            The type of data being displayed in the table. Can be "microorganism" or "antimicrobial".
#' @return                A reactive containing the updated change log data.
server <- function(id, changeLogData, cleanedData, availableData, type = "microorganism") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    currentChangeLogData <- reactiveVal(NULL)
    tableChangeLogData <- reactiveVal(NULL) # Data passed back to be saved in html

    #' Format the change log data to be displayed in the table.
    formattedData <- reactive({
      input$reset # Reset the table to the original data when the reset button is clicked

      shinyjs::runjs(sprintf('enableModalClose("%s");', type))
      if (is.null(currentChangeLogData())) {
        return(NULL)
      }
      data <- as.data.frame(currentChangeLogData())
      colnames(data) <- c("original", "renamed")
      data <- data %>%
        mutate(Action = ifelse(original == renamed, "No Change", "Automatically changed")) %>%
        arrange(original)

      #' Above will assume anything that is not the same is automatically changed.
      #' Even if the user changes it, it will still be "automatically changed" so we need to
      #' update the action based on the original data from the change log (that was automatically
      #' renamed).
      originalCLData <- changeLogData()
      if (!is.null(originalCLData)) {
        colnames(originalCLData) <- c("original", "automatically_renamed")
        data <- data %>%
          left_join(originalCLData, by = "original") %>%
          mutate(Action = ifelse(renamed == automatically_renamed, Action, "Changed by user")) %>%
          select(-automatically_renamed)
      }
      # Factorize the Action column to allow column filtering as a select input rather than text
      data$Action <- factor(
        data$Action,
        levels = c("No Change", "Automatically changed", "Changed by user")
      )
      data
    })

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    #' `server = FALSE` ensures that any changes made to the table via JS are persisted and not
    #' reset by the server side when doing things like filtering or sorting.
    output$table <- DT::renderDataTable(
      {
        req(!is.null(formattedData()))

        data <- formattedData()
        columnNames <- colnames(data)
        if (type == "microorganism") {
          columnNames <- c("Original Microorganism", "Renamed Microorganism", "Action")
        } else if (type == "antimicrobial") {
          columnNames <- c("Original Antimicrobial", "Renamed Antimicrobial", "Action")
        }
        datatable(
          data,
          colnames = columnNames,
          rownames = FALSE,
          style = 'bootstrap',
          class = 'table-bordered',
          caption = "Double click a cell to edit",
          selection = "none",
          filter = "top",
          # This enables text editing for cells in all columns, use CSS to restict editing of columns
          editable = TRUE,
          options = list(
            dom = 't', # Filtering and table only
            paging = FALSE, # Disable pagination
            pageLength = -1, # Show all rows
            ordering = FALSE, # Disable ordering
            stateSave = TRUE
          ),
          callback = JS(sprintf(
            'watchDTCellChange("%s", "%s");',
            ns("table"),
            ns("updateTableData")
          ))
        )
      },
      server = FALSE
    )

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    # Save the original change log data so that we can compare it to the current data.
    observe({
      currentChangeLogData(changeLogData())
    }) %>%
      bindEvent(changeLogData())

    # Set this initially to populate it with the original data
    observe({
      data <- formattedData()
      tableChangeLogData(data)
    }) %>%
      bindEvent(formattedData(), input$reset)

    #' This observe is triggered each time the table is edited.
    #' It allow the `input$updateTableData` value to be the current table data at all times.
    observe({
      newData <- fromJSON(input$updateTableData)
      req(!is.null(newData), is.data.frame(newData), nrow(newData) > 0)

      #' At this point need to update any rows with the value "Changed by user" (in the `action`
      #' column) in the cleaned data.
      updatedByUser <- newData %>%
        filter(action == "Changed by user")

      if (nrow(updatedByUser) <= 0) {
        # Enable the modal close button if no changes were made
        shinyjs::runjs(sprintf('enableModalClose("%s");', type))
      } else {
        shinyjs::runjs(sprintf('enableModalClose("%s", false);', type))
      }
    }) %>%
      bindEvent(input$updateTableData)

    # Triggered on the save button click.
    observe({
      newData <- fromJSON(input$updateTableData)
      tableChangeLogData(newData)
      req(!is.null(newData), is.data.frame(newData), nrow(newData) > 0)

      #' At this point need to update any rows with the value "Changed by user" (in the `action`
      #' column) in the cleaned data.
      updatedByUser <- newData %>%
        filter(action == "Changed by user")
      # If no changes were made, do not proceed.
      req(nrow(updatedByUser) > 0)

      #' Join the original renamed data (from `cleanedData`) with the newly renamed data.
      #'
      #' Also need to update the `currentChangeLogData` with the new values.
      #' This is so that when the table is re-rendered, the change log uses the new values to
      #' check whether the difference between the original change log and the current one is whether
      #' the user changed the value and saved it or if it was automatically changed in processing.
      #'
      #' We use the IDs from `availableData` to match the `InternalID` to make sure we are only
      #' changing target column values for the rows that are expected to be changed.
      #' This is a fail-safe in case there are any duplicates of names in the change log.
      oldChangeLogData <- currentChangeLogData()
      cleanedData <- cleanedData()
      availableData <- availableData() %>%
        mutate(InternalID = as.character(InternalID))

      originalTargetColumn <- switch(
        type,
        microorganism = "Microorganism",
        antimicrobial = "Antimicrobial"
      )

      availableData <- availableData %>%
        filter(!!sym(originalTargetColumn) %in% updatedByUser$original)

      for (x in seq_along(unique(availableData[[originalTargetColumn]]))) {
        targetValue <- unique(availableData[[originalTargetColumn]])[x]
        targetIDs <- availableData$InternalID[availableData[[originalTargetColumn]] == targetValue]
        newValue <- updatedByUser$renamed[updatedByUser$original == targetValue]
        cleanedData[[originalTargetColumn]][cleanedData$InternalID %in% targetIDs] <- newValue
        oldChangeLogData[, 2][oldChangeLogData[[originalTargetColumn]] == targetValue] <- newValue
      }

      # Update the cleaned data with the new values changed by the user.
      cleanedData(cleanedData)
      currentChangeLogData(oldChangeLogData)
      shinyjs::runjs(sprintf('enableModalClose("%s");', type))
    }) %>%
      bindEvent(input$save)

    # ------------------------------------------------------------------------------
    # Module return
    # ------------------------------------------------------------------------------

    # Return the current change log data reactive
    return(tableChangeLogData)
  })
}

change_log <- list(
  ui = ui,
  server = server
)
