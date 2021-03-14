#' upload_cs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_cs_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 4,
      bs4Dash::bs4Card(
        fileInput(
          ns("file_cs"),
          "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        checkboxInput(ns("cs_upload_header"), "Header", TRUE),
        shinyWidgets::awesomeRadio(
          inputId = ns("cs_upload_sep_radio"),
          label = "Separator",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ",",
          status = "warning"
        ),
        width = NULL,
        title = "Upload Sites",
        solidHeader = TRUE,
        dropdownMenu = bs4Dash::dropdownItemList(bs4Dash::dropdownItem(name = "Upload a csv file"),
                                                 icon = "question-circle"),
        closable = FALSE,
        status = "warning"
      )
    ),
    column(
      8,
      bs4Dash::bs4Card(
        title = c("Uploaded Sites", uiOutput('undoUI')),
        closable = FALSE,
        status = "primary",
        collapsible = TRUE,
        elevation = 4,
        width = NULL,
        solidHeader = TRUE,
        maximizable = TRUE,
        DT::dataTableOutput(ns("cs_upload_table"))
      )
    )
  ))
}

#' upload_cs Server Functions
#'
#' @noRd
mod_upload_cs_server <- function(input, output, session) {
  ns <- session$ns
  
  output$cs_upload_table <- DT::renderDataTable({
    
    req(input$file_cs)
    
    tryCatch(
      {
        upload_cs_df <- read.csv(input$file_cs$datapath,
                       header = input$cs_upload_header,
                       sep = input$cs_upload_sep_radio)
        
        required_columns <- c('latitude', 'longitude')
        column_names <- colnames(upload_cs_df)
        print(column_names)
        print(apply(upload_cs_df, 2, is.numeric))
        max_columns <- 2
        
        shiny::validate(
          need(ncol(upload_cs_df) <= max_columns, "Your data has too many columns"),
          need(all(required_columns %in% column_names), "You don't have the right column names in your data"),
          need(apply(upload_cs_df, 2, is.numeric), "You don't have the right data type")
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    DT::datatable(
      upload_cs_df,
      selection = "single",
      filter = 'top',
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = "500px",
        paging = TRUE,
        columnDefs = list(list(
          className = 'dt-center', targets = "_all"
        )),
        dom = 'Bflrtip',
        buttons = c(('colvis')),
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#EBECEC', 'color': '#000'});",
          "}"
        )
      ),
      class = 'nowrap display'
    )
  })
  
  # Return values -------------
  return (list (
    "rvData" = rvData,
    "mapProxy" = leaflet::leafletProxy(mapId = "wa_road_map")
  ))
}
## To be copied in the UI
# mod_upload_cs_ui("upload_cs_ui_1")

## To be copied in the server
# mod_upload_cs_server("upload_cs_ui_1")
