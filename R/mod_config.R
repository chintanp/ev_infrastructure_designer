#' config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::bs4Card(
      tags$div(id = 'leadTextDiv',
               p(
                 id = "leadText",
                 class = "text-muted",
                 paste(
                   "Create list of new sites for charging stations by clicking on the map"
                 )
               )),
      width = NULL,
      title = "New Site List",
      solidHeader = TRUE,
      closable = FALSE,
      status = "danger",
      tags$div(id = 'siteEditor'),
      tags$div(
        id = 'siteEditorBtns',
        shinyWidgets::actionBttn(inputId = ns("newSiteBtn"), label = "Click to enter new sites")
      ),
      tags$div(id = ns("postSubmit"))
    ),
    verbatimTextOutput("user_info")
  )
}

#' config Server Function
#'
#' @noRd
mod_config_server <- function(input, output, session) {
  ns <- session$ns
  
}

## To be copied in the UI
# mod_config_ui("config_ui_1")

## To be copied in the server
# callModule(mod_config_server, "config_ui_1")
