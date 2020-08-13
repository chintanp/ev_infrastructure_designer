#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  # GlobalData contains the globals and
  # these need to be passed to the modules in need
  GlobalData = callModule(GlobalModule, "globals")
  mapData <- callModule(mod_mapdes_server, "mapdes_ui_1", GlobalData)
  callModule(mod_config_server, "config_ui_1", GlobalData, mapData)
}
