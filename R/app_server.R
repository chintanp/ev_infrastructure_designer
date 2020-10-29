#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  session$onFlushed(function() {
    print("Called now")
    
    user_name <- session$userData$auth0_info$name
    email <- session$userData$auth0_info$email
    
    # browser()
    
    insertUI(
      selector = "#userInfo",
      ui = tags$div(
        status = "primary",
        tags$h5(email),
        auth0::logoutButton()
        
      )
    )
  })
  # List the first level callModules here
  # GlobalData contains the globals and
  # these need to be passed to the modules in need
  GlobalData = callModule(GlobalModule, "globals")
  mapData <- callModule(mod_mapdes_server, "mapdes_ui_1", GlobalData)
  
  gParamData <- mod_globals_params_server("globals_params_ui_1", GlobalData)
  tParamData <- mod_tripgen_params_server("tripgen_params_ui_1", GlobalData)
  eParamData <- mod_eviabm_params_server("eviabm_params_ui_1", GlobalData)
  callModule(mod_config_server, "config_ui_1", 
             GlobalData, mapData, gParamData, tParamData, eParamData)
}
