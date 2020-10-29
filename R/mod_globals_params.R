#' globals_params UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_globals_params_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::bs4Card(
      title = "Global Parameters",
      #paste0("Washington State DCFC Network (last updated: ",setup_data$map_data$cs_data$last_updated_date,")"),
      closable = FALSE,
      status = "primary",
      collapsible = TRUE,
      labelTooltip = "Set the global simulation params here",
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      dropdownMenu = bs4Dash::dropdownItemList(bs4Dash::dropdownItem(name = ""),
                                               icon = "question-circle"),
      maximizable = TRUE,
      uiOutput(ns("global_params"))
    )
  )
}

#' globals_params Server Functions
#'
#' @noRd
mod_globals_params_server <- function(id, globals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$global_params <- renderUI({
      global_params <- globals$stash$global_params
      
      req(global_params)
      
      # print(global_params)
      # browser()
      
      tagList(# Generate the list of elements dynamically - explanation: https://stackoverflow.com/a/32089489/1328232
        lapply(1:nrow(global_params), function(i) {
          tagList(fluidRow(
            sliderInput(
              inputId = ns(paste0("slider_", i)),
              label = global_params$param_name[i],
              min = as.numeric(global_params$param_lower_bound[i]),
              max = as.numeric(global_params$param_upper_bound[i]),
              value = as.numeric(global_params$param_value[i]),
              step = 1,
              round = TRUE
            )
          ))
        }))
    })
    
    return ( list(
      # Function to send the updated parameters out
      getGlobalParams = function() {
        global_params <- globals$stash$global_params
        req(global_params)
        
        global_parameter_updates <-
          sapply(1:nrow(global_params), function(i) {
            # input[[paste0("select_", i)]]
            parameter_id <- global_params$param_id[i]
            # parameter_name <- rvData$parameter_df$parameter_name[i]
            updated_parameter_value <- input[[paste0("slider_", i)]]
            # print(parameter_id)
            # print(updated_parameter_value)
            
            return (c(parameter_id, updated_parameter_value))
          })
        # print(global_parameter_updates)
        return (global_parameter_updates)
      }
    ))
  })
}

## To be copied in the UI
# mod_globals_params_ui("globals_params_ui_1")

## To be copied in the server
# mod_globals_params_server("globals_params_ui_1")
