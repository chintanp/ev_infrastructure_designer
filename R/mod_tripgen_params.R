#' tripgen_params UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tripgen_params_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::bs4Card(
      title = "tripgen Parameters",
      #paste0("Washington State DCFC Network (last updated: ",setup_data$map_data$cs_data$last_updated_date,")"),
      closable = FALSE,
      status = "secondary",
      collapsible = TRUE,
      labelTooltip = "Set the tripgen simulation params here",
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      dropdownMenu = bs4Dash::dropdownItemList(bs4Dash::dropdownItem(name = ""),
                                               icon = "question-circle"),
      maximizable = TRUE,
      uiOutput(ns("tripgen_params"))
    )
  )
}

#' tripgen_params Server Functions
#'
#' @noRd
mod_tripgen_params_server <- function(id, globals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$tripgen_params <- renderUI({
      tripgen_params <- globals$stash$tripgen_params
      req(tripgen_params)
      
      # print(global_params)
      # browser()
      
      tagList(# Generate the list of elements dynamically - explanation: https://stackoverflow.com/a/32089489/1328232
        lapply(1:nrow(tripgen_params), function(i) {
          if (!is.na(as.numeric(tripgen_params$param_value[i]))) {
            pv <- as.numeric(tripgen_params$param_value[i])
            plb <-  as.numeric(tripgen_params$param_lower_bound[i])
            pub <-  as.numeric(tripgen_params$param_upper_bound[i])
          }
          
          tagList(fluidRow(
            sliderInput(
              inputId = ns(paste0("slider_", i)),
              label = tripgen_params$param_name[i],
              min = plb,
              max = pub,
              value = pv,
              step = 1,
              round = TRUE
            )
          ))
        }))
    })
    
    return ( list(
      # Function to send the updated parameters out
      getTripgenParams = function() {
        tripgen_params <- globals$stash$tripgen_params
        req(tripgen_params)
        
        tripgen_parameter_updates <-
          sapply(1:nrow(tripgen_params), function(i) {
            # input[[paste0("select_", i)]]
            parameter_id <- tripgen_params$param_id[i]
            # parameter_name <- rvData$parameter_df$parameter_name[i]
            updated_parameter_value <- input[[paste0("slider_", i)]]
            # print(parameter_id)
            # print(updated_parameter_value)
            
            return (c(parameter_id, updated_parameter_value))
          })
        # print(global_parameter_updates)
        return (tripgen_parameter_updates)
      }
    ))
  })
}

## To be copied in the UI
# mod_tripgen_params_ui("tripgen_params_ui_1")

## To be copied in the server
# mod_tripgen_params_server("tripgen_params_ui_1")
