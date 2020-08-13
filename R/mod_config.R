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
                 id = ns("leadText"),
                 class = "text-muted",
                 paste(
                   "Create list of new sites for charging stations by clicking on the map"
                 )
               )),
      width = NULL,
      title = "New Site List",
      solidHeader = TRUE,
      dropdownMenu = bs4Dash::dropdownItemList(
        bs4Dash::dropdownItem(name = "Click the green button to configure the charging station"),
        bs4Dash::dropdownItem(name = "Click the red button to delete the charging station"),
        bs4Dash::dropdownItem(name = "Click the 'Submit' start the analysis"),
        icon = "question-circle"
      ),
      closable = FALSE,
      status = "danger",
      tags$div(id = 'siteEditor'),
      tags$div(id = ('submitReset'),
               # shinyWidgets::actionBttn(inputId = ns("newSiteBtn"), label = "Click to enter new sites")),
               tags$div(id = ns("postSubmit"))),
      verbatimTextOutput("user_info")
    )
  )
}

#' config Server Function
#'
#' @noRd
mod_config_server <-
  function(input, output, session, globals, mapData) {
    ns <- session$ns
    
    # This function generates the site row UI,
    # this includes a configuration button and a delete button
    siteRowUi <- function(site_id) {
      tags$div(id = paste0("siterow", site_id),
               fluidRow(
                 column(6,
                        tags$h5(paste0(
                          "Site ID: ", site_id
                        ))),
                 column(
                   3,
                   shinyWidgets::dropdownButton(
                     makeConfigUI(site_id),
                     inputId = ns(paste0("configBtn", site_id)),
                     circle = FALSE,
                     status = "success",
                     icon = icon("sliders"),
                     width = "900px",
                     tooltip = "Click to configure site details !",
                     #shinyWidgets::tooltipOptions(title = "Click to enter the site details !", placement = "left"),
                     right = TRUE,
                     up = FALSE,
                     margin = "10px",
                     size = "sm"
                   )
                 ),
                 column(
                   3,
                   shinyWidgets::actionBttn(
                     inputId = ns(paste0("removeBtn", site_id)),
                     label = NULL,
                     style = "material-circle",
                     color = "danger",
                     icon = icon("remove"),
                     size = "sm"
                   )
                 )
               ))
    }
    # When a map marker is clicked, add a new row on the config tab
    observeEvent(mapData$rvData$siteID, {
      print("fired")
      print(mapData$rvData$siteID)
      if (mapData$rvData$siteID > 0) {
        # removeUI(selector = '#leadText')
        insertUI(
          selector = '#siteEditor',
          where = 'afterBegin',
          ui = tags$div(siteRowUi(mapData$rvData$siteID))
        )
        attachRemoveObserver(mapData$rvData$siteID)
        
      }
      if (mapData$rvData$siteID == 1) {
        # removeUI(selector = '#leadText')
        insertUI(
          selector = '#submitReset',
          where = 'afterEnd',
          ui = tags$div(
            id = "submitResetBtns",
            hr(),
            shinyWidgets::materialSwitch(
              inputId = ns("tesla_toggle"),
              label = "Tesla",
              value = FALSE,
              status = "primary"
            ),
            hr(),
            fluidRow(
              column(
                6,
                shinyWidgets::actionBttn(
                  inputId = ns("submit_btn"),
                  label = "Submit",
                  style = "material-flat",
                  color = "primary",
                  size = "md"
                )
              ),
              column(
                6,
                shinyWidgets::actionBttn(
                  inputId = ns("reset_btn"),
                  label = "Reset",
                  style = "material-flat",
                  color = "default",
                  size = "md"
                )
              )
            )
          )
        )
      }
    })
    
    attachRemoveObserver <- function(site_id) {
      observeEvent(input[[paste0("removeBtn", site_id)]], {
        print("Button clicked")
        print(site_id)
        
        mapData$mapProxy %>%
          leaflet::removeMarker(layerId = as.character(site_id))
        
        removeUI(selector = paste0("#siterow", site_id))
        # Remove the site_id from the siteIDs
        mapData$rvData$siteIDs <-
          subset(mapData$rvData$siteIDs,
                 !(mapData$rvData$siteIDs %in% site_id))
      })
    }
    
    # This function makes the config UI
    makeConfigUI <- function(site_id) {
      tags$div(
        tags$h4(paste0(
          "Enter the station ", site_id, " details"
        ),
        style = "color: purple;"),
        hr(style = "border-color: purple;"),
        fluidRow(column(
          12,
          shinyWidgets::prettyRadioButtons(
            ns(paste0("dcfc_plug_type", site_id)),
            label = "Type of DCFC plug",
            choices = list(
              "CHAdeMO only" = 1,
              "COMBO only" = 2,
              "Both CHAdeMO and COMBO" = 3
            ),
            selected = 3,
            inline = TRUE
          )
        )),
        hr(),
        fluidRow(
          column(
            3,
            numericInput(
              inputId = ns(paste0("dcfc_plug_count", site_id)),
              label = "Number of plugs",
              value = 1,
              min = 0
            )
          ),
          column(
            3,
            sliderInput(
              inputId = ns(paste0("dcfc_plug_power", site_id)),
              label = 'Power per plug (kW)',
              value = 50,
              min = 10,
              max = 500,
              step = 10
            )
          ),
          column(3,
                 wellPanel(
                   column(
                     12,
                     sliderInput(
                       inputId = ns(paste0(
                         "fixed_charging_price_slider", site_id
                       )),
                       label = 'Fixed Charging Price ($)',
                       value = 0.5,
                       min = 0,
                       max = 10,
                       step = 0.1
                     )
                   ),
                   column(12,
                          selectInput(
                            inputId = ns(paste0("dd_var_charging_unit", site_id)),
                            choices = c("min", "kWh"),
                            label = "Unit"
                          )),
                   column(
                     12,
                     sliderInput(
                       inputId = ns(paste0(
                         "var_charging_price_slider", site_id
                       )),
                       label = 'Variable Charging Price ($)',
                       value = 0.5,
                       min = 0,
                       max = 10,
                       step = 0.1
                     )
                   )
                 )),
          column(3,
                 wellPanel(
                   column(
                     12,
                     sliderInput(
                       inputId = ns(paste0(
                         "fixed_parking_price_slider", site_id
                       )),
                       label = 'Fixed Parking Price ($)',
                       value = 0.5,
                       min = 0,
                       max = 10,
                       step = 0.1
                     ),
                     column(12,
                            selectInput(
                              inputId = ns(paste0("dd_var_parking_unit", site_id)),
                              choices = c("min", "hour"),
                              label = "Unit"
                            )),
                     column(
                       12,
                       sliderInput(
                         inputId = ns(paste0("var_parking_price_slider", site_id)),
                         label = 'Variable Parking Price ($)',
                         value = 0.5,
                         min = 0,
                         max = 10,
                         step = 0.1
                       )
                     )
                   )
                 ))
        ),
        hr(),
        fluidRow(
          column(
            3,
            numericInput(
              inputId = ns(paste0("level2_plug_count", site_id)),
              label = "Number of Level-2 plugs",
              value = 1,
              min = 0
            )
          ),
          column(
            3,
            sliderInput(
              inputId = ns(paste0("level2_plug_power", site_id)),
              label = 'Power per Level-2 plug',
              value = 10,
              min = 1,
              max = 19.2,
              step = 0.1
            )
          ),
          column(3,
                 wellPanel(
                   column(
                     12,
                     sliderInput(
                       inputId = ns(paste0(
                         "level2_fixed_charging_price_slider", site_id
                       )),
                       label = 'Fixed Charging Price ($)',
                       value = 0.5,
                       min = 0,
                       max = 10,
                       step = 0.1
                     )
                   ),
                   column(12,
                          selectInput(
                            inputId = ns(paste0(
                              "dd_level2_var_charging_unit", site_id
                            )),
                            choices = c("min", "kWh"),
                            label = "Unit"
                          )),
                   column(
                     12,
                     sliderInput(
                       inputId = ns(paste0(
                         "level2_var_charging_price_slider", site_id
                       )),
                       label = 'Variable Charging Price ($)',
                       value = 0.5,
                       min = 0,
                       max = 10,
                       step = 0.1
                     )
                   )
                 )),
          column(3,
                 wellPanel(
                   column(
                     12,
                     sliderInput(
                       inputId = ns(paste0(
                         "level2_fixed_parking_price_slider", site_id
                       )),
                       label = 'Parking Price ($)',
                       value = 0.5,
                       min = 0,
                       max = 10,
                       step = 0.1
                     )
                   ),
                   column(12,
                          selectInput(
                            inputId = ns(paste0(
                              "dd_level2_var_parking_unit", site_id
                            )),
                            choices = c("min", "hour"),
                            label = "Unit"
                          )),
                   column(
                     12,
                     sliderInput(
                       inputId = ns(paste0(
                         "level2_var_parking_price_slider", site_id
                       )),
                       label = 'Variable Parking Price ($)',
                       value = 0.5,
                       min = 0,
                       max = 10,
                       step = 0.1
                     )
                   )
                 ))
        )
      )
    }
    
    # Reset Btn click -------------
    # Remove all markers and all site rows
    observeEvent(input$reset_btn, {
      resetNewStations()
      removeSubmitResetBtns()
    })
    
    observeEvent(input$submit_btn, {
      browser()
      dt_submit <- Sys.time()
      pool <- globals$stash$pool
      
      auth0_sub <- session$userData$auth0_info$sub
      auth0_userid <-
        strsplit(auth0_sub, "|", fixed = TRUE)[[1]][2]
      user_name <- session$userData$auth0_info$name
      user_email <- session$userData$auth0_info$email
      
      rest_new_evse_query <- ''
      trans_values <- list()
      
      for (site_id in mapData$rvData$siteIDs) {
        # siteDetailsDF[i, 6:22] <-
        #   c(
        #     input[[paste0("dcfc_plug_type", site_id)]],
        #     input[[paste0("dcfc_plug_count", site_id)]],
        #     input[[paste0("dcfc_plug_power", site_id)]],
        #     input[[paste0("fixed_charging_price_slider", site_id)]],
        #     input[[paste0("dd_var_charging_unit", site_id)]],
        #     input[[paste0("var_charging_price_slider", site_id)]],
        #     input[[paste0("fixed_parking_price_slider", site_id)]],
        #     input[[paste0("dd_var_parking_unit", site_id)]],
        #     input[[paste0("var_parking_price_slider", site_id)]],
        #     input[[paste0("level2_plug_count", site_id)]],
        #     input[[paste0("level2_plug_power", site_id)]],
        #     input[[paste0("level2_fixed_charging_price_slider", site_id)]],
        #     input[[paste0("dd_level2_var_charging_unit", site_id)]],
        #     input[[paste0("level2_var_charging_price_slider", site_id)]],
        #     input[[paste0("level2_fixed_parking_price_slider", site_id)]],
        #     input[[paste0("dd_level2_var_parking_unit", site_id)]],
        #     input[[paste0("level2_var_parking_price_slider", site_id)]]
        #   )
        # rest_new_evse_query <- glue::glue(rest_new_evse_query, '(?a_id, ?trip_count{site_id}, ?od_pairs{site_id}, ?lat{site_id}, ?lng{site_id}, 
        #                                   ?dcfc_plug_cnt{site_id}, ?dcfc_power{site_id}, ?level2_plug_count{site_id}, ?level2_power{site_id}, 
        #                         ?dcfc_fixed_charging_price{site_id}, ?dcfc_var_charging_price_unit{site_id}, 
        #                         ?dcfc_var_charging_price{site_id}, ?dcfc_fixed_parking_price{site_id}, ?dcfc_var_parking_price_unit{site_id}, 
        #                         ?dcfc_var_parking_price{site_id}, ?level2_fixed_charging_price{site_id}, ?level2_var_charging_price_unit{site_id}, 
        #                         ?level2_var_charging_price{site_id}, ?level2_fixed_parking_price{site_id}, ?level2_var_parking_price_unit{site_id}, 
        #                         ?level2_var_parking_price{site_id}, ?connector_code{site_id}), ') 
        rest_new_evse_query <- glue::glue(rest_new_evse_query, '(lastval(), $1, $2, $3, $4, 
                                          $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, 
                                          $17, $18, $19, $20, $21), ') 
        trans_values <- append(trans_values, c(mapData$rvData$bufferRoad$trip_count, mapData$rvData$bufferRoad$od_pairs, mapData$rvData$click$lat, mapData$rvData$click$lng, 
                                               input[[paste0("dcfc_plug_count", site_id)]],
                                               input[[paste0("dcfc_plug_power", site_id)]],
                                               input[[paste0("level2_plug_count", site_id)]],
                                               input[[paste0("level2_plug_power", site_id)]],                       
                                               input[[paste0("fixed_charging_price_slider", site_id)]],
                                               input[[paste0("dd_var_charging_unit", site_id)]],
                                               input[[paste0("var_charging_price_slider", site_id)]],
                                               input[[paste0("fixed_parking_price_slider", site_id)]],
                                               input[[paste0("dd_var_parking_unit", site_id)]],
                                               input[[paste0("var_parking_price_slider", site_id)]],    
                                               input[[paste0("level2_fixed_charging_price_slider", site_id)]],
                                               input[[paste0("dd_level2_var_charging_unit", site_id)]],
                                               input[[paste0("level2_var_charging_price_slider", site_id)]],
                                               input[[paste0("level2_fixed_parking_price_slider", site_id)]],
                                               input[[paste0("dd_level2_var_parking_unit", site_id)]],
                                               input[[paste0("level2_var_parking_price_slider", site_id)]], 
                                               input[[paste0("dcfc_plug_type", site_id)]]))
      }
      # remove the last comma
      rest_new_evse_query <- substr(rest_new_evse_query, 1, nchar(rest_new_evse_query) - 2)
      new_evse_query <-
        glue::glue(
          "INSERT INTO new_evses (analysis_id, trip_count, od_pairs, latitude, longitude, 
                                dcfc_plug_count, dcfc_power, level2_plug_count, level2_power, 
                                dcfc_fixed_charging_price, dcfc_var_charging_price_unit, 
                                dcfc_var_charging_price, dcfc_fixed_parking_price, dcfc_var_parking_price_unit, 
                                dcfc_var_parking_price, level2_fixed_charging_price, level2_var_charging_price_unit, 
                                level2_var_charging_price, level2_fixed_parking_price, level2_var_parking_price_unit, 
                                level2_var_parking_price, connector_code) VALUES 
          {rest_new_evse_query}"
        )
      trans_query <- ''
      trans_query <- glue::glue("BEGIN;
                                  INSERT INTO analysis_record (user_id, status, include_tesla) VALUES
                                    ('{auth0_userid}', 'inserted', '{input$tesla_toggle}');
                                {new_evse_query};  
                                  INSERT INTO user_details (user_id, user_name, email_id) VALUES 
                                    ('{auth0_userid}', '{user_name}', '{user_email}')
                                    ON CONFLICT (user_id) DO UPDATE SET last_submit_date = NOW();
                                COMMIT;")
      print("query")
      print(trans_query)
      print("-----------------------------------------")
      print("values")
      print(trans_values)
      conn <- pool::poolCheckout(pool);
      trans_sendQuery <- DBI::dbSendQuery(conn, trans_query)
      DBI::dbBind(trans_sendQuery, trans_values)
      DBI::dbFetch(trans_sendQuery)
      pool::poolReturn(conn)
      insertUI(selector = "#postSubmit",
               ui = tags$div(
                 id = "postSubmitText",
                 p(
                   paste0(
                     "The input has been submitted for analysis. The analysis was submitted at: ",
                     dt_submit,
                     ". An email will be sent to your registered email id - ",
                     user_email,
                     " when
                    analysis results are ready."
                   )
                 ),
                 shinyWidgets::actionBttn(inputId = "postSubmitBtn", label = "Run another analysis")
               ))
      
      resetNewStations()
    })
    
    resetNewStations <- function() {
      for (site_id in mapData$rvData$siteIDs) {
        removeUI(selector = paste0("#siterow", site_id))
        mapData$mapProxy %>%
          leaflet::removeMarker(layerId = as.character(site_id))
      }
      mapData$rvData$siteIDs <- c()
      mapData$rvData$siteID <- 0
    }
    
    removeSubmitResetBtns <- function() {
      print("remove submit reset Btns")
      removeUI(selector = '#submitResetBtns')
    }
  }

## To be copied in the UI
# mod_config_ui("config_ui_1")

## To be copied in the server
# callModule(mod_config_server, "config_ui_1")
