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
                 id = ("leadText"),
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
               tags$div(id = ("postSubmit"))),
      verbatimTextOutput("user_info")
    )
  )
}


#' config Server Function
#'
#' @noRd
mod_config_server <-
  function(input,
           output,
           session,
           globals,
           mapData,
           gParamData,
           tParamData,
           eParamData) {
    ns <- session$ns
    lead_exists <- TRUE
    setText_exists <- FALSE
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
      # print("fired")
      print(mapData$rvData$siteID)
      
      if (mapData$rvData$siteID > 0) {
        removeUI(selector = "#postSubmitText")
        if (!lead_exists) {
          insertUI(
            selector = '#leadTextDiv',
            ui = p(
              id = "leadText",
              class = "text-muted",
              paste(
                "Create list of new sites for charging stations by clicking on the map"
              )
            )
          )
          lead_exists <<- TRUE
        }
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
            tags$div(
              id = ("setRadioText"),
              shinyWidgets::prettyRadioButtons(
                ns("set_radio"),
                label = "Choose set",
                choices = list("Previous" = 2, "New" = 1),
                selected = 1,
                inline = TRUE,
                status = "danger"
              )
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
        shinyjs::disable(ns("tesla_toggle"))
      }
    })
    
    observeEvent(input$set_radio, {
      # browser()
      print("Set radio: ")
      print(input$set_radio)
      
      if ((input$set_radio == "1") &
          (setText_exists == FALSE)) {
        print("New")
        # This is the choice of creating a new set
        # removeUI(selector = '#postSubmitText')
        insertUI(selector = '#setRadioText',
                 ui = tags$div( id = "setTextDiv",
                   textInput(
                     ns("setText"),
                     label = NULL,
                     placeholder = "Enter new set description",
                     value = "Enter new set description"
                   )
                 ))
        setText_exists <<- TRUE
      } else if ((input$set_radio == "2") &
                 (setText_exists == TRUE)) {
        print("Old")
        removeUI(selector = "#setTextDiv")
        setText_exists <<- FALSE
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
          subset(mapData$rvData$siteIDs,!(mapData$rvData$siteIDs %in% site_id))
        
        # mapData$rvData$siteDetailsDF <- mapData$rvData$siteDetailsDF[-c(site_id), ]
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
    
    # Submit Btn click -------------
    observeEvent(input$submit_btn, {
      # browser()
      print("New submission")
      pool <- globals$stash$pool
      dt_submit <- Sys.time()
      user_email <- session$userData$auth0_info$email
      
      # browser()
      # Get global, tripgen and eviabm parameter updates
      gParamUpdates <- globals$stash$global_params
      # browser()
      tParamUpdates <- globals$stash$tripgen_params
      eParamUpdates <- globals$stash$eviabm_params
      
      # Form queries
      transactionQueries <-
        formTransactionQueries(gParamUpdates,
                               tParamUpdates,
                               eParamUpdates)
      
      # browser()
      if (input$set_radio == '1') {
        query_set <- transactionQueries$set_query
      }
      query_analysis <- transactionQueries$analysis_query
      query_user <- transactionQueries$user_query
      new_evse_query <- transactionQueries$new_evse_query
      query_ap <- transactionQueries$param_query
      
      print("queries")
      if (input$set_radio == '1') {
        print(query_set)
      }
      print("-----------------------------------------")
      print(query_analysis)
      print("-----------------------------------------")
      print(query_user)
      print("-----------------------------------------")
      print(new_evse_query)
      print("-------------------")
      print(query_ap)
      
      # browser()
      conn <- pool::poolCheckout(pool)
      DBI::dbBegin(conn)
      if (input$set_radio == '1') {
        DBI::dbExecute(conn, query_set)
      }
      DBI::dbExecute(conn, query_analysis)
      DBI::dbExecute(conn, query_user)
      # Only execute the new_evse_query if new chargers are added
      # otherwise this is a base-case analysis
      if (length(mapData$rvData$siteIDs) > 0) {
        DBI::dbExecute(conn, new_evse_query)
      }
      DBI::dbExecute(conn, query_ap)
      DBI::dbCommit(conn)
      pool::poolReturn(conn)
      insertUI(
        selector = "#postSubmit",
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
          shinyWidgets::actionBttn(
            inputId = ns("postSubmitBtn"),
            label = "Run another analysis",
            style = "material-flat",
            color = "success",
            size = "lg"
          )
        )
      )
      removeUI(selector = '#leadText')
      lead_exists <<- FALSE
      resetNewStations()
      removeSubmitResetBtns()
    })
    
    formTransactionQueries <-
      function(gParamUpdates,
               tParamUpdates,
               eParamUpdates) {
        if (input$set_radio == '1') {
          set_query <- formSetQuery()
        } else {
          set_query <- NULL
        }
        new_evse_query <- formNewEVSEQuery()
        analysis_query <- formAnalysisQuery()
        user_query <- formUserQuery()
        param_query <-
          formParamQuery(gParamUpdates, tParamUpdates, eParamUpdates)
        
        return (
          list (
            set_query = set_query,
            new_evse_query = new_evse_query,
            analysis_query = analysis_query,
            user_query = user_query,
            param_query = param_query
          )
        )
      }
    
    formSetQuery <- function() {
      # browser()
      print(input$setText)
      
      new_set_query <-
        glue::glue("insert into analysis_sets (description) values ('{input$setText}');")
      return (new_set_query)
    }
    
    formNewEVSEQuery <- function() {
      rest_new_evse_query <- ''
      trans_values <- c()
      
      for (site_id in mapData$rvData$siteIDs) {
        rest_new_evse_query <-
          glue::glue(
            rest_new_evse_query,
            "(currval('analysis_record_analysis_id_seq'), {mapData$rvData$siteDetailsDF[site_id, 'trip_count']}, ",
            "'",
            mapData$rvData$siteDetailsDF[site_id, "od_pairs"],
            "', {mapData$rvData$siteDetailsDF[site_id, 'latitude']}, {mapData$rvData$siteDetailsDF[site_id, 'longitude']}, {input[[paste0('dcfc_plug_count', site_id)]]}, {input[[paste0('dcfc_plug_power', site_id)]]}, {input[[paste0('level2_plug_count', site_id)]]}, {input[[paste0('level2_plug_power', site_id)]]},
            {input[[paste0('fixed_charging_price_slider', site_id)]]}, '",
            input[[paste0("dd_var_charging_unit", site_id)]],
            "', {input[[paste0('var_charging_price_slider', site_id)]]}, {input[[paste0('fixed_parking_price_slider', site_id)]]}, '",
            input[[paste0("dd_var_parking_unit", site_id)]],
            "', {input[[paste0('var_parking_price_slider', site_id)]]}, {input[[paste0('level2_fixed_charging_price_slider', site_id)]]}, '",
            input[[paste0("dd_level2_var_charging_unit", site_id)]],
            "', {input[[paste0('level2_var_charging_price_slider', site_id)]]}, {input[[paste0('level2_fixed_parking_price_slider', site_id)]]}, '",
            input[[paste0("dd_level2_var_parking_unit", site_id)]],
            "', {input[[paste0('level2_var_parking_price_slider', site_id)]]}, {input[[paste0('dcfc_plug_type', site_id)]]}), "
          )
      }
      # remove the last comma
      rest_new_evse_query <-
        substr(rest_new_evse_query, 1, nchar(rest_new_evse_query) - 2)
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
      
      return (new_evse_query)
    }
    
    formAnalysisQuery <- function() {
      req(session$userData$auth0_info$sub)
      auth0_sub <- session$userData$auth0_info$sub
      auth0_userid <-
        strsplit(auth0_sub, "|", fixed = TRUE)[[1]][2]
      
      query_analysis <-
        glue::glue(
          "INSERT INTO analysis_record (user_id, status, include_tesla, set_id) VALUES
                                    ('{auth0_userid}', 'inserted', '{input$tesla_toggle}',
                                      (SELECT last_value
                                        FROM analysis_sets_set_id_seq));"
        )
      
      return (query_analysis)
    }
    
    formUserQuery <- function() {
      req(session$userData$auth0_info$sub)
      auth0_sub <- session$userData$auth0_info$sub
      auth0_userid <-
        strsplit(auth0_sub, "|", fixed = TRUE)[[1]][2]
      user_name <- session$userData$auth0_info$name
      user_email <- session$userData$auth0_info$email
      
      
      query_user <-
        glue::glue(
          "INSERT INTO user_details (user_id, user_name, email_id) VALUES
                                    ('{auth0_userid}', '{user_name}', '{user_email}')
                                    ON CONFLICT (user_id) DO UPDATE SET last_submit_date = NOW();"
        )
      
      return (query_user)
    }
    
    formParamQuery <-
      function(gParamUpdates,
               tParamUpdates,
               eParamUpdates) {
        # browser()
        query_ap_rest <- ''
        
        for (i in 1:nrow(gParamUpdates)) {
          query_ap_rest <-
            paste0(
              query_ap_rest,
              "(currval('analysis_record_analysis_id_seq'), ",
              gParamUpdates$param_id[i],
              ", '",
              gParamUpdates$param_value[i],
              "' ),"
            )
        }
        
        for (i in 1:nrow(tParamUpdates)) {
          query_ap_rest <-
            paste0(
              query_ap_rest,
              "(currval('analysis_record_analysis_id_seq'), ",
              tParamUpdates$param_id[i],
              ", '",
              tParamUpdates$param_value[i],
              "' ),"
            )
        }
        
        for (i in 1:nrow(eParamUpdates)) {
          query_ap_rest <-
            paste0(
              query_ap_rest,
              " (currval('analysis_record_analysis_id_seq'), ",
              eParamUpdates$param_id[i],
              ", '",
              eParamUpdates$param_value[i],
              "' ),"
            )
        }
        
        # remove the comma at the end
        query_ap_rest_1 <-
          substr(query_ap_rest, 1, nchar(query_ap_rest) - 1)
        
        query_ap <-
          glue::glue(
            "INSERT INTO analysis_params (analysis_id, param_id, param_value) VALUES
                    {query_ap_rest_1};"
          )
        
        return (query_ap)
        
      }
    
    resetNewStations <- function() {
      for (site_id in mapData$rvData$siteIDs) {
        removeUI(selector = paste0("#siterow", site_id))
        mapData$mapProxy %>%
          leaflet::removeMarker(layerId = as.character(site_id))
      }
      mapData$rvData$siteIDs <- c()
      mapData$rvData$siteID <- 0
      mapData$rvData$siteDetailsDF <-
        mapData$rvData$siteDetailsDF[0, ]
    }
    
    removeSubmitResetBtns <- function() {
      print("remove submit reset Btns")
      removeUI(selector = '#submitResetBtns')
    }
    
    observeEvent(input$postSubmitBtn, {
      removeUI(selector = '#postSubmitText')
      insertUI(selector = '#leadTextDiv',
               ui = p(
                 id = "leadText",
                 class = "text-muted",
                 paste(
                   "Create list of new sites for charging stations by clicking on the map"
                 )
               ))
      
      lead_exists <<- TRUE
      # clearAllMarkers()
    })
  }
