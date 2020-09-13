library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(leaflet)
library(dplyr)
library(httr)
library(curl) # make the jsonlite suggested dependency explicit
library(rgdal)
library(sp)
library(htmltools)
library(Hmisc)
library(RColorBrewer)
library(shinythemes)
library(readr)
library(rgeos)
library(hash)
library(shinycssloaders)
library(ulid)
library(here)
library(stringi)
library(DT)
library(auth0)
library(bs4Dash)
library(leaflet.extras)
# library(leafgl)
library(sf)
library(DBI)
library(RPostgres)
library(pool)
library(png)
library(leaflet.mapboxgl)

# AWS EC2 specific
# Get the ip address of the local instance
# options(shiny.host = as.character(system("wget -qO- http://instance-data/latest/meta-data/local-ipv4", intern = TRUE)))
options(shiny.port = 8100)
options(mapbox.accessToken = "pk.eyJ1IjoiY2hpbnRhbnAiLCJhIjoiY2ppYXU1anVuMThqazNwcDB2cGtneDdkYyJ9.TL6RTyRRFCbvJWyFa4P0Ow" )

setup = function() {
  database = function() {
    main_con <- pool::dbPool(
      RPostgres::Postgres(),
      host = Sys.getenv("MAIN_HOST"),
      dbname = Sys.getenv("MAIN_DB"),
      user = Sys.getenv("MAIN_USER"),
      password = Sys.getenv("MAIN_PWD"),
      port = Sys.getenv("MAIN_PORT")
    )
    
    return (main_con)
  }
  main_con = database()
  map_data = create_map_layers(main_con)
  return(list('main_con' = main_con, 'map_data' = map_data))
}

populate_charging_stations = function(main_con) {
  bevses_db <- main_con %>% dplyr::tbl("built_evse")
  
  evse_dcfc <-
    bevses_db %>% dplyr::select(dcfc_count,
                                latitude,
                                longitude,
                                bevse_id,
                                connector_code) %>% dplyr::filter(dcfc_count > 0) %>% collect()
  
  last_updated_date <-
    main_con %>% DBI::dbGetQuery("select last_updated from table_stats where table_name = 'built_evse';")
  
  all_chargers_combo <-
    evse_dcfc[evse_dcfc$connector_code == 2 |
                evse_dcfc$connector_code == 3,]
  
  all_chargers_chademo <-
    evse_dcfc[evse_dcfc$connector_code == 1 |
                evse_dcfc$connector_code == 3,]
  
  return (list(
    'evse_dcfc' = evse_dcfc,
    'last_updated_date' = last_updated_date,
    'all_chargers_combo' = all_chargers_combo,
    'all_chargers_chademo' = all_chargers_chademo
  ))
}

create_map_layers = function(main_con) {
  cs_data = populate_charging_stations(main_con)
  
  overlay_names <-
    c("Buffer", "Combo", "CHAdeMO")
  
  base_layers <- c("Combo", "CHAdeMO")
  base_tile_layers <- c("MapBox Light", "OSM (default)")
  
  combo_icons <-
    icons(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/7/7d/Symbol_electric_vehicle_charging_stations.jpg",
      iconWidth = 10,
      iconHeight = 10,
      iconAnchorX = 0,
      iconAnchorY = 0
    )
  # Read the RDS files
  shape_trip_feasibility_chademo <-
    readRDS("data/shape_trip_feasibility_chademo.Rds")
  shape_trip_feasibility_combo <-
    readRDS("data/shape_trip_feasibility_combo.Rds")
  buf_merged <- readRDS("data/buf_merged.Rds")
  buf_critical_ll <- readRDS("data/buf_critical_ll.Rds")
  # browser()
  wa_map <-
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    setMaxBounds(-124.8361, 45.5437,-116.9174, 49.0024) %>%
    addMapboxGL(style = "mapbox://styles/mapbox/streets-v11")  %>%
    # Base groups
    addPolylines(
      data = shape_trip_feasibility_combo,
      weight = shape_trip_feasibility_combo$trip_count / 20000,
      color = "#475DCC",
      group = overlay_names[2],
      opacity = 1,
      label = paste0("trip_count : ",
                     shape_trip_feasibility_combo$trip_count),
      popup = paste0("trip_count : ",
                     shape_trip_feasibility_combo$trip_count)
    ) %>%
    addPolylines(
      data = shape_trip_feasibility_chademo,
      weight = shape_trip_feasibility_chademo$trip_count / 20000,
      color = "#F23D3D",
      group = overlay_names[3],
      opacity = 1,
      label = paste0("trip_count : ",
                     shape_trip_feasibility_chademo$trip_count),
      popup = paste0("trip_count : ",
                     shape_trip_feasibility_chademo$trip_count)
    ) %>%
    addMarkers(
      lng = cs_data$all_chargers_combo$longitude ,
      lat = cs_data$all_chargers_combo$latitude,
      popup = paste0("ID : ", cs_data$all_chargers_combo$bevse_id),
      label = paste0("ID : ", cs_data$all_chargers_combo$bevse_id),
      icon = combo_icons,
      group = overlay_names[2]
    )  %>%
    addMarkers(
      lng = cs_data$all_chargers_chademo$longitude ,
      lat = cs_data$all_chargers_chademo$latitude,
      popup = paste0("ID : ", cs_data$all_chargers_chademo$bevse_id),
      label = paste0("ID : ", cs_data$all_chargers_chademo$bevse_id),
      icon = combo_icons,
      group = overlay_names[3]
    ) %>%
    addPolygons(
      data = buf_merged ,
      color = "#808080",
      group = overlay_names[1],
      opacity = 0.5
    ) %>%
    addLayersControl(overlayGroups = overlay_names,
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addResetMapButton() %>%
    addSearchOSM()
  
  return (
    list(
      'cs_data' = cs_data,
      'overlay_names' = overlay_names,
      'base_layers' = base_layers,
      'base_tile_layers' = base_tile_layers,
      'combo_icons' = combo_icons,
      'shape_trip_feasibility_chademo' = shape_trip_feasibility_chademo,
      'shape_trip_feasibility_combo' = shape_trip_feasibility_combo,
      'buf_merged' = buf_merged,
      'buf_critical_ll' = buf_critical_ll,
      'wa_map' = wa_map
    )
  )
  
}

setup_data = setup()

# # Read the EVSE information through the AFDC API
# afdc_url  <-
#   paste0(
#     "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?fuel_type=ELEC&state=WA&ev_charging_level=dc_fast&status=E&access=public&api_key=",
#     Sys.getenv('AFDC_API_KEY')
#   )
# evse_dcfc <- read_csv(afdc_url)
# # TODO: Add a fallback clause in the case the API is non-responsive
#
# evse_dcfc$EV_Connector_Code <- 0
# evse_dcfc$ChargingCost <- 0
#
# # Convert the connector type to code for easy parsing in GAMA
# # CHADEMO only - 1
# # J1772COMBO only - 2
# # CHADEMO and J1772COMBO - 3
# # TESLA - 4
# # Ignore J1772 as it is level-2
# for (i in 1:nrow(evse_dcfc)) {
#   conns <- evse_dcfc$`EV Connector Types`[i]
#   if (grepl("CHADEMO", conns)) {
#     if (grepl("J1772COMBO", conns)) {
#       evse_dcfc$EV_Connector_Code[i] <- 3
#     } else {
#       evse_dcfc$EV_Connector_Code[i] <- 1
#     }
#   } else if (grepl("J1772COMBO", conns)) {
#     evse_dcfc$EV_Connector_Code[i] <- 2
#   } else if (grepl("TESLA", conns)) {
#     evse_dcfc$EV_Connector_Code[i] <- 4
#   }
# }


# bevses_db <- dplyr::tbl(main_con, "built_evse")

# 

###################
### ui ###########
####################
# browser()
ui <- bs4DashPage(
  navbar = bs4DashNavbar(
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = "bars",
    controlbarIcon = "th",
    fixed = FALSE
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "EV Infrastructure Designer",
    brandColor = "primary",
    url = "",
    src = "",
    elevation = 3,
    opacity = 0.3,
    imageOutput("logo2", height = 66, width = 200),
    bs4SidebarMenu(
      bs4SidebarHeader(""),
      bs4SidebarMenuItem("Home",
                         tabName = "home_tab",
                         icon = "home"),
      bs4SidebarMenuItem("Parameters",
                         tabName = "param_tab",
                         icon = "sliders")
    )
  ),
  controlbar = bs4DashControlbar(skin = "light",
                                 tags$div(id = "userInfo")),
  footer = bs4DashFooter(bs4DashFooter(
    copyrights = a(
      href = "https://faculty.washington.edu/dwhm/",
      target = "_blank",
      "Chintan Pathak and Don MacKenzie, UW"
    ),
    right_text = "2019"
  )),
  title = "EV Infrastructure Designer",
  body = bs4DashBody(bs4TabItems(
    bs4TabItem(tabName = "sortabled",
               fluidRow(
                 column(
                   width = 9,
                   bs4Card(
                     title = paste0("Washington State DCFC Network (last updated: ",setup_data$map_data$cs_data$last_updated_date,")"),
                     closable = FALSE,
                     status = "primary",
                     collapsible = TRUE,
                     labelTooltip = "Washington State DCFC Network",
                     elevation = 4,
                     width = NULL,
                     solidHeader = TRUE,
                     dropdownIcon = "question-circle",
                     dropdownMenu = dropdownItemList(
                       dropdownItem(url = "https://evi-dss.readthedocs.io/en/latest/evi_des.html#washington-ev-dcfc-system", name = "Read about the overlay in the docs")),
                     maximizable = TRUE,
                     withSpinner(
                       leafletOutput("wa_road_map", height = 700, width = "100%"),
                       type = 8,
                       color = "#0dc5c1"
                     )
                   )
                   # tags$div(
                   #   width = 9,
                   #   elevation = 4,
                   #   status = "primary",
                   #   class = "table",
                   #   tags$h3("Submission Details"),
                   #   tags$div(DT::dataTableOutput(outputId = "siteDetails"),
                   #            width = 9)
                   # )
                 ),
                 column(
                   width = 3,
                   bs4Card(
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
                       actionBttn(inputId = "newSiteBtn", label = "Click to enter new sites")
                     ),
                     tags$div(id = "postSubmit")
                   ),
                   # bs4Card(
                   #   width = NULL,
                   #   solidHeader = TRUE,
                   #   status = "success",
                   #   title = "Submitted Inputs",
                   #   uiOutput(outputId = "submittedInputs")
                   # ),
                   verbatimTextOutput("user_info")
                 )
               ))
  ))
)

server <- function(input, output, session) {
  rvData <- reactiveValues(
    click = NULL,
    siteListChoiceNames = NULL,
    siteListChoiceValues = NULL,
    siteListSelected = NULL,
    siteID = 0,
    siteIDs = NULL,
    siteDisplayHash = hash(),
    siteDetailsDF = data.frame(
      input_evse_id = integer(),
      trip_count = numeric(),
      od_pairs = character(),
      latitude = numeric(),
      longitude = numeric(),
      connector_code = integer(),
      dcfc_plug_count = integer(),
      dcfc_power = numeric(),
      dcfc_fixed_charging_price = numeric(),
      dcfc_var_charging_price_unit = character(),
      dcfc_var_charging_price = numeric(),
      dcfc_fixed_parking_price = numeric(),
      dcfc_var_parking_price_unit = character(),
      dcfc_var_parking_price = numeric(),
      level2_plug_count = integer(),
      level2_power = numeric(),
      level2_fixed_charging_price = numeric(),
      level2_var_charging_price_unit = character(),
      level2_var_charging_price = numeric(),
      level2_fixed_parking_price = numeric(),
      level2_var_parking_price_unit = character(),
      level2_var_parking_price = numeric(),
      analysis_id = integer(),
      stringsAsFactors = FALSE
    ),
    submittedInputs = hash(),
    firstClick = TRUE
    
  )
  
  clearAllMarkers <- function() {
    print("New sites added now")
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      leafletProxy(mapId = "wa_road_map") %>%
        removeMarker(layerId = rvData$siteDetailsDF$input_evse_id[i])
    }
    removeUI(selector = "#newSiteBtn")
    rvData$siteDetailsDF <- rvData$siteDetailsDF[0, ]
    rvData$firstClick <- FALSE
  }
  
  observeEvent(input$radioInputs, {
    leafletProxy(mapId = "wa_road_map") %>%
      removeMarker(layerId = rvData$siteDetailsDF$input_evse_id)
    print("radio button clicked")
    output$siteDetails  <- DT::renderDataTable({
      submissionId <- input$radioInputs
      rvData$submittedInputs[[submissionId]]
    }, options = list(scrollX = TRUE, autoWidth = TRUE))
    ulid_current <- input$radioInputs
    print(ulid_current)
    auth0_sub <- session$userData$auth0_info$sub
    auth0_userid <- strsplit(auth0_sub, "|", fixed = TRUE)[[1]][2]
    
    rvData$siteDetailsDF <-
      read.csv(here::here(
        "inputs",
        auth0_userid,
        ulid_current,
        paste0("siteDetails", ulid_current, ".csv")
      ),
      stringsAsFactors = FALSE)
    
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      leafletProxy(mapId = "wa_road_map") %>%
        addMarkers(
          lat = rvData$siteDetailsDF$latitude[i],
          lng = rvData$siteDetailsDF$longitude[i],
          label = paste0(
            "SiteID:",
            rvData$siteDetailsDF$input_evse_id[i],
            " @ ",
            signif(rvData$siteDetailsDF$latitude[i], digits = 5),
            ", ",
            signif(rvData$siteDetailsDF$longitude[i], digits = 5)
          ),
          labelOptions = labelOptions(noHide = T),
          group = "overlay",
          layerId = as.character(rvData$siteDetailsDF$input_evse_id[i])
        )
    }
    
  })
  
  observeEvent((input$newSiteBtn), {
    clearAllMarkers()
  })
  
  output$wa_road_map <- renderLeaflet(setup_data$map_data$wa_map)
  
  observeEvent(input$wa_road_map_click, {
    # print("map clicked")
    if (rvData$firstClick == TRUE) {
      removeUI(selector = "#newSiteBtn")
      clearAllMarkers()
    }
    
    
    
    rvData$click <- input$wa_road_map_click
    
    # Get lat, long from map_click
    clat <- rvData$click$lat
    clng <- rvData$click$lng
    # print(clat, clng)
    # Convert the lat,long to Spatial point
    sp_start <- SpatialPoints(cbind(clng, clat),
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    buffer_road <- over(sp_start, setup_data$map_data$buf_critical_ll)
    
    if (!is.na(buffer_road$trip_count)) {
      # print("The point is within buffer")
      
      rvData$siteID <-  rvData$siteID + 1
      
      ele_id <- rvData$siteID
      
      rvData$siteDisplayHash[[as.character(rvData$siteID)]] <-
        list(
          fluidRow(
            column(
              4, h5(paste0("Site ID: ", rvData$siteID), style = "text-align: center;")
            ),
            column(
              2,
              dropdownButton(
                tags$h4(paste0("Enter the station ", rvData$siteID, " details"), style = "color: purple;"),
                hr(style = "border-color: purple;"), 
                fluidRow(
                  column(
                    12,
                    prettyRadioButtons(paste0("dcfc_plug_type", rvData$siteID), label = "Type of DCFC plug",
                                       choices = list("CHAdeMO only" = 1, "COMBO only" = 2, "Both CHAdeMO and COMBO" = 3), 
                                       selected = 3, inline = TRUE)
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    3,
                    numericInput(
                      inputId = paste0("dcfc_plug_count", rvData$siteID),
                      label = "Number of plugs",
                      value = 1,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    sliderInput(
                      inputId = paste0("dcfc_plug_power", rvData$siteID),
                      label = 'Power per plug (kW)',
                      value = 50,
                      min = 10,
                      max = 500,
                      step = 10
                    )
                  ),
                  column(
                    3,
                    wellPanel(
                      column(
                        12,
                        sliderInput(
                          inputId = paste0("fixed_charging_price_slider", rvData$siteID),
                          label = 'Fixed Charging Price ($)',
                          value = 0.5,
                          min = 0,
                          max = 10,
                          step = 0.1
                        )
                      ),
                      column(
                        12,
                        selectInput(
                          inputId = paste0("dd_var_charging_unit", rvData$siteID),
                          choices = c("min", "kWh"),
                          label = "Unit"
                        )
                      ),
                      column(
                        12,
                        sliderInput(
                          inputId = paste0("var_charging_price_slider", rvData$siteID),
                          label = 'Variable Charging Price ($)',
                          value = 0.5,
                          min = 0,
                          max = 10,
                          step = 0.1
                        )
                      )
                    )
                  ),
                  column(
                    3,
                    wellPanel(
                      column(
                        12,
                        sliderInput(
                          inputId = paste0("fixed_parking_price_slider", rvData$siteID),
                          label = 'Fixed Parking Price ($)',
                          value = 0.5,
                          min = 0,
                          max = 10,
                          step = 0.1
                        ),
                        column(
                          12,
                          selectInput(
                            inputId = paste0("dd_var_parking_unit", rvData$siteID),
                            choices = c("min"),
                            label = "Unit"
                          )
                        ),
                        column(
                          12,
                          sliderInput(
                            inputId = paste0("var_parking_price_slider", rvData$siteID),
                            label = 'Variable Parking Price ($)',
                            value = 0.5,
                            min = 0,
                            max = 10,
                            step = 0.1
                          )
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    3,
                    numericInput(
                      inputId = paste0("level2_plug_count", rvData$siteID),
                      label = "Number of Level-2 plugs",
                      value = 1,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    sliderInput(
                      inputId = paste0("level2_plug_power", rvData$siteID),
                      label = 'Power per Level-2 plug',
                      value = 10,
                      min = 1,
                      max = 19.2,
                      step = 0.1
                    )
                  ),
                  column(
                    3,
                    wellPanel(
                      column(
                        12,
                        sliderInput(
                          inputId = paste0("level2_fixed_charging_price_slider", rvData$siteID),
                          label = 'Fixed Charging Price ($)',
                          value = 0.5,
                          min = 0,
                          max = 10,
                          step = 0.1
                        )
                      ),
                      column(
                        12,
                        selectInput(
                          inputId = paste0("dd_level2_var_charging_unit", rvData$siteID),
                          choices = c("min", "kWh", "fixed"),
                          label = "Unit"
                        )
                      ),
                      column(
                        12,
                        sliderInput(
                          inputId = paste0("level2_var_charging_price_slider", rvData$siteID),
                          label = 'Variable Charging Price ($)',
                          value = 0.5,
                          min = 0,
                          max = 10,
                          step = 0.1
                        )
                      )
                    )
                  ),
                  column(
                    3,
                    wellPanel(
                      column(
                        12,
                        sliderInput(
                          inputId = paste0("level2_fixed_parking_price_slider", rvData$siteID),
                          label = 'Parking Price ($)',
                          value = 0.5,
                          min = 0,
                          max = 10,
                          step = 0.1
                        )
                      ),
                      column(
                        12,
                        selectInput(
                          inputId = paste0("dd_level2_var_parking_unit", rvData$siteID),
                          choices = c("min", "hour", "fixed"),
                          label = "Unit"
                        )
                      ),
                      column(
                        12,
                        sliderInput(
                          inputId = paste0("level2_var_parking_price_slider", rvData$siteID),
                          label = 'Variable Parking Price ($)',
                          value = 0.5,
                          min = 0,
                          max = 10,
                          step = 0.1
                        )
                      )
                    )
                  )
                ),
                circle = TRUE,
                status = "success",
                icon = icon("sliders"),
                width = "1200px",
                tooltip = tooltipOptions(title = "Click to enter the site details !", placement = "left")
              )
            ),
            column(
              1,
              actionBttn(
                inputId = paste0("removeBtn", rvData$siteID),
                label = NULL,
                style = "material-circle",
                color = "danger",
                icon = icon("remove")
              )
            )
          )
        )
      
      insertUI(
        selector = '#siteEditor',
        ui = tags$div(rvData$siteDisplayHash[[as.character(rvData$siteID)]],
                      id = rvData$siteID)
      )
      
      if (length(rvData$siteDisplayHash) == 1) {
        insertUI(
          selector = '#siteEditorBtns',
          ui = tags$div(
            hr(),
            materialSwitch(
              inputId = "tesla_toggle",
              label = "Tesla",
              value = FALSE,
              status = "primary"
            ),
            actionBttn(inputId = "submitButton", label = "Submit for analysis"),
            actionBttn(inputId = "resetButton", label = "Reset"),
            id = 'site_editor_btns'
          )
          
        )
      }
      leafletProxy(mapId = "wa_road_map") %>%
        addMarkers(
          lat = clat,
          lng = clng,
          label = paste0(
            "SiteID:",
            rvData$siteID,
            " @ ",
            signif(clat, digits = 5),
            ", ",
            signif(clng, digits = 5)
          ),
          labelOptions = labelOptions(noHide = T),
          group = "overlay",
          layerId = as.character(rvData$siteID)
        )
      
      rvData$siteDetailsDF[nrow(rvData$siteDetailsDF) + 1, 1:5] <-
        c(
          rvData$siteID,
          buffer_road$trip_count,
          as.character(buffer_road$od_pairs),
          clat,
          clng
        )
      
      observeEvent(input[[paste0("removeBtn", ele_id)]], {
        print("Button clicked")
        print(ele_id)
        
        leafletProxy(mapId = "wa_road_map") %>%
          removeMarker(layerId = ele_id)
        
        del(ele_id, rvData$siteDisplayHash)
        
        rvData$siteDetailsDF <-
          rvData$siteDetailsDF[-which(rvData$siteDetailsDF$input_evse_id == ele_id), ]
        
        removeUI(selector = paste0("#", ele_id))
        
        if (nrow(rvData$siteDetailsDF) == 0) {
          removeUI(selector = '#site_editor_btns')
        }
      })
      
    }
    
    
    
  })
  
  observeEvent(input$resetButton, {
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      removeUI(selector = paste0("#", rvData$siteDetailsDF$input_evse_id[i]))
    }
    del(keys(rvData$siteDisplayHash),
        rvData$siteDisplayHash)
    
    removeUI(selector = '#site_editor_btns')
    
    leafletProxy(mapId = "wa_road_map") %>%
      removeMarker(layerId = rvData$siteDetailsDF$input_evse_id)
    
    rvData$siteDetailsDF <- rvData$siteDetailsDF[0, ]
  })
  
  
  observeEvent(input$submitButton, {
    # ulid_submit <- ulid::ULIDgenerate()
    print("In submit")
    dt_submit <- Sys.time()
    print(ls(session$userData))
    print(session$userData$auth0_info)
    auth0_sub <- session$userData$auth0_info$sub
    print(auth0_sub)
    # Give auth0_sub default value
    if(is.null(auth0_sub)) {
      auth0_sub <- "google-oauth2|116694976789141474362"
    }
    auth0_userid <-
      strsplit(as.character(auth0_sub), "|", fixed = TRUE)[[1]][2]
    user_name <- session$userData$auth0_info$name
    user_email <- session$userData$auth0_info$email
    if(is.null(user_email)) {
      user_email <- "cp84@uw.edu"
    }
    # if (!dir.exists("inputs")) {
    #   dir.create("inputs")
    # }
    # if (!dir.exists(paste0("inputs/", auth0_userid))) {
    #   dir.create(paste0("inputs/", auth0_userid))
    # }
    # dir.create(paste0("inputs/", auth0_userid, "/", ulid_submit))
    #
    #
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      site_id <- rvData$siteDetailsDF$input_evse_id[i]
      rvData$siteDetailsDF[i, 6:22] <-
        c(
          input[[paste0("dcfc_plug_type", site_id)]],
          input[[paste0("dcfc_plug_count", site_id)]],
          input[[paste0("dcfc_plug_power", site_id)]],
          input[[paste0("fixed_charging_price_slider", site_id)]],
          input[[paste0("dd_var_charging_unit", site_id)]],
          input[[paste0("var_charging_price_slider", site_id)]],
          input[[paste0("fixed_parking_price_slider", site_id)]],
          input[[paste0("dd_var_parking_unit", site_id)]],
          input[[paste0("var_parking_price_slider", site_id)]],
          input[[paste0("level2_plug_count", site_id)]],
          input[[paste0("level2_plug_power", site_id)]],
          input[[paste0("level2_fixed_charging_price_slider", site_id)]],
          input[[paste0("dd_level2_var_charging_unit", site_id)]],
          input[[paste0("level2_var_charging_price_slider", site_id)]],
          input[[paste0("level2_fixed_parking_price_slider", site_id)]],
          input[[paste0("dd_level2_var_parking_unit", site_id)]],
          input[[paste0("level2_var_parking_price_slider", site_id)]]
        )
    }
    
    DBI::dbWriteTable(
      setup_data$main_con,
      "analysis_record",
      data.frame(
        "user_id" = auth0_userid,
        "sim_date_time" = as.character(dt_submit),
        "status" = "inserted",
        "include_tesla" = input$tesla_toggle
      ),
      append = TRUE
    )
    
    DBI::dbGetQuery(
      setup_data$main_con,
      paste0(
        "insert into user_details (user_id, user_name, email_id, last_submit_date) values ('",
        auth0_userid,
        "', '",
        user_name,
        "', '",
        user_email,
        "', '",
        as.character(dt_submit),
        "' ) on conflict (user_id) do update set last_submit_date = EXCLUDED.last_submit_date"
      )
    )
    # Get the analysis_id from the just inserted record
    a_id <-
      DBI::dbGetQuery(
        setup_data$main_con,
        paste0(
          "select analysis_id from analysis_record where  sim_date_time =  '",
          as.character(dt_submit),
          "' and user_id = '",
          auth0_userid,
          "'"
        )
      )$analysis_id
    rvData$siteDetailsDF$analysis_id <- a_id
    DBI::dbWriteTable(setup_data$main_con, "new_evses", rvData$siteDetailsDF, append = TRUE)
    
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      removeUI(selector = paste0("#", rvData$siteDetailsDF$input_evse_id[i]))
    }
    
    removeUI(selector = '#site_editor_btns')
    removeUI(selector = '#leadText')
    
    leafletProxy(mapId = "wa_road_map") %>%
      removeMarker(layerId = rvData$siteDetailsDF$input_evse_id)
    
    del(keys(rvData$siteDisplayHash),
        rvData$siteDisplayHash)
    
    
    
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
               actionBttn(inputId = "postSubmitBtn", label = "Run another analysis")
             ))
    
    # TODO: Read the submitted inputs and status from the database
    # and display as a table
    
    # rvData$submittedInputs[[ulid_submit]] <- rvData$siteDetailsDF
    #
    # if (length(rvData$submittedInputs) == 1) {
    #   insertUI(
    #     selector = "#submittedInputs",
    #     ui = radioButtons(
    #       inputId = "radioInputs",
    #       label = "Select the input ID",
    #       choiceNames = keys(rvData$submittedInputs),
    #       choiceValues = keys(rvData$submittedInputs),
    #       selected = keys(rvData$submittedInputs)[1]
    #     )
    #   )
    # } else {
    #   updateRadioButtons(
    #     session,
    #     "radioInputs",
    #     choiceNames = keys(rvData$submittedInputs),
    #     choiceValues = keys(rvData$submittedInputs)
    #   )
    #
    # }
    
    rvData$siteDetailsDF <- rvData$siteDetailsDF[0, ]
    clearAllMarkers()
  })
  
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
    clearAllMarkers()
  })
  
  session$onFlushed(function() {
    print("Called now")
    
    user_name <- session$userData$auth0_info$name
    email <- session$userData$auth0_info$email
    
    insertUI(
      selector = "#userInfo",
      ui = tags$div(
        status = "primary",
        tags$h3(user_name),
        tags$h4(email),
        logoutButton()
        
      )
    )
  })
  
  output$logo2 <- renderImage({
    return(
      list(
        src = "data/logo2.png",
        width = 200,
        height = 66,
        contentType = "image/png",
        alt = "logo"
      )
    )
  }, deleteFile = FALSE)
}

auth0::shinyAppAuth0(ui, server)
