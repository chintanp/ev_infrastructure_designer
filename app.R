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
library(leafgl)
library(sf)

if (Sys.info()['sysname'] != "Windows") {
  options(shiny.host = '125.95.204.84')
  options(shiny.port = 8100)
}


#####
# evse_dcfc <- read.csv("data/evse_locations/WA_EVSE_DCFCSC.csv")
#####

# Read the EVSE information through the AFDC API
afdc_url  <-
  paste0(
    "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?fuel_type=ELEC&state=WA&ev_charging_level=dc_fast&status=E&access=public&api_key=",
    Sys.getenv('AFDC_API_KEY')
  )
evse_dcfc <- read_csv(afdc_url)
# TODO: Add a fallback clause in the case the API is non-responsive

evse_dcfc$EV_Connector_Code <- 0
evse_dcfc$ChargingCost <- 0

# Convert the connector type to code for easy parsing in GAMA
# CHADEMO only - 1
# J1772COMBO only - 2
# CHADEMO and J1772COMBO - 3
# TESLA - 4
# Ignore J1772 as it is level-2
for (i in 1:nrow(evse_dcfc)) {
  conns <- evse_dcfc$`EV Connector Types`[i]
  if (grepl("CHADEMO", conns)) {
    if (grepl("J1772COMBO", conns)) {
      evse_dcfc$EV_Connector_Code[i] <- 3
    } else {
      evse_dcfc$EV_Connector_Code[i] <- 1
    }
  } else if (grepl("J1772COMBO", conns)) {
    evse_dcfc$EV_Connector_Code[i] <- 2
  } else if (grepl("TESLA", conns)) {
    evse_dcfc$EV_Connector_Code[i] <- 4
  }
}

all_chargers_combo <-
  evse_dcfc[evse_dcfc$EV_Connector_Code == 2 |
              evse_dcfc$EV_Connector_Code == 3,]

all_chargers_chademo <-
  evse_dcfc[evse_dcfc$EV_Connector_Code == 1 |
              evse_dcfc$EV_Connector_Code == 3,]

overlay_names <-
  c("Buffer")

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
shape_trip_feasibility_chademo <- readRDS("data/shape_trip_feasibility_chademo.Rds")
shape_trip_feasibility_combo <- readRDS("data/shape_trip_feasibility_combo.Rds")
buf_critical_ll <- readRDS("data/buf_critical_ll.Rds")

wa_map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setMaxBounds(-124.8361, 45.5437,-116.9174, 49.0024) %>%
  addProviderTiles(
    "MapBox",
    options = providerTileOptions(
      id = "mapbox.light",
      noWrap = FALSE,
      accessToken = Sys.getenv("MAPBOX_ACCESS_TOKEN")
    ),
    group = base_tile_layers[1]
  )  %>%
  # Base groups
  addPolylines(
    data = shape_trip_feasibility_combo,
    weight = shape_trip_feasibility_combo$trip_count / 20000,
    color = "#750db5",
    group = base_layers[1],
    opacity = 1,
    label = paste0(
      "trip_count : ",
      shape_trip_feasibility_combo$trip_count
    ),
    popup = paste0(
      "trip_count : ",
      shape_trip_feasibility_combo$trip_count
    )
  ) %>%
  addPolylines(
    data = shape_trip_feasibility_chademo,
    weight = shape_trip_feasibility_chademo$trip_count / 20000,
    color = "#0d4db5",
    group = base_layers[2],
    opacity = 1,
    label = paste0(
      "trip_count : ",
      shape_trip_feasibility_chademo$trip_count
    ),
    popup = paste0(
      "trip_count : ",
      shape_trip_feasibility_chademo$trip_count
    )
  ) %>%
  addMarkers(
    lng = all_chargers_combo$Longitude ,
    lat = all_chargers_combo$Latitude,
    popup = paste0("ID : ", all_chargers_combo$ID),
    label = paste0("ID : ", all_chargers_combo$ID),
    icon = combo_icons,
    group = base_layers[1]
  )  %>%
  addMarkers(
    lng = all_chargers_chademo$Longitude ,
    lat = all_chargers_chademo$Latitude,
    popup = paste0("ID : ", all_chargers_chademo$ID),
    label = paste0("ID : ", all_chargers_chademo$ID),
    icon = combo_icons,
    group = base_layers[2]
  ) %>%
  addPolygons(data = buf_critical_ll ,
              color = "#a6bddb",
              group = overlay_names[1], 
              opacity = 0.3) %>%
  addLayersControl(
    baseGroups = base_layers,
    overlayGroups = overlay_names,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addResetMapButton() %>%
  addSearchOSM()

###################
### UI ###########
####################

ui <- bs4DashPage(
  navbar = bs4DashNavbar(
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = "bars",
    controlbarIcon = "th",
    fixed = FALSE,
    leftUi = bs4DropdownMenu(
      show = FALSE,
      align = "left",
      status = "warning",
      menuIcon = "envelope-open",
      src = NULL
    ),
    rightUi = bs4DropdownMenu(
      show = FALSE,
      status = "danger",
      src = "https://www.google.fr",
      bs4DropdownMenuItem(text = "message 1",
                          date = "today"),
      bs4DropdownMenuItem(text = "message 2",
                          date = "yesterday")
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "WSDOT EVI-ABM Dashboard",
    brandColor = "primary",
    url = "",
    src = "",
    elevation = 3,
    opacity = 0.3,
    bs4SidebarUserPanel(img = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
                        text = "Welcome Onboard!"),
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
  title = "WSDOT EVSE Dashboard",
  body = bs4DashBody(bs4TabItems(
    bs4TabItem(tabName = "sortabled",
               fluidRow(
                 column(
                   width = 9,
                   bs4Card(
                     title = "WSDOT Road Network",
                     closable = TRUE,
                     status = "primary",
                     collapsible = TRUE,
                     labelText = 1,
                     labelStatus = "primary",
                     labelTooltip = "WSDOT Road Network",
                     dropdownIcon = "wrench",
                     dropdownMenu = dropdownItemList(
                       dropdownItem(url = "https://www.google.com", name = "Link to google"),
                       dropdownItem(url = "#", name = "item 2"),
                       dropdownDivider(),
                       dropdownItem(url = "#", name = "item 3")
                     ),
                     elevation = 4,
                     width = NULL,
                     solidHeader = TRUE,
                     withSpinner(
                       leafglOutput("wa_road_map", height = 700),
                       type = 8,
                       color = "#0dc5c1"
                     )
                   ),
                   tags$div(
                     width = 9,
                     elevation = 4,
                     status = "primary",
                     class = "table",
                     tags$h3("Submission Details"),
                     tags$div(
                       DT::dataTableOutput(outputId = "siteDetails"),
                       width = 9
                     )
                   )
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
                     status = "danger",
                     tags$div(id = 'siteEditor'),
                     tags$div(
                       id = 'siteEditorBtns',
                       actionBttn(inputId = "newSiteBtn", label = "Click to enter new sites")
                     ),
                     tags$div(id = "postSubmit")
                   ),
                   bs4Card(
                     width = NULL,
                     solidHeader = TRUE,
                     status = "success",
                     title = "Submitted Inputs",
                     uiOutput(outputId = "submittedInputs")
                   ),
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
      ID = integer(),
      trip_count = numeric(),
      od_pairs = character(),
      latitude = numeric(),
      longitude = numeric(),
      chademo_plug_count = integer(),
      chademo_power = numeric(),
      combo_plug_count = integer(),
      combo_power = numeric(),
      stringsAsFactors = FALSE
    ),
    submittedInputs = hash(),
    firstClick = TRUE
    
  )
  
  clearAllMarkers <- function() {
    print("New sites added now")
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      leafletProxy(mapId = "wa_road_map") %>%
        removeMarker(layerId = rvData$siteDetailsDF$ID[i])
    }
    removeUI(selector = "#newSiteBtn")
    rvData$siteDetailsDF <- rvData$siteDetailsDF[0,]
    rvData$firstClick <- FALSE
  }
  
  output$submittedInputs <- renderUI({
    auth0_sub <- session$userData$auth0_info$sub
    auth0_userid <- strsplit(auth0_sub, "|", fixed = TRUE)[[1]][2]
    if (dir.exists(here::here("inputs", auth0_userid))) {
      input_dirs <-
        list.dirs(here::here("inputs", auth0_userid), recursive = FALSE)
      print(input_dirs)
      if (length(input_dirs >= 1)) {
        for (i in 1:length(input_dirs)) {
          input_file_i <-
            list.files(input_dirs[[i]],
                       full.names = TRUE,
                       pattern = "*.csv")
          print(input_file_i)
          if (!stri_isempty(input_file_i[1])) {
            rvData$submittedInputs[[basename(input_dirs[[i]])]] <-
              read.csv(input_file_i[1], stringsAsFactors = FALSE)
          }
          
          
        }
        
        radioButtons(
          label = "Select an input ID",
          
          inputId = "radioInputs",
          choiceNames = keys(rvData$submittedInputs),
          choiceValues = keys(rvData$submittedInputs),
          selected = keys(rvData$submittedInputs)[1]
        )
        
        
      }
      
    }
    
  })
  
  observeEvent(input$radioInputs, {
    leafletProxy(mapId = "wa_road_map") %>%
      removeMarker(layerId = rvData$siteDetailsDF$ID)
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
            rvData$siteDetailsDF$ID[i],
            " @ ",
            signif(rvData$siteDetailsDF$latitude[i], digits = 5),
            ", ",
            signif(rvData$siteDetailsDF$longitude[i], digits = 5)
          ),
          labelOptions = labelOptions(noHide = T),
          group = "overlay",
          layerId = as.character(rvData$siteDetailsDF$ID[i])
        )
    }
    
  })
  
  observeEvent((input$newSiteBtn), {
    clearAllMarkers()
  })
  
  output$wa_road_map <- renderLeaflet(wa_map)
  
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
    
    buffer_road <- over(sp_start, buf_critical_ll)
    
    if (!is.na(buffer_road$trip_count)) {
      # print("The point is within buffer")
      
      rvData$siteID <-  rvData$siteID + 1
      
      ele_id <- rvData$siteID
      
      rvData$siteDisplayHash[[as.character(rvData$siteID)]] <-
        list(fluidRow(
          column(4, h5(
            paste0("Site ID: ",
                   rvData$siteID), style = "text-align: center;"
          )),
          column(
            2,
            dropdownButton(
              tags$h5("Enter the number of plugs and power per plug"),
              numericInput(
                inputId = paste0("chademo_plug_count", rvData$siteID),
                label = "Number of Chademo plugs",
                value = 1
              ),
              sliderInput(
                inputId = paste0("chademo_plug_power", rvData$siteID),
                label = 'Power per Chademo plug',
                value = 50,
                min = 10,
                max = 500,
                step = 10
              ),
              numericInput(
                inputId = paste0("combo_plug_count", rvData$siteID),
                label = "Number of COMBO plugs",
                value = 1
              ),
              sliderInput(
                inputId = paste0("combo_plug_power", rvData$siteID),
                label = 'Power per COMBO plug',
                value = 50,
                min = 10,
                max = 500,
                step = 10
              ),
              circle = TRUE,
              status = "success",
              icon = icon("sliders"),
              width = "250px",
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
        ))
      
      insertUI(
        selector = '#siteEditor',
        ui = tags$div(rvData$siteDisplayHash[[as.character(rvData$siteID)]],
                      id = rvData$siteID)
      )
      
      if (length(rvData$siteDisplayHash) == 1) {
        insertUI(
          selector = '#siteEditorBtns',
          ui = tags$div(
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
          rvData$siteDetailsDF[-which(rvData$siteDetailsDF$ID == ele_id),]
        
        removeUI(selector = paste0("#", ele_id))
        
        if (nrow(rvData$siteDetailsDF) == 0) {
          removeUI(selector = '#site_editor_btns')
        }
      })
      
    }
    
    
    
  })
  
  observeEvent(input$resetButton, {
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      removeUI(selector = paste0("#", rvData$siteDetailsDF$ID[i]))
    }
    del(keys(rvData$siteDisplayHash), rvData$siteDisplayHash)
    
    removeUI(selector = '#site_editor_btns')
    
    leafletProxy(mapId = "wa_road_map") %>%
      removeMarker(layerId = rvData$siteDetailsDF$ID)
    
    rvData$siteDetailsDF <- rvData$siteDetailsDF[0,]
  })
  
  
  observeEvent(input$submitButton, {
    ulid_submit <- ulid::ULIDgenerate()
    auth0_sub <- session$userData$auth0_info$sub
    auth0_userid <- strsplit(auth0_sub, "|", fixed = TRUE)[[1]][2]
    if (!dir.exists("inputs")) {
      dir.create("inputs")
    }
    if (!dir.exists(paste0("inputs/", auth0_userid))) {
      dir.create(paste0("inputs/", auth0_userid))
    }
    dir.create(paste0("inputs/", auth0_userid, "/", ulid_submit))
    
    
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      site_id <- rvData$siteDetailsDF$ID[i]
      rvData$siteDetailsDF[i, 6:9] <-
        c(input[[paste0("chademo_plug_count", site_id)]],
          input[[paste0("chademo_plug_power", site_id)]],
          input[[paste0("combo_plug_count", site_id)]],
          input[[paste0("combo_plug_power", site_id)]])
    }
    write.csv(
      rvData$siteDetailsDF,
      file = here::here(
        "inputs",
        auth0_userid,
        ulid_submit,
        paste0("siteDetails", ulid_submit, ".csv")
      ),
      row.names = FALSE
    )
    for (i in 1:nrow(rvData$siteDetailsDF)) {
      removeUI(selector = paste0("#", rvData$siteDetailsDF$ID[i]))
    }
    
    removeUI(selector = '#site_editor_btns')
    removeUI(selector = '#leadText')
    leafletProxy(mapId = "wa_road_map") %>%
      removeMarker(layerId = rvData$siteDetailsDF$ID)
    
    
    del(keys(rvData$siteDisplayHash), rvData$siteDisplayHash)
    
    user_email <- session$userData$auth0_info$email
    
    insertUI(selector = "#postSubmit",
             ui = tags$div(
               id = "postSubmitText",
               p(
                 paste0(
                   "The input has been submitted for analysis. The analysis ID is: ",
                   ulid_submit,
                   ". An email will be sent to your registered email id - ",
                   user_email,
                   " when
                    analysis results are ready."
                 )
               ),
               actionBttn(inputId = "postSubmitBtn", label = "Run another analysis")
             ))
    rvData$submittedInputs[[ulid_submit]] <- rvData$siteDetailsDF
    
    if (length(rvData$submittedInputs) == 1) {
      insertUI(
        selector = "#submittedInputs",
        ui = radioButtons(
          inputId = "radioInputs",
          label = "Select the input ID",
          choiceNames = keys(rvData$submittedInputs),
          choiceValues = keys(rvData$submittedInputs),
          selected = keys(rvData$submittedInputs)[1]
        )
      )
    } else {
      updateRadioButtons(
        session,
        "radioInputs",
        choiceNames = keys(rvData$submittedInputs),
        choiceValues = keys(rvData$submittedInputs)
      )
      
    }
    
    rvData$siteDetailsDF <- rvData$siteDetailsDF[0,]
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
}

auth0::shinyAppAuth0(ui, server)
