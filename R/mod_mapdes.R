#' mapdes UI Function
#'
#' @description A shiny Module that manages the map on the page.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import magrittr
#' @importFrom shiny NS tagList
mod_mapdes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::bs4Card(
      title = textOutput(ns("map_card_title")),
      #paste0("Washington State DCFC Network (last updated: ",setup_data$map_data$cs_data$last_updated_date,")"),
      closable = FALSE,
      status = "primary",
      collapsible = TRUE,
      labelTooltip = "Washington State DCFC Network",
      elevation = 4,
      width = NULL,
      solidHeader = TRUE,
      dropdownMenu = bs4Dash::dropdownItemList(
        bs4Dash::dropdownItem(name = "Click on the map - within the buffer - to place a charging station"),
        bs4Dash::dropdownItem(url = "https://evi-dss.readthedocs.io/en/latest/evi_des.html#washington-ev-dcfc-system", name = "Read about the overlay in the docs"),
        icon = "question-circle"
      ),
      maximizable = TRUE,
      shinycssloaders::withSpinner(
        leaflet::leafletOutput(ns("wa_road_map"), height = 700, width = "100%"),
        type = 8,
        color = "#0dc5c1"
      )
    )
  )
}

#' mapdes Server Function
#'
#' @noRd
mod_mapdes_server <- function(input, output, session, globals) {
  ns <- session$ns
  
  new_icon <-
    leaflet::makeAwesomeIcon(
      icon = "plus-square",
      library = "fa",
      markerColor = "purple",
      iconColor = "white"
    )
  
  combo_icon <-
    leaflet::makeAwesomeIcon(
      icon = "charging-station",
      library = "fa",
      iconColor = "#475DCC",
      markerColor = "white",
      extraClasses = ""
    )
  
  combo_icon_sel <-
    leaflet::makeAwesomeIcon(
      icon = "charging-station",
      library = "fa",
      iconColor = "#475DCC",
      markerColor = "orange",
      extraClasses = ""
    ) 
  
  combo_icon2 <-
    leaflet::makeIcon(iconUrl = "www/combo_fa.svg",
                      iconWidth = "15.75",
                      iconHeight = "14")
  
  chademo_icon <-
    leaflet::makeAwesomeIcon(
      icon = "charging-station",
      library = "fa",
      iconColor = "#F23D3D",
      markerColor = "white",
      extraClasses = ""
    )
  
  chademo_icon_sel <-
    leaflet::makeAwesomeIcon(
      icon = "charging-station",
      library = "fa",
      iconColor = "#F23D3D",
      markerColor = "orange",
      extraClasses = ""
    )
  
  
  chademo_icon2 <-
    leaflet::makeIcon(iconUrl = "www/chademo_fa.svg",
                      iconWidth = "15.75",
                      iconHeight = "14")
  
  
  rvData <- reactiveValues(
    siteID = 0,
    new_site_counter = 0,
    siteIDs = c(),
    click = NULL,
    siteDetailsDF = data.frame(
      trip_count = numeric(),
      od_pairs = character(),
      latitude = numeric(),
      longitude = numeric(),
      siteID = character(),
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
    bevse_dcfc = data.frame()
  )
  
  output$map_card_title <- renderText({
    pool <- globals$stash$pool
    
    last_updated_date <-
      pool %>% DBI::dbGetQuery("select last_updated from table_stats where table_name = 'built_evse';")
    
    paste0("Washington State DCFC Network (last updated: ",
           last_updated_date,
           ")")
  })
  
  # Map rendering ---------------------------
  output$wa_road_map <- leaflet::renderLeaflet({
    pool <- globals$stash$pool
    bevses_db <- pool %>% dplyr::tbl("built_evse")
    
    evse_dcfc <-
      bevses_db %>% dplyr::select(dcfc_count,
                                  latitude,
                                  longitude,
                                  bevse_id,
                                  connector_code) %>% dplyr::filter(dcfc_count > 0) %>% dplyr::collect()
    rvData$bevse_dcfc <- evse_dcfc
    
    all_chargers_combo <-
      evse_dcfc[evse_dcfc$connector_code == 2 |
                  evse_dcfc$connector_code == 3,]
    
    all_chargers_chademo <-
      evse_dcfc[evse_dcfc$connector_code == 1 |
                  evse_dcfc$connector_code == 3,]
    
    overlay_names <-
      c("Buffer")
    
    base_layers <- c( "Both", "CHAdeMO", "COMBO")
    

    wa_map <-
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::setMaxBounds(-124.8361, 45.5437,-116.9174, 49.0024) %>%
      leaflet::addTiles() %>%
      leaflet.mapboxgl::addMapboxGL(
        style = "mapbox://styles/chintanp/ckhmly2n1009u1bpg7ojuzn3h", # COMBO only
        accessToken = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
        group = base_layers[3],
        setView = FALSE
      )  %>%
    leaflet.mapboxgl::addMapboxGL(
      style = "mapbox://styles/chintanp/ckhmnh3oa01r419pkc9qgn0sq", # both combo and chademo
      accessToken = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
      group = base_layers[1],
      setView = FALSE
    ) %>%
      leaflet.mapboxgl::addMapboxGL(
        style = "mapbox://styles/chintanp/ckhmnjizl01s819qk4vqw449m", # chademo only
        accessToken = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
        group = base_layers[2],
        setView = FALSE
      ) %>%
      
      leaflet::addAwesomeMarkers(
        lng = all_chargers_combo$longitude ,
        lat = all_chargers_combo$latitude,
        popup = paste0("ID : ", all_chargers_combo$bevse_id),
        label = paste0("ID : ", all_chargers_combo$bevse_id),
        icon = combo_icon,
        group = base_layers[3], 
        layerId = all_chargers_combo$bevse_id, 
        popupOptions = leaflet::popupOptions(autoClose = FALSE, closeOnClick = FALSE, closeButton = FALSE)
        # clusterOptions = leaflet::markerClusterOptions()
      )  %>%
      leaflet::addAwesomeMarkers(
        lng = all_chargers_chademo$longitude ,
        lat = all_chargers_chademo$latitude,
        popup = paste0("ID : ", all_chargers_chademo$bevse_id),
        label = paste0("ID : ", all_chargers_chademo$bevse_id),
        icon = chademo_icon,
        group = base_layers[2], 
        layerId = all_chargers_chademo$bevse_id, 
        popupOptions = leaflet::popupOptions(autoClose = FALSE, closeOnClick = FALSE, closeButton = FALSE)
        # clusterOptions = leaflet::markerClusterOptions()
      )  %>%
      leaflet::addAwesomeMarkers(
        lng = all_chargers_combo$longitude ,
        lat = all_chargers_combo$latitude,
        popup = paste0("ID : ", all_chargers_combo$bevse_id),
        label = paste0("ID : ", all_chargers_combo$bevse_id),
        icon = combo_icon,
        group = base_layers[1], 
        layerId = all_chargers_combo$bevse_id, 
        popupOptions = leaflet::popupOptions(autoClose = FALSE, closeOnClick = FALSE, closeButton = FALSE)
        # clusterOptions = leaflet::markerClusterOptions()
      ) %>%
      leaflet::addAwesomeMarkers(
        lng = all_chargers_chademo$longitude ,
        lat = all_chargers_chademo$latitude,
        popup = paste0("ID : ", all_chargers_chademo$bevse_id),
        label = paste0("ID : ", all_chargers_chademo$bevse_id),
        icon = chademo_icon,
        group = base_layers[1],
        layerId = all_chargers_chademo$bevse_id, 
        popupOptions = leaflet::popupOptions(autoClose = FALSE, closeOnClick = FALSE, closeButton = FALSE)
        # clusterOptions = leaflet::markerClusterOptions()
      ) %>%
      leaflet::addPolygons(
        data = buf_merged,
        color = "#808080",
        group = overlay_names[1],
        opacity = 0.5
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = overlay_names,
        baseGroups = base_layers,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet.extras::addSearchOSM() %>%
      leafem::addMouseCoordinates() %>% 
      leaflet::addScaleBar(position = "bottomright")
  })
  
  # Map click event handling ----------------
  observeEvent(input$wa_road_map_click, {
    rvData$click <- input$wa_road_map_click
    
    # Get lat, long from map_click
    clat <- rvData$click$lat
    clng <- rvData$click$lng
    
    # Convert the lat,long to Spatial point
    sp_start <- sp::SpatialPoints(
      cbind(clng, clat),
      proj4string = sp::CRS(
        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84
+towgs84=0,0,0"
      )
    )
    
    buffer_road <- sp::over(sp_start, buf_critical_ll)
    
    # Only add a marker to the map if it is within the buffer region
    if (!is.na(buffer_road$trip_count)) {
      rvData$new_site_counter <-  rvData$new_site_counter + 1
      rvData$siteID <- paste0('n', as.character(rvData$new_site_counter))
      rvData$siteIDs <- c(rvData$siteIDs, rvData$siteID)
      
      leaflet::leafletProxy(mapId = "wa_road_map") %>%
        leaflet::addAwesomeMarkers(
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
          labelOptions = leaflet::labelOptions(noHide = T),
          group = "overlay",
          
          icon = new_icon,
          layerId = as.character(rvData$siteID)
        )
      
      rvData$siteDetailsDF[nrow(rvData$siteDetailsDF) + 1, 1:5] <-
        c(buffer_road$trip_count,
          as.character(buffer_road$od_pairs),
          clat,
          clng, rvData$siteID)
      # Below jugglery to set the row names as siteID may not be needed
      #rownames(rvData$siteDetailsDF)[rownames(rvData$siteDetailsDF) == as.character(nrow(rvData$siteDetailsDF))] <- rvData$siteID
    }
  })
  
  observeEvent(input$wa_road_map_marker_click, {
    
    print(input$wa_road_map_marker_click$id)
    print("marker clicked")
    
    if (input$wa_road_map_marker_click$id %in% rvData$siteIDs) {

    } else {
      rvData$siteID <-  input$wa_road_map_marker_click$id
      rvData$siteIDs <- c(rvData$siteIDs, rvData$siteID)
      
      rvData$siteDetailsDF[nrow(rvData$siteDetailsDF) + 1, 1:5] <-
        c(0,
          '',
          input$wa_road_map_marker_click$lat,
          input$wa_road_map_marker_click$lng, rvData$siteID)
    }
    
    
    
    
    
  })
  # Return values -------------
  return (list (
    "rvData" = rvData,
    "mapProxy" = leaflet::leafletProxy(mapId = "wa_road_map")
  ))
}

## To be copied in the UI
# mod_mapdes_ui("mapdes_ui_1")

## To be copied in the server
# callModule(mod_mapdes_server, "mapdes_ui_1")
