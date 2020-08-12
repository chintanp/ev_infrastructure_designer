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
      dropdownIcon = "question-circle",
      dropdownMenu = bs4Dash::dropdownItemList(
        bs4Dash::dropdownItem(url = "https://evi-dss.readthedocs.io/en/latest/evi_des.html#washington-ev-dcfc-system", name = "Read about the overlay in the docs")
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
  
  output$map_card_title <- renderText({
    pool <- globals$stash$pool
    
    last_updated_date <-
      pool %>% DBI::dbGetQuery("select last_updated from table_stats where table_name = 'built_evse';")
    
    paste0("Washington State DCFC Network (last updated: ",
           last_updated_date,
           ")")
  })
  
  output$wa_road_map <- leaflet::renderLeaflet({
    pool <- globals$stash$pool
    bevses_db <- pool %>% dplyr::tbl("built_evse")
    
    evse_dcfc <-
      bevses_db %>% dplyr::select(dcfc_count,
                                  latitude,
                                  longitude,
                                  bevse_id,
                                  connector_code) %>% dplyr::filter(dcfc_count > 0) %>% dplyr::collect()
    
    all_chargers_combo <-
      evse_dcfc[evse_dcfc$connector_code == 2 |
                  evse_dcfc$connector_code == 3,]
    
    all_chargers_chademo <-
      evse_dcfc[evse_dcfc$connector_code == 1 |
                  evse_dcfc$connector_code == 3,]
    
    overlay_names <-
      c("Buffer", "Combo", "CHAdeMO")
    
    base_layers <- c("Combo", "CHAdeMO")
    base_tile_layers <- c("MapBox Light", "OSM (default)")
    
    combo_icon <-
      leaflet::makeAwesomeIcon(icon = "charging-station",
                               library = "fa",
                               iconColor = "#475DCC")
    
    chademo_icon <-
      leaflet::makeAwesomeIcon(icon = "charging-station",
                               library = "fa",
                               iconColor = "#F23D3D")
    
    wa_map <-
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::setMaxBounds(-124.8361, 45.5437,-116.9174, 49.0024) %>%
      leaflet.mapboxgl::addMapboxGL(
        style = "mapbox://styles/mapbox/streets-v11",
        accessToken = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
        setView = FALSE
      )  %>%
      # Base groups
      leaflet::addPolylines(
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
      leaflet::addPolylines(
        data = shape_trip_feasibility_chademo,
        weight = shape_trip_feasibility_chademo$trip_count / 20000,
        color = "#F23D3D",
        group = overlay_names[3],
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
      leaflet::addAwesomeMarkers(
        lng = all_chargers_combo$longitude ,
        lat = all_chargers_combo$latitude,
        popup = paste0("ID : ", all_chargers_combo$bevse_id),
        label = paste0("ID : ", all_chargers_combo$bevse_id),
        icon = combo_icon,
        group = overlay_names[2]
        # clusterOptions = leaflet::markerClusterOptions()
      )  %>%
      leaflet::addAwesomeMarkers(
        lng = all_chargers_chademo$longitude ,
        lat = all_chargers_chademo$latitude,
        popup = paste0("ID : ", all_chargers_chademo$bevse_id),
        label = paste0("ID : ", all_chargers_chademo$bevse_id),
        icon = chademo_icon,
        group = overlay_names[3]
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
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet.extras::addSearchOSM()
  })
  
  observeEvent(input$wa_road_map_click, {
    
    click <- input$wa_road_map_click
    
    # Get lat, long from map_click
    clat <- click$lat
    clng <- click$lng
    
    # Convert the lat,long to Spatial point
    sp_start <- sp::SpatialPoints(cbind(clng, clat),
                              sp::proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    buffer_road <- sp::over(sp_start, buf_critical_ll)
    
    if (!is.na(buffer_road$trip_count)) {
      
      
    }
    
  })
}

## To be copied in the UI
# mod_mapdes_ui("mapdes_ui_1")

## To be copied in the server
# callModule(mod_mapdes_server, "mapdes_ui_1")
