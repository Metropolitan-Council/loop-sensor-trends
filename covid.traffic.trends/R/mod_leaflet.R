#' leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import leaflet
#' @importFrom shiny NS tagList 
mod_leaflet_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = "600px")
    
  )
}

#' leaflet Server Function
#'
#' @noRd 
mod_leaflet_server <- function(input, output, session,
                               sidebar_values){
  ns <- session$ns
  
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(-94.51,43.9,-91.93,45.72)%>% # fitting boundary of map to metro area and rochester
      addProviderTiles("CartoDB.DarkMatter",
                       group = "Carto DarkMatter") %>%
      addProviderTiles("CartoDB.Positron",
                       group = "Carto Positron") %>%
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("points", zIndex = 420) %>%
      addPolygons(data = covid.traffic.trends::mn_counties,
                  group = "County outlines",
                  fill = NA,
                  color = "darkgray",
                  weight = 1) %>% 
      hideGroup("County outlines") %>% 
      # addCircleMarkers(
      #   data = covid.traffic.trends::nodes, 
      #   color = "gray",
      #   radius = 2,
      #   fill = FALSE,
      #   group = "Traffic Detector Locations",
      #   options = leafletOptions(pane = "points")
      # ) %>% 
      addLayersControl(position = "bottomright",
                       baseGroups = c("Carto Positron",
                                      "Carto DarkMatter"),
                       overlayGroups = c("Nodes",
                                         "County outlines"))
  })
  
  
  observeEvent(sidebar_values$date, {
    # browser()
    dat <- covid.traffic.trends::predicted_actual_by_node %>% 
      filter(date == sidebar_values$date)
    
    col_pal <- colorNumeric(palette = "PuOr",
                        domain = c(-100:100), # suggest white is zero, purple is decrease, orange is increase 
                        reverse = T)
    
    
    map <- leafletProxy("map", session = session) %>% 
      clearGroup("Nodes") %>%
      addCircleMarkers(data = dat,
                       lng = ~r_node_lon,
                       lat = ~r_node_lat,
                       color = ~col_pal(volume.diff),
                       stroke = T,
                       fillOpacity = 0.75, 
                       popup = ~paste(hover_text),
                       radius = ~5*(scl_volume),
                       group = "Nodes",
                       options = leafletOptions(pane = "points") 
      ) %>% 
      addLegend(position = "topright",
                pal = col_pal,
                values = dat$volume.diff,
                layerId = "Nodes",
                title = paste("% Change", "<br>",
                              "from Expected"),
                labFormat = labelFormat(suffix = "%"))
    
    return(map)
  })
}

## To be copied in the UI
# mod_leaflet_ui("leaflet_ui")

## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui")

