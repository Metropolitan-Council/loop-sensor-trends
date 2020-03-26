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
    leafletOutput(ns("map"))
 
  )
}
    
#' leaflet Server Function
#'
#' @noRd 
mod_leaflet_server <- function(input, output, session){
  ns <- session$ns
 
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("points", zIndex = 420) %>%
      addCircleMarkers(
        data = covid.traffic.trends::nodes, 
        color = "gray",
        radius = 2,
        fill = FALSE,
        group = "Traffic Detector Locations",
        options = leafletOptions(pane = "points")
      )
  })
}
    
## To be copied in the UI
# mod_leaflet_ui("leaflet_ui")
    
## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui")
 
