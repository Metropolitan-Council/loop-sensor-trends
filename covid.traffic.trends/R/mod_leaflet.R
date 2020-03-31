#' leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import leaflet
#' @importFrom shiny NS tagList
mod_leaflet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = "600px")
  )
}

#' leaflet Server Function
#'
#' @noRd
#' @importFrom purrr map
#' @importFrom dplyr filter

mod_leaflet_server <- function(input, output, session,
                               map_inputs) {
  ns <- session$ns

  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(-93.521749, 44.854051, -92.899649, 45.081892) %>% # fitting boundary of map to metro area and rochester
      addProviderTiles("CartoDB.DarkMatter",
        group = "Carto DarkMatter"
      ) %>%
      addProviderTiles("CartoDB.Positron",
        group = "Carto Positron"
      ) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("points", zIndex = 420) %>%
      addPolygons(
        data = covid.traffic.trends::mn_counties,
        group = "County outline",
        fill = NA,
        color = "darkgray",
        weight = 1
      ) %>%
      hideGroup("County outline") %>%
      addLayersControl(
        position = "bottomright",
        baseGroups = c(
          "Carto Positron",
          "Carto DarkMatter"
        ),
        overlayGroups = c(
          "Station",
          "Entrance",
          "Exit",
          "Intersection",
          "County outline"
        )
      )
  })


  observeEvent(map_inputs$date, {
    # browser()
    dat <- purrr::map(
      covid.traffic.trends::predicted_actual_by_node,
      filter, date == map_inputs$date
    )

    col_pal <- colorBin(
      palette = "RdBu",
      domain = c(-100:100), bins = 10, # suggest white is zero, purple is decrease, orange is increase
      reverse = T
    )


    map <- leafletProxy("map", session = session) %>%
      clearGroup("Station") %>%
      clearGroup("Entrance") %>%
      clearGroup("Exit") %>%
      clearGroup("Intersection") %>%
      addCircleMarkers(
        data = dat$Station,
        lng = ~r_node_lon,
        lat = ~r_node_lat,
        color = ~ col_pal(volume.diff),
        stroke = T,
        fillOpacity = 0.75,
        popup = ~ paste(hover_text),
        radius = 3,
        group = "Station",
        options = leafletOptions(pane = "points")
      ) %>%
      addCircleMarkers(
        data = dat$Entrance,
        lng = ~r_node_lon,
        lat = ~r_node_lat,
        color = ~ col_pal(volume.diff),
        stroke = T,
        fillOpacity = 0.75,
        popup = ~ paste(hover_text),
        radius = 3,
        group = "Entrance",
        options = leafletOptions(pane = "points")
      ) %>%
      addCircleMarkers(
        data = dat$Exit,
        lng = ~r_node_lon,
        lat = ~r_node_lat,
        color = ~ col_pal(volume.diff),
        stroke = T,
        fillOpacity = 0.75,
        popup = ~ paste(hover_text),
        radius = 3,
        group = "Exit",
        options = leafletOptions(pane = "points")
      ) %>%
      addCircleMarkers(
        data = dat$Intersection,
        lng = ~r_node_lon,
        lat = ~r_node_lat,
        color = ~ col_pal(volume.diff),
        stroke = T,
        fillOpacity = 0.75,
        popup = ~ paste(hover_text),
        radius = 3,
        group = "Intersection",
        options = leafletOptions(pane = "points")
      ) %>%
      addLegend(
        position = "topright",
        pal = col_pal,
        values = dat$volume.diff,
        layerId = "Nodes",
        title = paste(
          "% Change", "<br>",
          "from Expected"
        ),
        labFormat = labelFormat(suffix = "%")
      )

    return(map)
  })
}

## To be copied in the UI
# mod_leaflet_ui("leaflet_ui")

## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui")
