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
          "Freeway Segment",
          "Entrance",
          "Exit",
          "County outline"
        ),
        options = layersControlOptions(collapsed = T)
      )
  })

  current_nodes <- reactive({
    req(map_inputs$date)
    dat <- purrr::map(
      covid.traffic.trends::predicted_actual_by_node,
      filter, date == map_inputs$date
    ) %>%
      purrr::map(filter, corridor_route %in% c(map_inputs$corridor))

    return(dat)
  })

  update_map <- reactive({
    dat <- current_nodes()

    col_pal <- colorBin( # palette generated with Chroma.js (#ee3124 to #0054a4)
      palette = c(
        "#840000", "#bd1712",
        "#ed4434", "#fe8466",
        "#fac0b1", "#accdfe",
        "#78a3e8", "#497acd",
        "#2353a2", "#002f77"
      ),
      domain = c(-100:100), bins = 10, # suggest white is zero, purple is decrease, orange is increase
      reverse = T
    )


    map <- leafletProxy("map", session = session) %>%
      clearGroup("Freeway Segment") %>%
      clearGroup("Entrance") %>%
      clearGroup("Exit") %>%
      addCircleMarkers(
        data = dat$Freeway_Segment,
        lng = ~r_node_lon,
        lat = ~r_node_lat,
        color = ~ col_pal(volume_difference),
        stroke = T,
        fillOpacity = 0.75,
        popup = ~ paste(hover_text),
        radius = 3,
        group = "Freeway Segment",
        options = leafletOptions(pane = "points")
      ) %>%
      addCircleMarkers(
        data = dat$Entrance,
        lng = ~r_node_lon,
        lat = ~r_node_lat,
        color = ~ col_pal(volume_difference),
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
        color = ~ col_pal(volume_difference),
        stroke = T,
        fillOpacity = 0.75,
        popup = ~ paste(hover_text),
        radius = 3,
        group = "Exit",
        options = leafletOptions(pane = "points")
      ) %>%
      addLegend(
        position = "topright",
        pal = col_pal,
        values = dat$volume_difference,
        layerId = "Nodes",
        title = paste(
          "% Change", "<br>",
          "from Expected"
        ),
        labFormat = labelFormat(suffix = "%")
      )

    return(map)
  })

  observeEvent(map_inputs$corridor, {
    # print(map_inputs$corridor)
    # browser()
    update_map()
  })



  observeEvent(map_inputs$date, {
    update_map()
  })
}

## To be copied in the UI
# mod_leaflet_ui("leaflet_ui")

## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui")
