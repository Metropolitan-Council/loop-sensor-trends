#' map_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @importFrom shiny NS tagList 
mod_map_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    dateInput(ns("select_date"), "Select a date to see change in the map over time",
              value = max(covid.traffic.trends::predicted_actual_by_region$date),
              min = min(covid.traffic.trends::predicted_actual_by_region$date),
              max = max(covid.traffic.trends::predicted_actual_by_region$date),
              format = "mm/dd/yyyy",
              startview = "month"
    ),
    shinyWidgets::pickerInput(ns("select_corridor"),
                              "Select a corridor",
                              choices = unique(c(predicted_actual_by_node$Entrance$corridor_route, predicted_actual_by_node$Exit$corridor_route, predicted_actual_by_node$Intersection$corridor_route, predicted_actual_by_node$Station$corridor_route)),
                              selected = unique(c(predicted_actual_by_node$Entrance$corridor_route, predicted_actual_by_node$Exit$corridor_route, predicted_actual_by_node$Intersection$corridor_route, predicted_actual_by_node$Station$corridor_route)),
                              multiple = TRUE,
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    dropupAuto = FALSE,
                                                                    liveSearch = TRUE))
    
    
  )
}

#' map_inputs Server Function
#'
#' @noRd 
mod_map_inputs_server <- function(input, output, session){
  ns <- session$ns
  vals <- reactiveValues()
  
  observeEvent(input$select_date, {
    vals$date <- input$select_date
  })
  
  observeEvent(input$select_corridor, {
    vals$corridor <- input$select_corridor
  })
  
  
  return(vals)
}

## To be copied in the UI
# mod_map_inputs_ui("map_inputs_ui_1")

## To be copied in the server
# callModule(mod_map_inputs_server, "map_inputs_ui_1")

