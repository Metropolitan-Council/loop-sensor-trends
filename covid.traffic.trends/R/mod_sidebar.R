#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    wellPanel(
      dateInput(ns("select_date"), "Date:",
                        value = "2020-03-25",
                        min = min(predicted_actual_by_region$date),
                        max = max(predicted_actual_by_region$date),
                        format = "yyyy-mm-dd", startview = "month"
    ),
    
    shinipsum::random_text(nwords = 30))
    
  )
}

#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(input, output, session){
  ns <- session$ns
  vals <- reactiveValues()
  
  observeEvent(input$select_date, {
    vals$date <- input$select_date
  })
  
  return(vals)
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")

## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")

