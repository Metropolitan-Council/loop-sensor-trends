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
    
    wellPanel(id = "sidebar_panel",
              
              p("This plot shows the daily relative decrease in travel over time across the Twin Cities metropolitan region after March 1. Points that fall below the zero-line represent decreases in travel relative to typical travel on that day of the year and day of the week. Typical travel is estimated using a statistical analysis of traffic volumes from 2018, 2019, and 2020 prior to March 1. "),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              p("The map shows the decreases in travel at individual traffic monitoring sites across the Twin Cities Metropolitan area. Traffic monitoring is performed by the Minnesota Department of Transportation (MnDOT) using detectors built into the infrastructure of the roads. These detectors are usually used to estimate congestion along Metro area highways. "),
              
      dateInput(ns("select_date"), "Select a date to see change in the map over time",
                value = max(covid.traffic.trends::predicted_actual_by_region$date),
                min = min(covid.traffic.trends::predicted_actual_by_region$date),
                max = max(covid.traffic.trends::predicted_actual_by_region$date),
                format = "mm/dd/yyyy",
                startview = "month"
      )
    )
    
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

