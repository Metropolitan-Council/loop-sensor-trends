#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id, pair){
  ns <- NS(id)
  tagList(
    if(pair == "map"){
      wellPanel(
        p("The map shows the decreases in travel at individual traffic monitoring sites across the Twin Cities Metropolitan area. Traffic monitoring is performed by the Minnesota Department of Transportation (MnDOT) using detectors built into the infrastructure of the roads. These detectors are usually used to estimate congestion along Metro area highways. "),
        mod_map_inputs_ui("map_inputs_ui_1")
      )
    } else if(pair == "plot"){
      wellPanel(
        p("This plot shows the daily relative decrease in freeway travel over time across the Twin Cities metropolitan region after March 1. Points that fall below the zero-line represent decreases in travel relative to typical travel on that day of the year and day of the week. Typical travel is estimated using a statistical analysis of traffic volumes from 2018, 2019, and 2020 prior to March 1. ")
      )
    } else if(pair == "table"){
      wellPanel(
        mod_download_ui("download_ui_1")
        
        
      )
    }
  )
}

#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(input, output, session){
  ns <- session$ns
  
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")

## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")

