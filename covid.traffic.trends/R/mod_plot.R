#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import plotly
#' @importFrom shiny NS tagList 
mod_plot_ui <- function(id){
  ns <- NS(id)
  tagList(

    plotlyOutput(ns("plot") )
  )
}
    
#' plot Server Function
#'
#' @noRd 
mod_plot_server <- function(input, output, session){
  ns <- session$ns
  
  output$plot <- renderPlotly({
    shinipsum::random_ggplotly()
  })
}
    
## To be copied in the UI
# mod_plot_ui("plot_ui_1")
    
## To be copied in the server
# callModule(mod_plot_server, "plot_ui_1")
 
