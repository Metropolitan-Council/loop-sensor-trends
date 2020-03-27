#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  sidebar_values <- callModule(mod_sidebar_server, "sidebar_ui")
  
  callModule(mod_leaflet_server, "leaflet_ui",
             sidebar_values = sidebar_values)
  
  callModule(mod_plot_server, "plot_ui_1")
  
}
