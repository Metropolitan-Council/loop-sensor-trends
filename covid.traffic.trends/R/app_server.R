#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  callModule(mod_sidebar_server, "sidebar_ui_map")
  callModule(mod_sidebar_server, "sidebar_ui_plot")

  map_inputs <- callModule(mod_map_inputs_server, "map_inputs_ui_1")


  callModule(mod_leaflet_server, "leaflet_ui",
    map_inputs = map_inputs
  )

  callModule(mod_plot_server, "plot_ui_1")
  callModule(mod_table_server, "table_ui_1")
  callModule(mod_download_server, "download_ui_1")
}
