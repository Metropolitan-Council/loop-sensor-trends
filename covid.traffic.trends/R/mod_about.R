#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import council.skeleton
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    sk_col(
      ns("About"),
      width = 12,
      includeMarkdown(
        system.file("app/www/ABOUT.md", package = "covid.traffic.trends")
      )
    ),
    
 
  )
}
    
#' about Server Function
#'
#' @noRd 
mod_about_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_about_ui("about_ui")
    
## To be copied in the server
# callModule(mod_about_server, "about_ui_1")
 
