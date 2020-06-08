#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download"), "Download")
  )
}

#' download Server Function
#'
#' @noRd
#' @importFrom utils write.csv
mod_download_server <- function(input, output, session) {
  ns <- session$ns
  output$download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "region_state_traffic_trends.csv")
    },
    content = function(file) {
      write.csv(
        x = covid.traffic.trends::table_data,
        file,
        row.names = FALSE
      )
    }
  )
}

## To be copied in the UI
# mod_download_ui("download_ui_1")

## To be copied in the server
# callModule(mod_download_server, "download_ui_1")
