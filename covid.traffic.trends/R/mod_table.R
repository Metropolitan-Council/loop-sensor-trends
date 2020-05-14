#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    DT::dataTableOutput(ns("table"), height = "350px")
  )
}

#' table Server Function
#'
#' @noRd 
mod_table_server <- function(input, output, session){
  ns <- session$ns
  
  output$table <- DT::renderDataTable({
    DT::datatable(data = table_data,
                  colnames = c("Date",
                               "Weekday",
                               "District",
                               "Observed total vehicle miles traveled",
                               "Predicted total vehicle miles traveled",
                               "Percent difference between observed and predicted"),
                  rownames = FALSE,
                  options = list(dom = "tpl")) %>% 
      DT::formatRound(c("vmt.sum",
                        "vmt.predict"),
                      digits = 0, interval = 3,
                      mark = ",")
  })
  
}

## To be copied in the UI
# mod_table_ui("table_ui_1")

## To be copied in the server
# callModule(mod_table_server, "table_ui_1")

