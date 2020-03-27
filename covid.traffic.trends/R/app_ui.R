#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import skeleton
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      skeleton::sk_page(
        skeleton::sk_header("Minnesotans Reduce Travel in Response to COVID-19",
                            shiny::h4("Traffic Data Reveals More Minnesotans are Staying at Home During the COVID-19 Outbreak")),
        skeleton::sk_nav(
          skeleton::sk_nav_item("map_plot", "Map & Plot"),
          skeleton::sk_nav_item("about", "About")
        ),
        
        skeleton::sk_row(id = "map_plot",
                         width = 12,
                         skeleton::sk_col("sk_sidebar", width = 2,
                                          mod_sidebar_ui("sidebar_ui")
                         ),
                         skeleton::sk_col("sk_map", width = 10,
                                          h5("Decreases in travel are occuring across the Twin Cities metropolitan region"),
                                          
                                          mod_leaflet_ui("leaflet_ui"),
                                          h5("Travel decreased steadily in the days following the first COVID-19 case in Minnesota"),
                                          mod_plot_ui("plot_ui_1")
                                          
                         )
        ),
        skeleton::sk_row(id = "about",
                         mod_about_ui("about_ui")
        ),
        
        
        # tags$div("For an accessible version of this information, please contact us at research(at)metc.state.mn.us",
        #          style = "font-size: 1rem;
        #      display: block;
        #      text-align: right;
        #      padding: 1%;", align = "right"),
        
        tags$footer(
          #----
          tags$a(href="https://metrocouncil.org", target="_blank",
                 img(src = "www/logo.png", align = "right", style = "padding: 1%"))
          # tags$div(
          #   tags$a(href="https://github.com/Metropolitan-Council/loop-sensor-trends", target="_blank",
          #          icon(name = "github", lib = "font-awesome"))
          # )    
        )
        
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Minnesota COVID-19 Traffic Trends'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

