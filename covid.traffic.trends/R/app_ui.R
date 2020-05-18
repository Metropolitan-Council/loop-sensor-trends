#' The application User-Interface
#'
#' @param request Internal parameter for \code{shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import council.skeleton
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      council.skeleton::sk_page(
        council.skeleton::sk_header(
          "COVID-19 Outbreak – Metro Area Travel Declines",
          shiny::h4("Traffic data show more metro area residents are staying home")
        ),
        council.skeleton::sk_nav(
          council.skeleton::sk_nav_item("map_plot", "Map & Plot"),
          council.skeleton::sk_nav_item("download", "Download Data"),
          council.skeleton::sk_nav_item("about", "About")
        ),

        council.skeleton::sk_row(
          id = "map_plot",
          width = 12,
          h5("Twin Cities’ freeway travel decreasing across COVID-19 timeline"),

          council.skeleton::sk_col("sk_plot_col",
            width = 12,

            council.skeleton::sk_col("sk_sidebar_plot",
              width = 3,
              mod_sidebar_ui("sidebar_ui_plot", pair = "plot")
            ),
            council.skeleton::sk_col("sk_plot",
              width = 9,
              mod_plot_ui("plot_ui_1")
            )
          ),
          h5("Decreases in freeway travel are occuring across the Twin Cities metropolitan region"),

          council.skeleton::sk_col("sk_map_col",
            width = 12,
            council.skeleton::sk_col("sk_sidebar_map",
              width = 3,
              mod_sidebar_ui("sidebar_ui_map", pair = "map")
            ),

            council.skeleton::sk_col("sk_map",
              width = 9,
              mod_leaflet_ui("leaflet_ui")
            )
          )
        ),

        council.skeleton::sk_row(
          id = "download",
          sk_col("sk_sidebar_download",
            width = 3,
            h5("Download the most recent data"),
            mod_sidebar_ui("sidebar_ui_table", pair = "table"),
          ),
          sk_col("sk_table",
            width = 9,
            mod_table_ui("table_ui_1")
          )
        ),


        council.skeleton::sk_row(
          id = "about",
          mod_about_ui("about_ui")
        ),


        tags$div("For an accessible version of this information, please contact us at",
          tags$a(href = "mailto:public.info@metc.state.mn.us", "public.info@metc.state.mn.us"),
          style = "font-size: 1rem;
             display: block;
             text-align: right;
             padding: 1%;", align = "right"
        ),

        tags$footer(
          #----
          tags$a(
            href = "https://metrocouncil.org", target = "_blank",
            img(src = "www/main-logo.png", align = "right", style = "padding: 1%")
          )
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  suppressDependencies()
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "COVID-19 Outbreak – Metro Area Travel Declines"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
