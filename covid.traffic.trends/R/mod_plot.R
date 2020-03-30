#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import plotly
#' @importFrom shiny NS tagList
mod_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"), height = "500px")
  )
}

#' plot Server Function
#'
#' @noRd
mod_plot_server <- function(input, output, session) {
  ns <- session$ns
  # Color palette values  
  
  ## Council Colors
  councilBlue   <- "#0054A4" # Council blue
  cdGreen       <- "#78A22F" # Community Development green
  mtsRed        <- "#EE3124" # MTS red
  esBlue        <- "#009AC7" # Environmental Services blue
  suppYellow    <- "#FFD200"
  suppBlack     <- "#000000"
  suppGray      <- "#666666"
  suppWhite     <- "#FFFFFF"
  ## Play Features colors
  playGreen    <- "#7BA529"
  playBlue     <- "#005AAD"
  playDaBlue   <- "#00295A"
  playYellow   <- "#C6DE29"
  playLiBlue   <- "#009CCE"
  playSalmon   <- "#E78452"
  ## Metro Stats colors
  metroBlue    <- "#0875C3"
  metroBrown   <- "#A16C4C"
  metroRed     <- "#A14D5D"
  metroDaPurp  <- "#643967"
  metroMePurp  <- "#AC74A6"
  metroLiPurp  <- "#D8B5D6"
  metroPink    <- "#F6BD9C"
  metroTan     <- "#EAE6C8"
  
  ## Regional Parks Travel Patterns
  parkGreen    <- "#008537" # the starting color for all parks
  menuBlue     <- "#012b5d" # play features dark blue, background for sidebar menu
  menuHoBlue   <- "#033a7d" # menu item hover color (~2 shades lighter than menuBlue)
  startRed     <- "#f0c0a8" # starting color for TAZ index values
  endRed       <- "#6d3112" # ending color for TAZ index values
  parkYellow   <- "#ffd15d" # yellow for the selected park
  
  whiteSmoke   <- "#F5F5F5"
  darkGreen    <- "#547121"
  output$plot <- renderPlotly({
    plot_ly() %>%
      plotly::add_markers(
        data = covid.traffic.trends::predicted_actual_by_region["doy" > 60 & "year" == 2020],
        x = covid.traffic.trends::predicted_actual_by_region$date,
        y = covid.traffic.trends::predicted_actual_by_region$typical_vmt_diff,
        name = "MnDOT Metro\n(1000+ Stations)\n",
        mode = "lines+markers",
        line = list(
          width = 3,
          color = councilBlue
        ),
        marker = list(color = councilBlue,
                      size = 8),
        hoverinfo = "text",
        text = paste(predicted_actual_by_region$hover_text),
        showlegend = TRUE
      ) %>%
      plotly::add_markers(
        data = covid.traffic.trends::predicted_actual_by_state,
        x = covid.traffic.trends::predicted_actual_by_state$date,
        y = covid.traffic.trends::predicted_actual_by_state$typical_vmt_diff,
        name = "MnDOT Statewide\n(105 Stations)\n",
        mode = "lines+markers",
        hoverinfo = "text",
        text = paste(covid.traffic.trends::predicted_actual_by_state$hover_text),
        line = list(
          width = 3,
          color = "black"
        ),
        marker = list(
          color = "black",
          size = 8
        )
      ) %>% 
      
      plotly::add_markers( ## mn actions -----
                           data = covid.traffic.trends::mn_actions,
                           x = covid.traffic.trends::mn_actions$date,
                           y = covid.traffic.trends::mn_actions$typical_vmt_diff,
                           name = "MN Actions",
                           mode = "markers",
                           hoverinfo = "none",
                           marker = list(
                             size = 12,
                             color = whiteSmoke,
                             line = list(
                               color = "black",
                               width = 1
                             )
                           ),
                           # text = ~paste(
                           #   "<b>", format(as.Date(date), "%B %d"), "</b>", format(typical_vmt_diff, digits = 1) , "<br>",
                           #   stringr::str_wrap(action, width = 20)
                           # )
      ) %>% 
      add_annotations(
        data = covid.traffic.trends::mn_actions,
        text = stringr::str_wrap(covid.traffic.trends::mn_actions$action, width = 35),
        x = covid.traffic.trends::mn_actions$date,
        y = covid.traffic.trends::mn_actions$typical_vmt_diff,
        showarrow = TRUE,
        xanchor = "right",
        yanchor = "top",
        arrowside = "none",
        standoff = 5,
        axref = "x",
        ayref = "y",
        ax = as.Date(covid.traffic.trends::mn_actions$date) - 2,
        ay = covid.traffic.trends::mn_actions$typical_vmt_diff - 15,
        # xshift = -20,
        # yshift = -30,
        font = list(
          size = 12,
          family = "'Raleway', 'HelveticaNeue', 'Helvetica Neue',
                           Helvetica, Arial, sans-serif",
          color = "black"
          
        )
      ) %>%
      layout( #-----
              margin = list(l = 10, r = 45, b = 10, t = 10, pad = 10), # l = left; r = right; t = top; b = bottom
              # title ="Metro Area Traffic: Difference between expected and observed",
              annotations = list(
                text = paste(
                             "<i>", "Data last updated", 
                             max(c(predicted_actual_by_state$date, as.Date(predicted_actual_by_region$date))),
                             "</i>"),
                x = 1, 
                y = -0.1,
                showarrow = F,
                xref = "paper", yref = "paper",
                xanchor = "right", yanchor = "auto",
                xshift = 0, yshift = -0.3
              ),
              hovermode = "closest",
              hoverdistance = "10",
              hoverlabel = list( #----
                                 font = list(
                                   size = 20,
                                   family = "'Raleway', 'HelveticaNeue', 'Helvetica Neue',
                           Helvetica, Arial, sans-serif",
                                   color = "black"
                                 ),
                                 bgcolor = "white",
                                 stroke = list(suppGray, suppGray, suppGray, suppGray),
                                 padding = list(l = 5, r = 5, b = 5, t = 5)
              ),
              xaxis = list( #----
                            title = "",
                            type = "date",
                            tickformat = "%A, %B %d",
                            
                            ## spikes
                            # showspikes = TRUE,
                            # spikesnap = "cursor",
                            # spikedash = "solid",
                            # spikemode = "toaxis+across",
                            # spikecolor = "black",
                            
                            zeroline = FALSE,
                            showline = FALSE,
                            showgrid = TRUE,
                            tickfont = list(
                              size = 14,
                              family = "'Raleway', 'HelveticaNeue', 'Helvetica Neue',
                           Helvetica, Arial, sans-serif",
                              color = "black"
                            )
              ),
              yaxis = list( #----
                            title = "% difference from typical traffic",
                            titlefont = list(
                              size = 14,
                              family = "'Raleway', 'HelveticaNeue', 'Helvetica Neue',
                           Helvetica, Arial, sans-serif",
                              color = "black"
                            ),
                            ticksuffix = "%",
                            tickfont = list(
                              size = 12,
                              family = "'Raleway', 'HelveticaNeue', 'Helvetica Neue',
                           Helvetica, Arial, sans-serif",
                              color = "black"
                            ),
                            zeroline = TRUE,
                            showline = FALSE,
                            showgrid = FALSE
              ),
              legend = list(
                font = list(
                  size = 14,
                  family = "'Raleway', 'HelveticaNeue', 'Helvetica Neue',
                           Helvetica, Arial, sans-serif",
                  color = "black"
                )  
              )
      )
  })
}

## To be copied in the UI
# mod_plot_ui("plot_ui_1")

## To be copied in the server
# callModule(mod_plot_server, "plot_ui_1")
