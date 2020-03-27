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
  
  output$plot <- renderPlotly({
    plot_ly() %>%
      plotly::add_markers(
        data = covid.traffic.trends::predicted_actual_by_region["doy" > 60 & "year" == 2020],
        x = covid.traffic.trends::predicted_actual_by_region$date,
        y = covid.traffic.trends::predicted_actual_by_region$typical_vmt_diff,
        name = "MnDOT Metro\n(1000+ Stations)\n",
        mode = "lines+markers",
        line = list(
          width = 2,
          color = councilBlue
        ),
        marker = list(color = suppGray,
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
          width = 2,
          color = "black"
        ),
        marker = list(
          color = suppGray,
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
        text = stringr::str_wrap(covid.traffic.trends::mn_actions$action, width = 20),
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
        ay = covid.traffic.trends::mn_actions$typical_vmt_diff - 8,
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
                text = paste("<i>", "Data last updated", "2020-03-27", "</i>"),
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
                            tickformat = "%B %d",
                            
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
