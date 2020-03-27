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
    plotlyOutput(ns("plot"))
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
        x = predicted_actual_by_region$date,
        y = predicted_actual_by_region$typical_vmt_diff,
        mode = "lines+markers",
        line = list(
          width = 1,
          color = "black"
        ),
        marker = list(color = "darkgray"),
        hoverinfo = "text",
        text = paste(predicted_actual_by_region$hover_text),
        showlegend = FALSE
      ) %>%
      plotly::add_markers(
        data = covid.traffic.trends::mn_actions,
        x = mn_actions$date,
        y = -30,
        name = "MN Actions",
        mode = "lines+markers",
        hoverinfo = "text",
        marker = list(
          symbol = "hexagon",
          size = 8
        ),
        text = ~ paste(
          "<b>", format(as.Date(date), "%B %d"), "</b>", "<br>",
          stringr::str_wrap(action, width = 20)
        )
      ) %>%
      layout(
        margin = list(l = 10, r = 45, b = 10, t = 10), # l = left; r = right; t = top; b = bottom
        # title ="Metro Area Traffic: Difference between expected and observed",
        annotations = list(
          text = paste("<i>", "Data last updated", "2020-03-26", "</i>"),
          x = 1, y = -0.1,
          showarrow = F,
          xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto",
          xshift = 0, yshift = -0.3
        ),
        hovermode = "x",
        hoverdistance = "10",
        hoverlabel = list( #----
          font = list(
            size = 20,
            family = "'Raleway', 'HelveticaNeue', 'Helvetica Neue',
                           Helvetica, Arial, sans-serif",
            color = "black"
          ),
          bgcolor = "white",
          stroke = list("#eee", "#eee", "#eee", "#eee")
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
          title = "Difference from Typical (%)",
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
        )
      )
  })
}

## To be copied in the UI
# mod_plot_ui("plot_ui_1")

## To be copied in the server
# callModule(mod_plot_server, "plot_ui_1")
