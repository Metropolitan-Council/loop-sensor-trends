#############
library(data.table)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)
#############


readRDS('appdata.RData')

binpal <- colorNumeric("RdBu", diffs_sf$volume.diff, reverse = T)


length(unique(diffs_sf$r_node_name)) # 1179 2020-03-22


m<-
  diffs_sf%>%
  filter(date == '2020-03-24') %>%
  leaflet()%>%
  addProviderTiles('CartoDB.DarkMatter')%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$r_node_n_type == "Entrance",],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-24' & diffs_sf$r_node_n_type == "Entrance"],
                   group = "Entrance Ramps")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$r_node_n_type == "Exit",],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-24'& diffs_sf$r_node_n_type == "Exit" ],
                   group = "Exit Ramps")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$r_node_n_type == "Station",],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-24'& diffs_sf$r_node_n_type == "Station" ],
                   group = "Stations")%>%
  addLayersControl(
    baseGroups = c("Entrance Ramps", "Exit Ramps", "Stations"),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend(pal = binpal, values = ~volume.diff, title = "% Change in Traffic")

m

library(htmlwidgets)
setwd('output')
saveWidget(m, file = paste0("test-volumes-by-node-exit-entrance-", Sys.Date(), ".html"))


