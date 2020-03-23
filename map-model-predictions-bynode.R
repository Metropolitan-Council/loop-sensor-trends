#############
library(data.table)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(tidyverse)
library(sf)
#############

diffs <- fread('data/predicted-and-actual-volumes-2020-03-23.csv')
diffs <- data.table(diffs)
diffs[,scl_volume:=scale(diffs$volume.predict, center= F)]
diffs[,date:=as.IDate(date)]

diffs_sf <- st_as_sf(diffs, coords = c('r_node_lon', 'r_node_lat'), crs = 4326)

binpal <- colorBin("RdBu", diffs$volume.diff, 
                   bins = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100), 
                   pretty = FALSE, reverse = T)

# hist(diffs$volume.predict)
# hist(diffs$scl_volume)

node_labels <- sprintf(
  "<strong>%s %s</strong>
  <br>Node ID: %s
      <br>Expected Volume: %s vehicles
      <br>Actual Volume: %s vehicles
      <br>Difference from Expected:<strong> %s %%</strong>",
  diffs[,corridor_route ], 
  diffs[,corridor_dir], 
  diffs[,r_node_name],
  diffs[,round(volume.predict)], 
  diffs[,volume.sum], 
  diffs[,round(volume.diff, 1)])%>% 
  lapply(htmltools::HTML)


length(unique(diffs$r_node_name)) # 1179 2020-03-22


m<-
  leaflet(diffs_sf)%>%
  addProviderTiles('CartoDB.DarkMatter')%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-22' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-22' ],
                   group = "Sun. March 22")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-21' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-21' ],
                   group = "Sat. March 21")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-20' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-20' ],
                   group = "Fri. March 20")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-19' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-19' ],
                   group = "Thurs. March 19")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-18' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-18' ],
                   group = "Weds. March 18")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-17' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-17' ],
                   group = "Tues. March 17")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-16' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-16' ],
                   group = "Mon. March 16")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-15' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-15' ],
                   group = "Sun. March 15")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-14' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-14' ],
                   group = "Sat. March 14")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-13' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-13' ],
                   group = "Fri. March 13")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-12' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-12' ],
                   group = "Thurs. March 12")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-11' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-11' ],
                   group = "Weds. March 11")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-10' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-10' ],
                   group = "Tues. March 10")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-09' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-09' ],
                   group = "Mon. March 9")%>%
  addLayersControl(
    baseGroups = c("Mon. March 9", "Tues. March 10", "Weds. March 11", "Thurs. March 12","Fri. March 13","Sat. March 14",
                   "Sun. March 15","Mon. March 16","Tues. March 17", "Weds. March 18", "Thurs. March 19", "Fri. March 20", "Sat. March 21",
                   "Sun. March 22"),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend(pal = binpal, values = ~volume.diff, title = "% Change in Traffic")

library(htmlwidgets)
setwd('output')
saveWidget(m, file = paste0("metro-volumes-by-node-", Sys.Date(), ".html"))


