# toolbox #####
library(leaflet)
library(data.table)
library(tidyverse)
################



binpal <- colorNumeric("RdYlBu", diffs$volume.diff, reverse = T)



m2<-
  leaflet(ctu_dat2)%>%
  addProviderTiles('CartoDB.Positron')%>%
  addMapPane("polygons", zIndex = 410) %>%
  addMapPane("points", zIndex = 420) %>%
  addCircleMarkers(data = detshp, color = 'white', radius = 2, fill = FALSE, 
                   group = "Traffic Detector Locations",
                   options = leafletOptions(pane = "points"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-23' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-23'],
              group = "Mon. March 23",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-22' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-22'],
              group = "Sun. March 22")%>%   
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-21' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-21'],
              group = "Sat. March 21",
              options = leafletOptions(pane = "polygons"))%>%    
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-20' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-20'],
              group = "Fri. March 20",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-19' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-19'],
              group = "Thurs. March 19",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-18' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-18'],
              group = "Weds. March 18",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-17' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-17'],
              group = "Tues. March 17",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-16' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-16'],
              group = "Mon. March 16",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-15' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-15'],
              group = "Sun. March 15",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-14' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-14'],
              group = "Sat. March 14",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-13' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-13'],
              group = "Fri. March 13",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-12' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-12'],
              group = "Thurs. March 12",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-11' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-11'],
              group = "Weds. March 11",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-10' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-10'],
              group = "Tues. March 10",
              options = leafletOptions(pane = "polygons"))%>%
  addPolygons(data = ctu_dat2[ctu_dat2$date == '2020-03-09' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[ctu_dat2df$date == '2020-03-09'],
              group = "Mon. March 9",
              options = leafletOptions(pane = "polygons"))%>%
  addLayersControl(
    baseGroups = c("Mon. March 9", "Tues. March 10", "Weds. March 11", "Thurs. March 12","Fri. March 13","Sat. March 14",
                   "Sun. March 15","Mon. March 16","Tues. March 17", "Weds. March 18", "Thurs. March 19", "Fri. March 20", "Sat. March 21",
                   "Sun. March 22", "Mon. March 23"),
    overlayGroups = c("Traffic Detector Locations"),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend(pal = binpal, values = ~avg.diff, title = "% Change in Traffic")

m2

saveWidget(m2, file="metro-volumes-by-ctu-2020-03-24.html")
