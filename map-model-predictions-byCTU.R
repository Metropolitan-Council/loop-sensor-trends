
# Aggregate to Thrive 2040 Designations

t2040shp <- st_read('data/thrive2040shapefile/ThriveMSP2040CommunityDesignation.shp')
t2040shp <- st_transform(t2040shp, crs = 4326)
mctu <- st_join(diffs_sf, t2040shp, join = st_within)
mctu <- mctu%>%
  group_by(CTU_NAME, date, year, dow, doy, woy, weekday, monthday)%>%
  summarise(avg.diff = mean(volume.diff), total.vol = sum(volume.sum), pred.vol = sum(volume.predict))

mctu2 <- st_join(t2040shp, mctu, on = 'CTU_NAME')
mctu2 <- mctu2%>%filter(!is.na(date))
mctu2df <- data.table(mctu2)
mctu2df$geometry<-NULL
str(mctu2df)
# str(mctu2df)
ctu_labels <- sprintf(
  "<strong>%s</strong>
      <br>Expected Volume: %s vehicles
      <br>Actual Volume: %s vehicles
      <br>Difference from Expected:<strong> %s %%</strong>",
  mctu2df[,CTU_NAME.y],
  mctu2df[,round(pred.vol)],
  mctu2df[,round(total.vol)],
  mctu2df[,round(avg.diff, 1)])%>% 
  lapply(htmltools::HTML)

m2<-
  leaflet(mctu2)%>%
  addProviderTiles('CartoDB.DarkMatter')%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-20' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-20'],
              group = "Fri. March 20")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-19' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-19'],
              group = "Thurs. March 19")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-18' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-18'],
              group = "Weds. March 18")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-17' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-17'],
              group = "Tues. March 17")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-16' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-16'],
              group = "Mon. March 16")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-15' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-15'],
              group = "Sun. March 15")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-14' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-14'],
              group = "Sat. March 14")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-13' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-13'],
              group = "Fri. March 13")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-12' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-12'],
              group = "Thurs. March 12")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-11' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-11'],
              group = "Weds. March 11")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-10' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-10'],
              group = "Tues. March 10")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-09' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-09'],
              group = "Mon. March 9")%>%
  addLayersControl(
    baseGroups = c("Mon. March 9", "Tues. March 10", "Weds. March 11", "Thurs. March 12","Fri. March 13","Sat. March 14",
                   "Sun. March 15","Mon. March 16","Tues. March 17", "Weds. March 18", "Thurs. March 19", "Fri. March 20"),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend(pal = binpal, values = ~avg.diff, title = "% Change in Traffic")


m2
saveWidget(m2, file="metro-volumes-by-ctu-2020-03-21.html")