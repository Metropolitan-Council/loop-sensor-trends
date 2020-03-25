# toolbox #####
library(sf)
library(data.table)
library(tidyverse)
################

# CTUS ####
# ...shapefile ####

ctu_sf <- st_read('data/thrive2040shapefile/ThriveMSP2040CommunityDesignation.shp')
ctu_sf <- st_transform(ctu_sf, crs = 4326)

# SENSOR DATA ####
#....data table ####
sensor_dt <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')
node_dt <- unique(sensor_dt[,.(r_node_name, r_node_n_type, r_node_lon, r_node_lat, r_node_station_id, corridor_route, corridor_dir)])
rm(sensor_dt)

#...shapefile ####
node_sf <- st_as_sf(node_dt, coords = c('r_node_lon', 'r_node_lat'), crs = 4326)

# PREDICTED & ACUTAL VOLUME, BY NODE ####
# ....DATA TABLE ####
diffs_dt <- fread('data/predicted-and-actual-volumes-2020-03-24.csv')
diffs_dt[,date:=as.IDate(date)]
diffs_dt <- diffs_dt[date>'2020-03-01',]
# ....SHAPEFILE ####
diffs_sf <- st_as_sf(diffs_dt, coords = c('r_node_lon', 'r_node_lat'), crs = 4326)

# VOLUMES, BY CTU #### 
ctu_diffs_join <- st_join(diffs_sf, ctu_sf, join = st_within) # this takes TIME, especially with a lot of data. 
# Aggregate predicted & actual volumes by CTU ####
ctu_diffs_join <- ctu_diffs_join%>%
  group_by(CTU_NAME, date, year, dow, doy, woy, weekday, monthday)%>%
  summarise(avg.diff = mean(volume.diff), 
            total.vol = sum(volume.sum), 
            pred.vol = sum(volume.predict),
            num.nodes = uniqueN(r_node_name))

ctu_diffs_sf <- st_join(ctu_sf, ctu_diffs_join, on = 'CTU_NAME')
ctu_diffs_sf <- ctu_diffs_sf%>%filter(!is.na(date))
ctu_diffs_dt <- data.table(ctu_diffs_join)
ctu_diffs_dt$geometry<-NULL
str(ctu_diffs_dt)

rm(ctu_diffs_join)

ctu_labels <- sprintf(
  "<strong>%s</strong>
      <br>Expected Volume: %s vehicles
      <br>Actual Volume: %s vehicles
      <br>Difference from Expected:<strong> %s %%</strong>",
  ctu_diffs_dt[,CTU_NAME.y],
  ctu_diffs_dt[,round(pred.vol)],
  ctu_diffs_dt[,round(total.vol)],
  ctu_diffs_dt[,round(avg.diff, 1)])%>% 
  lapply(htmltools::HTML)

save.image(file = 'traffic-trends-app/appdata.RData')

