library(doParallel)
library(foreach)
# library(devtools)
# install_github("ashleyasmus/tc.sensors")
library(tc.sensors)
library(data.table)
library(sf)
library(odbc)
library(leaflet)
library(tidyverse)

sensor_config <- pull_configuration()
sensor_ids <- pull_sensor_ids()



# Select sensors in Metro Area Only ####
# pull metro area shapefile
db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")
metro_area_shp <- DBI::dbGetQuery(db,"SELECT
                        *,
                        SHAPE.STAsText() as geometry
                        FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;") %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
# project sensor lat-longs
sensor_config_sf <- st_as_sf(sensor_config, coords = c('r_node_lon', 'r_node_lat'), crs = 4326)
# subset only to those that lie in the twin cities area
sensor_config_sf <- st_intersection(sensor_config_sf, metro_area_shp)
sensor_config <- sensor_config[sensor_config$detector_name %in% unique(sensor_config_sf$detector_name),]



# # Select just one corridor: I-94 ####
# sort(unique(sensor_config$corridor_route))
# sensors_94 <- sensor_config[sensor_config$corridor_route == "I-94",]


# # I-94 stations is 744 features alone
# sensor_config_sf%>%
#   filter(corridor_route %in% c('I-94'))%>%
#   filter(r_node_n_type %in% c('Station'))%>%
#   leaflet()%>%
#   addCircleMarkers(label = ~(r_node_name))%>%
#   addTiles()
  
# # Chosen sensor List ####
chosen_sensors_dt <- sensor_config%>%
  # filter(corridor_route %in% c('I-94'))%>% # to select only I-94
  filter(r_node_n_type %in% c('Station'))%>%
  filter(!detector_category == "CD")

chosen_sensors_dt <- data.table(chosen_sensors_dt)

chosen_sensors <- chosen_sensors_dt$detector_name


cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


foreach(j = chosen_sensors) %dopar% {
  date_range <- c(Sys.Date()-1)
  # date_range <- c(seq(as.Date("2020/03/18"), Sys.Date(), by = "days"))
  n <- length(date_range)
  loops_ls <- vector("list", n)
  
  for (i in 1:n) {
    loops_ls[[i]] <- tc.sensors::pull_sensor(j, date_range[[i]])
  }

  loops_df <- data.table::rbindlist(loops_ls)
  
  # This part me - gapfill:
  library(data.table)
  loops_df[,date:=as.IDate(date)]
  setorder(loops_df, date)
  loops_df[,year:=year(date)]
  loops_df[,`:=`(volume.rollmedian.hour = shift(frollapply(volume, 2*60, median, align = 'center', na.rm = T, hasNA = T))), 
         by = year]
  loops_df[,`:=`(occupancy.rollmedian.hour = shift(frollapply(volume, 2*60, median, align = 'center', na.rm = T, hasNA = T))),
         by = year]
  
  loops_df[,volume:=ifelse(is.na(volume), volume.rollmedian.hour, volume)]
  loops_df[,occupancy:=ifelse(is.na(occupancy), occupancy.rollmedian.hour, occupancy)]
  
  loops_df[,c('volume.rollmedian.hour', 'occupancy.rollmedian.hour'):=NULL]
  # this part me - aggregate to hourly: 
  loops_df <- loops_df[, as.list(unlist(lapply(.SD, function(x) list(nulls = sum(is.na(x)),
                                                                  sum = sum(x, na.rm = T),
                                                                  mean = mean(x, na.rm = T),
                                                                  median = median(x, na.rm = T))))),
                         by=.(date, hour, sensor), .SDcols=c("volume", "occupancy")]

  data.table::fwrite(loops_df, paste0("data\\data_hourly_20200318\\Sensor ", j, ".csv"), append = T)
}

stopCluster(cl)
