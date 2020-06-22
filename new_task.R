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

setwd('N:\\MTS\\Working\\Modeling\\MetroLoopDetectors\\loop-sensor-trends')
sensor_config <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')

# Select sensors in Metro Area Only ####
# pull metro area shapefile
# db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")
# metro_area_shp <- DBI::dbGetQuery(db,"SELECT
#                         *,
#                         SHAPE.STAsText() as geometry
#                         FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;") %>%
#   st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
#   st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")


# project sensor lat-longs
# sensor_config_sf <- st_as_sf(sensor_config, coords = c('r_node_lon', 'r_node_lat'), crs = 4326)
# subset only to those that lie in the twin cities area
# sensor_config_sf <- st_intersection(sensor_config_sf, metro_area_shp)
# sensor_config <- sensor_config[sensor_config$detector_name %in% unique(sensor_config_sf$detector_name),]



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

# # find sensors that already have data
# already_have_data <- data.frame(list.files('data/data_hourly_raw/'))
# names(already_have_data)<-"Sensor"
# already_have_data <- lapply(already_have_data, FUN = function(x) gsub(x, pattern = "Sensor ", replacement = ""))
# already_have_data <- lapply(already_have_data, FUN = function(x) gsub(x, pattern = ".csv", replacement = ""))

# # Chosen sensor List ####
chosen_sensors_dt <- sensor_config%>%
  # filter(corridor_route %in% c('I-94'))%>% # to select only I-94
  # filter(r_node_n_type %in% c('Station'))%>% want entrance/exit ramps now too
  filter(!detector_category == "CD")
  # filter(!detector_name %in% already_have_data$Sensor)

chosen_sensors_dt <- data.table(chosen_sensors_dt)

chosen_sensors <- chosen_sensors_dt$detector_name


cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

# tictoc::tic()
foreach(j = chosen_sensors) %dopar% {
  # date_range <- c(Sys.Date()-1) # yesterday's data
  date_range <- c(seq(Sys.Date()-3, Sys.Date()-1, by = "days"))

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
  # gapfill - hours -- i don't think this works? 
  # if(nrow(loops_df[is.na(volume)])>0){
  #   loops_df[,`:=`(volume.rollmedian.hour = shift(frollapply(volume, 2*60, median, align = 'center', na.rm = T, hasNA = T))), 
  #            by = year]
  #   # loops_df[,`:=`(occupancy.rollmedian.hour = shift(frollapply(volume, 2*60, median, align = 'center', na.rm = T, hasNA = T))),
  #   #        by = year]
  #   
  #   loops_df[,volume:=ifelse(is.na(volume), volume.rollmedian.hour, volume)]
  #   # loops_df[,occupancy:=ifelse(is.na(occupancy), occupancy.rollmedian.hour, occupancy)]
  #   
  #   loops_df[,c('volume.rollmedian.hour'):=NULL]
  # }else{
  #   loops_df <- loops_df
  # }
  
  # this part me - aggregate to hourly: 
  loops_df <- loops_df[, as.list(unlist(lapply(.SD, function(x) list(nulls = sum(is.na(x)),
                                                                     sum = sum(x, na.rm = T),
                                                                     mean = mean(x, na.rm = T),
                                                                     median = median(x, na.rm = T))))),
                       by=.(date, hour, sensor), .SDcols=c("volume", "occupancy")]
  
  data.table::fwrite(loops_df, paste0("data/data_hourly_raw/Sensor ", j, ".csv"), append = T)
  
  
}

stopCluster(cl)
# tictoc::toc()

##############
library(data.table)
library(foreach)
library(doParallel)
library(lubridate)
#############

# READ DATA (takes about 1.5 minutes)----
config <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date<-NULL

sensor_files <- list.files('data/data_hourly_raw')
sensor_names <- gsub('.csv', '', sensor_files)
sensor_names <- gsub('Sensor ', '', sensor_names)

# Node-Sensor Lookup Table ----
node_lut <- config[,.(r_node_name, detector_name)]
node_lut <- node_lut[,sensor_name:=as.character(detector_name)]
node_lut <- node_lut[detector_name %in% sensor_names,]
node_lut <- split(node_lut, node_lut$r_node_name)


cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

# node_lut <- node_lut[100:120] # test

# CLEAN UP TIME -----

foreach(i = node_lut) %dopar% {
  
  library(data.table)
  library(lubridate)
  
  
  node_info <- i
  # node_info <- node_lut[[1010]] # test
  this_node <- node_info$r_node_name[[1]]
  
  these_sensors <- paste0('Sensor ', node_info$sensor_name)
  
  
  these_sensor_files <- paste0(paste0('data/data_hourly_raw/', these_sensors, '.csv'))
  
  hourlydat_sensor <- rbindlist(lapply(these_sensor_files, fread))
  hourlydat_sensor[,sensor:=as.character(sensor)]
  hourlydat_sensor[date == Sys.Date()-1,]
  
  # CHECK FOR NODES MISSING ALL DATA
  total_zilch <- sum(!is.na(hourlydat_sensor$hour))
  if(total_zilch == 0) { } else{
    
    # EXTRACT COLUMNS ----
    # don't need occupancy data, only volume data: 
    # hourlydat_sensor[,c('occupancy.nulls', 'occupancy.sum', 'occupancy.mean', 'occupancy.median'):=NULL]
    
    # DELETE DUPLICATE OBSERVATIONS ----
    # Find duplicate observations -- multiple pulls are written into the same data file
    # example  -- hourlydat_sensor[date == '2020-03-19' & sensor == 100]
    # get only the second observation:
    hourlydat_sensor <- hourlydat_sensor[!duplicated(hourlydat_sensor, by = c('date', 'hour', 'sensor'), fromLast=TRUE)]
    
    # EXPAND DATASET FOR MISSING DAYS OF DATA (takes about 2 minutes)----
    # for whole days with no observations, "hour" is NA. Get rid of these for now, then fill in
    hourlydat_sensor <- hourlydat_sensor[!is.na(hour)]
    
    # create master matrix of dates and sensors, empty of data.
    master_mat <- expand.grid(date = unique(hourlydat_sensor$date), 
                              sensor = unique(hourlydat_sensor$sensor),
                              hour = 0:23)
    master_mat <- data.table(master_mat)
    master_mat <- master_mat[,lapply(.SD, as.character), .SDcols = c('date', 'sensor'), by = 'hour']
    
    # merge master to hourly data to fill in NA values for missing observations
    hourlydat_sensor <- merge(hourlydat_sensor, master_mat, by = c('date', 'sensor', 'hour'), all = T)
    
    # FORMAT DATE ----
    # dealing with date
    hourlydat_sensor[,date:=as.IDate(fast_strptime(date, "%Y-%m-%d"))] 
    hourlydat_sensor[,year:=year(date)]
    
    # GET RID OF IMPOSSIBLE VALUES ----
    hourlydat_sensor <- hourlydat_sensor[,volume.sum:=ifelse(volume.sum>=2300, NA, volume.sum)]
    # 60 scans per second * 60 secs per min * 60 mins per hour = 216,000 scans per hour
    hourlydat_sensor <- hourlydat_sensor[,occupancy.sum:=ifelse(occupancy.sum>=216000, NA, occupancy.sum)]
    
    
    # GAPFILLING 1: FILL IN MISSING MINUTES WITHIN HOURS ----
    # sometimes a few observations were missing within an hour (volume.nulls). 
    # each hour had 120 30-second observation windows.
    # calculate a new volume with the average volume for that hour, as long as there are at least 10 minutes of data (less than 100 nulls)
    # sum, mean, median, nad number of nulls already calculated when the data are pulled.
    hourlydat_sensor[,volume.sum.raw:=volume.sum] # copy the column for safekeeping
    hourlydat_sensor[,volume.sum := ifelse(volume.nulls <=100, round(volume.sum.raw + volume.nulls*volume.mean), volume.sum.raw)]
    
    hourlydat_sensor[,occupancy.sum.raw:=occupancy.sum] # copy the column for safekeeping
    hourlydat_sensor[,occupancy.sum := ifelse(occupancy.nulls <=100, round(occupancy.sum.raw + occupancy.nulls*occupancy.mean), occupancy.sum.raw)]
    
    
    # if more than 100 nulls (>50 minutes missing), call it NA for that hour:
    hourlydat_sensor[,volume.sum := ifelse(volume.nulls>100, NA, volume.sum)]
    hourlydat_sensor[,occupancy.sum := ifelse(occupancy.nulls>100, NA, occupancy.sum)]
    
    
    # DELETE OCCUPANCY COLUMNS - DON'T NEED ----
    hourlydat_sensor[,c('volume.nulls', 'volume.mean', 'volume.median', 'volume.sum.raw',
                        'occupancy.nulls', 'occupancy.mean', 'occupancy.median', 'occupancy.sum.raw'):=NULL]
    
    # GAPFILLING 2: WHOLE HOURS MISSING ----
    # if a whole hour is missing, fill in with average of the two hours on either side of it
    setorder(hourlydat_sensor, sensor, year, date, hour)
    hourlydat_sensor[,`:=`(volume.sum.rollmean = shift(frollapply(volume.sum, 3, mean, align = 'center', na.rm = T, hasNA = T))), 
                     # by = .(sensor, year)] # when all data is downloaded, and no breaks in the time series, can get rid of by-year argument
                     by = .(sensor)] # all data downloaded!
    hourlydat_sensor[,volume.sum:=ifelse(is.na(volume.sum), volume.sum.rollmean, volume.sum)]
    
    hourlydat_sensor[,`:=`(occupancy.sum.rollmean = shift(frollapply(occupancy.sum, 3, mean, align = 'center', na.rm = T, hasNA = T))), 
                     # by = .(sensor, year)] # when all data is downloaded, and no breaks in the time series, can get rid of by-year argument
                     by = .(sensor)] # all data downloaded!
    hourlydat_sensor[,occupancy.sum:=ifelse(is.na(occupancy.sum), occupancy.sum.rollmean, occupancy.sum)]
    
    # hist(hourlydat_sensor[is.na(volume.sum), hour]) # for some reason a lot of NAs at 0:00 (12 AM). wonder why...?
    # A full ten percent of 12 AM observations missing?? ####
    nrow(hourlydat_sensor[is.na(volume.sum) & hour == 0])/nrow(hourlydat_sensor[hour ==0])
    
    # Best to delete the midnight observations for now, but we should check on this with mndot
    hourlydat_sensor <- hourlydat_sensor[hour > 0]
    hourlydat_sensor[,c('volume.sum.rollmean', 'occupancy.sum.rollmean'):=NULL]
    
    # WRITE HOURLY DATA ! ----
    # data.table::fwrite(hourlydat_sensor, paste0("data/volume_hourly_clean/Sensor ", j, ".csv"), append = T)
    
    # MERGE BACK TO CONFIGURATION FILE --- 
    hourlydat_sensor <- merge(hourlydat_sensor, config, all.x = T, all.y = F, 
                              by.x = 'sensor', by.y = 'detector_name')
    
    # CALCULATE SPEED --- 
    hourlydat_sensor[,occupancy.pct := (occupancy.sum/216000)]
    hourlydat_sensor[,speed:=ifelse(volume.sum != 0, 
                                    (volume.sum*detector_field)/(5280*occupancy.pct), 0)]
    
    # AGGREGATE TO NODES ----
    # count number of lanes with data for each hour, at node ----
    hourlydat_sensor[,num_lanes_with_data_this_hr:=sum(!is.na(volume.sum)), 
                     by = .(r_node_name, r_node_station_id, r_node_n_type, r_node_transition, r_node_label, r_node_lanes, 
                            r_node_lon, r_node_lat, 
                            corridor_route, corridor_dir,
                            year, date, hour)]
    
    
    # sometimes more lanes than the node is reported to have from mndot (r_node_lanes). sometimes less. Confusing!
    # count number of unique sensors this year for this node: 
    hourlydat_sensor[,num_sensors_this_year:=uniqueN(sensor), 
                     by = .(r_node_name, year)]
    
    
    # exclude to only those hourly observations at each node where the number of lanes
    # equals the number of detectors for this year.
    hourlydat_sensor <- hourlydat_sensor[num_lanes_with_data_this_hr == num_sensors_this_year]
    
    hourlydat_node <- hourlydat_sensor[,as.list(unlist(lapply(.SD,
                                                              function(x) list(sum = sum(x), 
                                                                               mean = mean(x))))),
                                       by = .(r_node_name, r_node_station_id, r_node_n_type, r_node_transition, r_node_label, r_node_lanes, 
                                              r_node_lon, r_node_lat,  
                                              corridor_route, corridor_dir,
                                              year, date, hour,
                                              num_lanes_with_data_this_hr, num_sensors_this_year),
                                       .SDcols = c('volume.sum', 'occupancy.sum', 'speed')]
    hourlydat_node[,c('volume.sum.mean', 'occupancy.sum.mean', 'speed.sum'):=NULL]
    setnames(hourlydat_node, old = 'volume.sum.sum', new = 'volume.sum')
    setnames(hourlydat_node, old = 'occupancy.sum.sum', new = 'occupancy.sum')
    setnames(hourlydat_node, old = 'speed.mean', new = 'speed')
    
    # fwrite(hourlydat_node, paste0('data/data_hourly_node/', this_node, '.csv'), append = T)
    
    # sum for all hours of the day (daily-scale data) ----
    dailydat <- hourlydat_node[,as.list(unlist(lapply(.SD,
                                                      function(x) list(sum = sum(x), 
                                                                       mean = mean(x))))),
                               by = .(r_node_name, r_node_station_id, r_node_n_type, r_node_transition, r_node_label, r_node_lanes, 
                                      r_node_lon, r_node_lat,  
                                      corridor_route, corridor_dir,
                                      year, date),
                               .SDcols = c('volume.sum', 'occupancy.sum', 'speed')]
    dailydat[,c('volume.sum.mean', 'occupancy.sum.mean', 'speed.sum'):=NULL]
    setnames(dailydat, old = 'volume.sum.sum', new = 'volume.sum')
    setnames(dailydat, old = 'occupancy.sum.sum', new = 'occupancy.sum')
    setnames(dailydat, old = 'speed.mean', new = 'speed')
    
    # WRITE DAILY DATA! ----
    fwrite(dailydat, paste0('data/data_daily_node/', this_node, '.csv'), append = T)
    
  } # end check for nodes with no data at all
}



stopCluster(cl)

##############
library(data.table)
library(foreach)
library(doParallel)
library(mgcv)
library(lubridate)
#############
config <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date<-NULL
det_config <- unique(config[,.(r_node_name, r_node_n_type, 
                               r_node_transition, r_node_label, r_node_lon, r_node_lat, 
                               r_node_lanes, r_node_shift, r_node_s_limit, r_node_station_id, 
                               r_node_attach_side, corridor_route, corridor_dir)])

node_files <- list.files('data/data_daily_node')
node_names <- gsub('.csv', '', node_files)
# gam_list <- vector("list", length(node_names))
# pred_ls <- vector("list", length(node_names))

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


# node_files <- node_files[1:10] # test


foreach(i = node_files) %dopar% {
  
  print(i) 
  flush.console()
  library(data.table)
  library(lubridate)
  library(mgcv)
  
  
  # i <- node_files[[3245]] # test
  # i <- "rnd_1805.csv"
  dailydat <- fread(paste0("data/data_daily_node/", i))
  dailydat <- unique(dailydat)
  if(nrow(dailydat) == 0){} else{
    
    # Dealing with date ----
    dailydat[, date := as.IDate(fast_strptime(date, "%Y-%m-%d"))]
    dailydat[, dow := wday(date)]
    dailydat[, doy := yday(date)]
    dailydat[, year := year(date)]
    dailydat[, woy := week(date)]
    dailydat[, weekday := factor(weekdays(date))]
    dailydat[, monthday := format(date, "%b %d")]
    
    
    # get rid of 2017 data: (december 15-31 included in this pull) ----
    dailydat <- dailydat[year > 2017, ]
    
    # must have 3 years of data, at least 60 days of data in each year ----
    dailydat[, "num_days_per_year" := uniqueN(date), by = .(r_node_name, year)]
    dailydat <- dailydat[num_days_per_year > 60]
    
    has_2020_data <- unique(dailydat$r_node_name[dailydat$year == 2020 & dailydat$date < "2020-03-01"])
    has_2018_data <- unique(dailydat$r_node_name[dailydat$year == 2018])
    has_2019_data <- unique(dailydat$r_node_name[dailydat$year == 2019])
    
    dailydat <- dailydat[dailydat$r_node_name %in% has_2020_data
                         & dailydat$r_node_name %in% has_2019_data
                         & dailydat$r_node_name %in% has_2018_data, ]
    if(nrow(dailydat) == 0){} else{
      
      # subset to relevant dates:
      modeling_dat <- dailydat[dailydat$date < "2020-03-01",]
      
      # 2020 data v. sensitive - exclude some special holidays and weather days
      modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-01", ] # holiday - exclude
      modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-17", ] # cold snap - exclude
      modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-18", ] # cold snap - exclude
      modeling_dat <- modeling_dat[!modeling_dat$date == "2020-02-09", ] # snow day - exclude
      
      this_gam <- with(
        modeling_dat,
        mgcv::gam(
          volume.sum ~
            s(dow, k = 7, by = as.factor(year)) # one knot for each day of the week
          + s(doy) # general seasonal trend, let it vary by year, allow knots to be set by gam
          + as.factor(year) # intercept for each year
        )
      )
      
      # gam_list[[i]] <- this_gam
      
      
      # generate predictions
      date_range <- c(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "days"))
      
      predict_dat <- data.table(date = date_range)
      predict_dat[, date := as.IDate(date)]
      predict_dat[, dow := wday(date)]
      predict_dat[, doy := yday(date)]
      predict_dat[, year := year(date)]
      predict_dat[, woy := week(date)]
      predict_dat[, weekday := factor(weekdays(date))]
      predict_dat[, monthday := format(date, "%b %d")]
      predict_dat[, r_node_name := modeling_dat$r_node_name[[1]]]
      
      
      predict_dat <- merge(predict_dat, det_config, all.x = T)
      
      predict_dat[, c("volume.predict", "volume.predict.se") := cbind(predict.gam(object = this_gam, newdata = predict_dat, se.fit = T))]
      
      predict_dat <- merge(predict_dat, 
                           dailydat[,.(r_node_name, date, volume.sum)], all.x = T, 
                           by = c('r_node_name', 'date'))
      
      # difference from predicted, n volume:
      predict_dat[, volume.diff.raw := (volume.sum - volume.predict)]
      
      # difference from predicted, in %:
      predict_dat[, volume.diff := round(((volume.sum - volume.predict) / volume.predict) * 100, 1)]
      
      fwrite(predict_dat, file = paste0('output/daily_model_predictions_bynode/', i))
      
      # store difference from normal for 2020 for mapping
      # pred_ls[[i]] <- predict_dat
      
    } # end check for nodes with missing data
  } # end first check for nodes with no data at all
}


# saveRDS(gam_list, file = paste0('output/gam-models-', Sys.Date(), '.RData'))

stopCluster(cl)




##############
library(data.table)
library(foreach)
library(doParallel)
library(mgcv)
library(lubridate)
#############

node_files <- list.files('output/daily_model_predictions_bynode')
node_names <- gsub('.csv', '', node_files)

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


# node_files <- node_files[1:10] # test


diffs_dt <- rbindlist(foreach(i = node_files) %dopar% {
  library(data.table)
  dat <- fread(paste0('output/daily_model_predictions_bynode/', i))
  dat[, date := as.IDate(date)]
  # Trim to after March 1 2020:
  dat <- dat[date >= "2020-03-01", ]
  dat <- dat[date <= Sys.Date()-1]
  dat <- unique(dat)
  dat
})

stopCluster(cl)

# some very high numbers?
# hist(diffs_dt[,volume.diff])
# summary(diffs_dt[,volume.diff])
unique(diffs_dt[volume.diff < (-100), .(r_node_name, r_node_n_type)])
# r_node_name r_node_n_type
# 1:    rnd_5932          Exit
# 2:    rnd_5948          Exit
# 3:    rnd_5950      Entrance
# 4:   rnd_85131          Exit

unique(diffs_dt[volume.predict < 0, .(r_node_name, r_node_n_type)])

# negative predicted values -- one spurious node
# diffs_dt <- diffs_dt[!r_node_name == 'rnd_86223'] -- now many more
diffs_dt <- diffs_dt[!r_node_name %in% unique(diffs_dt[volume.predict < 0, r_node_name])]

# get rid of NAs
diffs_dt <- diffs_dt[!is.na(volume.diff)]

# other spurious observations (8 of them)
# diffs_dt<-diffs_dt[volume.diff < (100)]
# another problematic node:
# diffs_dt <- diffs_dt[!r_node_name %in% c('rnd_86469', 'rnd_95784')]

# very high values
summary(diffs_dt[volume.diff > 100 & year == 2020, .(volume.predict, volume.diff)])
# how many of these?
# unique(diffs_dt[volume.diff > 500 & year == 2020, .(r_node_name, r_node_n_type, date)])
# 1:     rnd_849          Exit 2020-03-15
# 2:     rnd_849          Exit 2020-03-17
# 3:     rnd_849          Exit 2020-03-18
# 4:     rnd_849          Exit 2020-03-19
# 5:     rnd_849          Exit 2020-03-20
# 6:   rnd_88037      Entrance 2020-03-14
# 7:   rnd_88037      Entrance 2020-03-15
# 8:   rnd_88037      Entrance 2020-03-17
# 9:   rnd_88037      Entrance 2020-03-18

# get rid of thse as well
diffs_dt <- diffs_dt[!r_node_name %in% unique(diffs_dt[volume.diff > 500 & year == 2020, r_node_name])]



fwrite(diffs_dt, paste0("output/pred-and-act-vol-by-node.csv"))
fwrite(diffs_dt, paste0("covid.traffic.trends/data-raw/pred-and-act-vol-by-node.csv"))


# More data reshaping of model output ----
daynodes <- diffs_dt[r_node_n_type == "Station" & year == 2020 # this year's data, stations only. ####
                     , lapply(.SD, mean),
                     .SDcols = c("volume.predict"),
                     by = .(date, dow, doy, year, woy, weekday, monthday)
                     ]


# Total difference from expected for whole metro area ----
diffs_4plot <- diffs_dt[r_node_n_type == "Station" & year == 2020 # this year's data, stations only. ####
                        , lapply(.SD, FUN = function(x) sum(x, na.rm = T)),
                        .SDcols = c("volume.sum", "volume.predict"),
                        by = .(date, dow, doy, year, woy, weekday, monthday)
                        ]

# vmt is 1/2 of volume ----
diffs_4plot[, c("vmt.sum", "vmt.predict") := list(volume.sum * 0.5, volume.predict * 0.5)]
diffs_4plot[, "Difference from Typical VMT (%)" := round(100 * (vmt.sum - vmt.predict) / vmt.predict, 2)]
diffs_4plot[, difference_text := ifelse(`Difference from Typical VMT (%)` < 0, paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % less than typical"),
                                        paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % more than typical")
)]


fwrite(diffs_4plot, paste0("output/pred-and-act-vol-region.csv"))
fwrite(diffs_4plot, paste0("covid.traffic.trends/data-raw/pred-and-act-vol-region.csv"))

