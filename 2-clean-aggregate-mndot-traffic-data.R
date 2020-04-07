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
    
    fwrite(hourlydat_node, paste0('data/data_hourly_node/', this_node, '.csv'))
    
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
    fwrite(dailydat, paste0('data/data_daily_node/', this_node, '.csv'))
    
  } # end check for nodes with no data at all
}



stopCluster(cl)



