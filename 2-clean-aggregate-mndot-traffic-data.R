##############
library(data.table)
library(foreach)
library(doParallel)
library(lubridate)
#############

# READ DATA (takes about 1.5 minutes)----
config <- fread('Configuration of Metro Detectors 2020-03-24.csv')
config$date<-NULL

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


sensors <- list.files('data/data_hourly_raw')

hourlydat_sensor <- rbindlist(foreach(i = sensors) %dopar% {
  data.table::fread(paste0("data/data_hourly_raw/", i))
})

stopCluster(cl)


# EXTRACT COLUMNS ----
# don't need occupancy data, only volume data: 
hourlydat_sensor[,c('occupancy.nulls', 'occupancy.sum', 'occupancy.mean', 'occupancy.median'):=NULL]

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

# GAPFILLING 1: FILL IN MISSING MINUTES WITHIN HOURS ----
# sometimes a few observations were missing within an hour (volume.nulls). 
# each hour had 120 30-second observation windows.
# calculate a new volume with the average volume for that hour, as long as there are at least 10 minutes of data (less than 100 nulls)
# sum, mean, median, nad number of nulls already calculated when the data are pulled.
hourlydat_sensor[,volume.sum.raw:=volume.sum] # copy the column for safekeeping
hourlydat_sensor[,volume.sum := ifelse(volume.nulls <=100, round(volume.sum.raw + volume.nulls*volume.mean), volume.sum.raw)]

# if more than 100 nulls (>50 minutes missing), call it NA for that hour:
hourlydat_sensor[,volume.sum := ifelse(volume.nulls>100, NA, volume.sum)]


# GET RID OF IMPOSSIBLE VOLUME VALUES ----
        # hist(hourlydat_sensor$volume.sum)
        # hourlydat_sensor[volume.sum>3000] # greater than 3000 cars per hour? 
        # uniqueN(hourlydat_sensor[volume.sum>3000,sensor])  # 117 sensors
        # plotly::ggplotly(ggplot(hourlydat_sensor[volume.sum>3000,], aes(x = sensor))+
        #   geom_bar(stat ="count"))
        #sensor 2211, 2772,3994,4049, 6283, 6285
        # config[detector_name %in% c('2211', '2772', '3994', '4049', '6283', '6285', 'T3518')]
        
        # 3000/60 # that's 50 cars per minute!
        # exclude these (0.06% of observations)
hourlydat_sensor <- hourlydat_sensor[,volume.sum:=ifelse(volume.sum>=2300, NA, volume.sum)]

# 0 cars per hour? 
# hist(hourlydat_sensor[volume.sum == 0, date], breaks = 'weeks')

# DELETE OCCUPANCY COLUMNS - DON'T NEED ----
hourlydat_sensor[,c('volume.nulls', 'volume.mean', 'volume.median', 'volume.sum.raw'):=NULL]

# GAPFILLING 2: WHOLE HOURS MISSING ----
# if a whole hour is missing, fill in with average of the two hours on either side of it
setorder(hourlydat_sensor, sensor, year, date, hour)
hourlydat_sensor[,`:=`(volume.sum.rollmean = shift(frollapply(volume.sum, 3, mean, align = 'center', na.rm = T, hasNA = T))), 
          by = .(sensor, year)] # when all data is downloaded, and no breaks in the time series, can get rid of by-year argument
hourlydat_sensor[,volume.sum:=ifelse(is.na(volume.sum), volume.sum.rollmean, volume.sum)]
# hist(hourlydat_sensor[is.na(volume.sum), hour]) # for some reason a lot of NAs at 0:00 (12 AM). wonder why...?
# A full ten percent of 12 AM observations missing?? ####
nrow(hourlydat_sensor[is.na(volume.sum) & hour == 0])/nrow(hourlydat_sensor[hour ==0])

# Best to delete the midnight observations for now, but we should check on this with mndot
hourlydat_sensor <- hourlydat_sensor[hour > 0]
hourlydat_sensor[,c('volume.sum.rollmean'):=NULL]


# AGGREGATE TO NODES ----
hourlydat_sensor <- merge(hourlydat_sensor, config, all.x = T, all.y = F, 
                   by.x = 'sensor', by.y = 'detector_name')

# count number of lanes with data for each hour, at node ----
hourlydat_sensor[,num_lanes_with_data_this_hr:=sum(!is.na(volume.sum)), 
          by = .(r_node_name, r_node_station_id, r_node_n_type, r_node_transition, r_node_label, r_node_lanes, 
                 r_node_lon, r_node_lat, 
                 corridor_route, corridor_dir,
                 year, date, hour)]

# hist(hourlydat_sensor[,num_lanes_with_data_this_hr-r_node_lanes], 
#      breaks = 2*max(abs(hourlydat_sensor[,num_lanes_with_data_this_hr-r_node_lanes])))

# sometimes more lanes than the node is reported to have from mndot (r_node_lanes). sometimes less. Confusing!
# count number of unique sensors this year for this node: 
hourlydat_sensor[,num_sensors_this_year:=uniqueN(sensor), 
          by = .(r_node_name, year)]


# hourlydat_sensor[num_sensors_this_year>6,]
# hist(hourlydat_sensor$num_sensors_this_year - hourlydat_sensor$num_lanes_with_data_this_hr)
# Yes, I believe this number of sensors instead

# exclude to only those hourly observations at each node where the number of lanes
# equals the number of detectors for this year.
hourlydat_sensor <- hourlydat_sensor[num_lanes_with_data_this_hr == num_sensors_this_year]


# WRITE HOURLY DATA ! ----
fwrite(hourlydat_sensor, 'data/data_hourly_bysensor_clean.csv')


hourlydat_node <- hourlydat_sensor[,lapply(.SD, sum), 
                       .SDcols = 'volume.sum',
                       by = .(r_node_name, r_node_station_id, r_node_n_type, r_node_transition, r_node_label, r_node_lanes, 
                              r_node_lon, r_node_lat,  
                              corridor_route, corridor_dir,
                              year, date, hour,
                              num_lanes_with_data_this_hr, num_sensors_this_year)]

fwrite(hourlydat_node, 'data/data_hourly_bynode_clean.csv')

# sum for all hours of the day (daily-scale data) ----
dailydat <- hourlydat_node[,lapply(.SD, sum), .SDcols = 'volume.sum', 
                      by = .(r_node_name, r_node_station_id, r_node_n_type, r_node_transition, r_node_label, r_node_lanes, 
                             r_node_lon, r_node_lat, 
                             num_sensors_this_year,
                             corridor_route, corridor_dir,
                             year, date)]

# WRITE DAILY DATA! ----
fwrite(dailydat, 'data/data_daily_bynode_clean.csv')
