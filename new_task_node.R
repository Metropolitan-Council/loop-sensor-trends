library(doParallel)
library(foreach)
library(devtools)
# devtools::install_github("Metropolitan-Council/tc.sensors", ref = 'main')
library(tc.sensors)
library(data.table)
library(sf)
library(odbc)
library(leaflet)
library(tidyverse)

setwd('N:\\MTS\\Working\\Modeling\\MetroLoopDetectors\\loop-sensor-trends')
sensor_config <-
  fread('data/Configuration of Metro Detectors 2020-03-24.csv')


# # Chosen sensor List ####
chosen_sensors_dt <- sensor_config %>%
  # filter(corridor_route %in% c('I-94'))%>% # to select only I-94
  # filter(r_node_n_type %in% c('Station'))%>% want entrance/exit ramps now too
  filter(!detector_category == "CD")
# filter(!detector_name %in% already_have_data$Sensor)

chosen_sensors_dt <- data.table(chosen_sensors_dt)

chosen_sensors <- chosen_sensors_dt$detector_name



# READ DATA (takes about 1.5 minutes)----
config <-
  fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date <- NULL

sensor_files <- list.files('data/data_hourly_raw')
sensor_names <- gsub('.csv', '', sensor_files)
sensor_names <- gsub('Sensor ', '', sensor_names)

# Node-Sensor Lookup Table ----
node_lut <- config[, .(r_node_name, detector_name)]
node_lut <- node_lut[, sensor_name := as.character(detector_name)]
node_lut <- node_lut[detector_name %in% sensor_names, ]
node_lut <- split(node_lut, node_lut$r_node_name)


cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)




# node_lut <- node_lut[100:120] # test

# CLEAN UP TIME -----

foreach(i = node_lut) %dopar% {
  
  library(data.table)
  library(lubridate)
  library(dplyr)
  
  # select dates:
  # date_range <- c(Sys.Date()-1) # yesterday's data
  date_range <-
    c(seq(as.Date("2020-10-15"), Sys.Date() - 1, by = "days"))
  # date_range <- c(seq(as.Date("2020-08-04"), as.Date("2020-08-09"), by = "days"))
  
  # node_info <- i
  
   
  
  # for(i in 1:length(node_lut)){
  # node_info <- node_lut[[i]] # when a for loop instead of in parallel - testing
  node_info <- node_lut[[1010]] # test
  this_node <- node_info$r_node_name[[1]]
  these_sensors <- paste0('Sensor ', node_info$sensor_name)
  these_sensor_files <-
    paste0(paste0('data/data_hourly_raw/', these_sensors, '.csv'))
  
  old_data <- rbindlist(lapply(these_sensor_files, fread))
  
  # DELETE DUPLICATE OBSERVATIONS ----
  # Find duplicate observations -- multiple pulls are written into the same data file
  # example  -- hourlydat_sensor[date == '2020-03-19' & sensor == 100]
  # get only the second observation:
  old_data <-
    old_data[!duplicated(old_data,
                                 by = c('date', 'hour', 'sensor'),
                                 fromLast = TRUE)]
  
  
  # DELETE OBSERVATIONS IN DATE RANGE ----
  old_data <- old_data[!date %in% date_range, ]
  
  
  # GET NEW DATA ----
  for (j in node_info$sensor_name) {
    n <- length(date_range)
    new_data_ls <- vector("list", n)
    
    for (i in 1:n) {
      new_data_ls[[i]] <-
        tc.sensors::pull_sensor(j, date_range[[i]], fill_gaps = T) %>%
        tc.sensors::aggregate_sensor(
          interval_length = 1,
          replace_impossible = T,
          interpolate_missing = TRUE,
          config = sensor_config
        ) %>%
        mutate(hour = interval_bin) %>%
        mutate(year = year(date)) %>%
        select(date, sensor, hour, volume.sum, occupancy.sum, year)
      
    }
    
    new_data <- data.table::rbindlist(new_data_ls)
  }
  
  new_data[,sensor:=as.character(sensor)]
  old_data[,sensor:=as.character(sensor)]
  hourlydat_sensor <- merge(old_data, new_data, all= T)
  
  # write over data: 
  # data.table::fwrite(hourlydat_sensor, paste0("data/data_hourly_raw/Sensor ", j, ".csv"), append = F)
  
  
  # GAPFILLING 1: FILL IN MISSING MINUTES WITHIN HOURS ----
  # sometimes a few observations were missing within an hour (volume.nulls).
  # each hour had 120 30-second observation windows.
  # calculate a new volume with the average volume for that hour, as long as there are at least 10 minutes of data (less than 100 nulls)
  # sum, mean, median, nad number of nulls already calculated when the data are pulled.
  hourlydat_sensor[, volume.sum.raw := volume.sum] # copy the column for safekeeping
  hourlydat_sensor[, volume.sum := ifelse(
    volume.nulls <= 100,
    round(volume.sum.raw + volume.nulls * volume.mean),
    volume.sum.raw
  )]
  
  hourlydat_sensor[, occupancy.sum.raw := occupancy.sum] # copy the column for safekeeping
  hourlydat_sensor[, occupancy.sum := ifelse(
    occupancy.nulls <= 100,
    round(occupancy.sum.raw + occupancy.nulls * occupancy.mean),
    occupancy.sum.raw
  )]
  
  
  # if more than 100 nulls (>50 minutes missing), call it NA for that hour:
  hourlydat_sensor[, volume.sum := ifelse(volume.nulls > 100, NA, volume.sum)]
  hourlydat_sensor[, occupancy.sum := ifelse(occupancy.nulls > 100, NA, occupancy.sum)]
  
  
  # DELETE OCCUPANCY COLUMNS - DON'T NEED ----
  hourlydat_sensor[, c(
    'volume.nulls',
    'volume.mean',
    'volume.median',
    'occupancy.nulls',
    'occupancy.mean',
    'occupancy.median',
    'occupancy.sum.raw',
    'volume.sum.raw'
  ) := NULL]
  
  # GAPFILLING 2: WHOLE HOURS MISSING ----
  # if a whole hour is missing, fill in with average of the two hours on either side of it
  setorder(hourlydat_sensor, sensor, year, date, hour)
  hourlydat_sensor[, `:=`(volume.sum.rollmean = shift(
    frollapply(
      volume.sum,
      3,
      mean,
      align = 'center',
      na.rm = T,
      hasNA = T
    )
  )),
  # by = .(sensor, year)] # when all data is downloaded, and no breaks in the time series, can get rid of by-year argument
  by = .(sensor)] # all data downloaded!
  hourlydat_sensor[, volume.sum := ifelse(is.na(volume.sum), volume.sum.rollmean, volume.sum)]
  
  hourlydat_sensor[, `:=`(occupancy.sum.rollmean = shift(
    frollapply(
      occupancy.sum,
      3,
      mean,
      align = 'center',
      na.rm = T,
      hasNA = T
    )
  )),
  # by = .(sensor, year)] # when all data is downloaded, and no breaks in the time series, can get rid of by-year argument
  by = .(sensor)] # all data downloaded!
  hourlydat_sensor[, occupancy.sum := ifelse(is.na(occupancy.sum),
                                             occupancy.sum.rollmean,
                                             occupancy.sum)]
  
  # hist(hourlydat_sensor[is.na(volume.sum), hour]) # for some reason a lot of NAs at 0:00 (12 AM). wonder why...?
  # A full ten percent of 12 AM observations missing?? ####
  # nrow(hourlydat_sensor[is.na(volume.sum) & hour == 0])/nrow(hourlydat_sensor[hour ==0])
  
  # Best to delete the midnight observations for now, but we should check on this with mndot
  hourlydat_sensor <- hourlydat_sensor[hour > 0]
  hourlydat_sensor[, c('volume.sum.rollmean', 'occupancy.sum.rollmean') :=
                     NULL]
  
  # WRITE HOURLY DATA ! ----
  
  data.table::fwrite(hourlydat_sensor,
                     paste0("data/data_hourly_raw/Sensor ", j, ".csv"),
                     append = T)
  
  
  
}



# Sensor reads as numeric - switch to character
hourlydat_sensor[, sensor := as.character(sensor)]







# CHECK FOR NODES MISSING ALL DATA
total_zilch <- sum(!is.na(hourlydat_sensor$hour))
if (total_zilch == 0) {
  
} else{
  # MERGE BACK TO CONFIGURATION FILE ---
  hourlydat_sensor <-
    merge(
      hourlydat_sensor,
      config,
      all.x = T,
      all.y = F,
      by.x = 'sensor',
      by.y = 'detector_name'
    )
  
  # CALCULATE SPEED ---
  hourlydat_sensor[, occupancy.pct := (occupancy.sum / 216000)]
  hourlydat_sensor[, speed := ifelse(volume.sum != 0,
                                     (volume.sum * detector_field) / (5280 *
                                                                        occupancy.pct),
                                     0)]
  
  # AGGREGATE TO NODES ----
  # count number of lanes with data for each hour, at node ----
  hourlydat_sensor[, num_lanes_with_data_this_hr := sum(!is.na(volume.sum)),
                   by = list(
                     r_node_name,
                     r_node_station_id,
                     r_node_n_type,
                     r_node_transition,
                     r_node_label,
                     r_node_lanes,
                     r_node_lon,
                     r_node_lat,
                     corridor_route,
                     corridor_dir,
                     year,
                     date,
                     hour
                   )]
  
  
  # sometimes more lanes than the node is reported to have from mndot (r_node_lanes). sometimes less. Confusing!
  # count number of unique sensors this year for this node:
  hourlydat_sensor[, num_sensors_this_year := uniqueN(sensor),
                   by = .(r_node_name, year)]
  
  
  # exclude to only those hourly observations at each node where the number of lanes
  # equals the number of detectors for this year.
  hourlydat_sensor <-
    hourlydat_sensor[num_lanes_with_data_this_hr == num_sensors_this_year]
  
  hourlydat_node <- hourlydat_sensor[, as.list(unlist(lapply(.SD,
                                                             function(x)
                                                               list(sum = sum(x),
                                                                    mean = mean(x))))),
                                     by = .(r_node_name,
                                            year, date, hour),
                                     .SDcols = c('volume.sum', 'occupancy.sum', 'speed')]
  hourlydat_node[, c('volume.sum.mean', 'occupancy.sum.mean', 'speed.sum') :=
                   NULL]
  setnames(hourlydat_node, old = 'volume.sum.sum', new = 'volume.sum')
  setnames(hourlydat_node, old = 'occupancy.sum.sum', new = 'occupancy.sum')
  setnames(hourlydat_node, old = 'speed.mean', new = 'speed')
  
  write.csv(hourlydat_node,
            paste0('data/data_hourly_node/', this_node, '.csv'))
  
  # sum for all hours of the day (daily-scale data) ----
  dailydat <- hourlydat_node[, as.list(unlist(lapply(.SD,
                                                     function(x)
                                                       list(sum = sum(x),
                                                            mean = mean(x))))),
                             by = .(r_node_name,
                                    year, date),
                             .SDcols = c('volume.sum', 'occupancy.sum', 'speed')]
  dailydat[, c('volume.sum.mean', 'occupancy.sum.mean', 'speed.sum') :=
             NULL]
  setnames(dailydat, old = 'volume.sum.sum', new = 'volume.sum')
  setnames(dailydat, old = 'occupancy.sum.sum', new = 'occupancy.sum')
  setnames(dailydat, old = 'speed.mean', new = 'speed')
  
  # WRITE DAILY DATA! ----
  write.csv(dailydat,
            paste0('data/data_daily_node/', this_node, '.csv'))
  
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
config <-
  fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date <- NULL
det_config <- unique(config[, .(
  r_node_name,
  r_node_n_type,
  r_node_transition,
  r_node_label,
  r_node_lon,
  r_node_lat,
  r_node_lanes,
  r_node_shift,
  r_node_s_limit,
  r_node_station_id,
  r_node_attach_side,
  corridor_route,
  corridor_dir
)])

node_files <- list.files('data/data_daily_node')
node_names <- gsub('.csv', '', node_files)
# gam_list <- vector("list", length(node_names))
# pred_ls <- vector("list", length(node_names))

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


# node_files <- node_files[1:10] # test


foreach(i  = node_files) %dopar% {
  library(data.table)
  library(lubridate)
  library(mgcv)
  
  # i <- node_files[[3244]] # test
  # i <- "rnd_1805.csv"
  dailydat <- fread(paste0("data/data_daily_node/", i))
  dailydat <- unique(dailydat)
  if (nrow(dailydat) < 2 |
      sum(dailydat$volume.sum, na.rm = T) < 100) {
  } else{
    # Dealing with date ----
    # dailydat[, date := as.IDate(fast_strptime(date, "%Y-%m-%d"))]
    dailydat[, dow := wday(date)]
    dailydat[, doy := yday(date)]
    dailydat[, year := year(date)]
    dailydat[, woy := week(date)]
    dailydat[, weekday := factor(weekdays(date))]
    dailydat[, monthday := format(date, "%b %d")]
    
    
    # get rid of 2017 data: (december 15-31 included in this pull) ----
    dailydat <- dailydat[year > 2017,]
    
    # get rid of NA values:
    dailydat <- dailydat[!is.na(dailydat$volume.sum)]
    
    # must have 3 years of data, at least 60 days of data in each year ----
    dailydat[, "num_days_per_year" := uniqueN(date), by = .(r_node_name, year)]
    dailydat <- dailydat[num_days_per_year > 60]
    
    # subset to relevant dates:
    modeling_dat <- dailydat[dailydat$date < "2020-03-01", ]
    
    # 2020 data v. sensitive - exclude some special holidays and weather days
    modeling_dat <-
      modeling_dat[!modeling_dat$date == "2020-01-01",] # holiday - exclude
    modeling_dat <-
      modeling_dat[!modeling_dat$date == "2020-01-17",] # cold snap - exclude
    modeling_dat <-
      modeling_dat[!modeling_dat$date == "2020-01-18",] # cold snap - exclude
    modeling_dat <-
      modeling_dat[!modeling_dat$date == "2020-02-09",] # snow day - exclude
    
    
    has_2020_data <-
      unique(modeling_dat$r_node_name[modeling_dat$year == 2020 &
                                        dailydat$date < "2020-03-01"])
    has_2018_data <-
      unique(modeling_dat$r_node_name[modeling_dat$year == 2018])
    has_2019_data <-
      unique(modeling_dat$r_node_name[modeling_dat$year == 2019])
    
    modeling_dat <-
      modeling_dat[modeling_dat$r_node_name %in% has_2020_data
                   &
                     modeling_dat$r_node_name %in% has_2019_data
                   &
                     modeling_dat$r_node_name %in% has_2018_data,]
    
    if (nrow(modeling_dat) == 0) {
    } else{
      this_gam <- with(modeling_dat,
                       mgcv::gam(volume.sum ~
                                   s(
                                     dow, k = 7, by = as.factor(year)
                                   ) # one knot for each day of the week+s(doy) # general seasonal trend, let it vary by year, allow knots to be set by gam
                                 # + as.factor(year), # intercept for each year, ))
                                 
                                 # gam_list[[i]] <- this_gam
                                 
                                 
                                 # generate predictions
                                 date_range <-
                                   c(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "days"))
                                 
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
                                 
                                 predict_dat[, c("volume.predict", "volume.predict.se") :=
                                               cbind(
                                                 predict.gam(
                                                   object = this_gam,
                                                   newdata = predict_dat,
                                                   se.fit = T,
                                                   type = 'response'
                                                 )
                                               )]
                                 
                                 predict_dat <- merge(
                                   predict_dat,
                                   dailydat[, .(r_node_name, date, volume.sum)],
                                   all.x = T,
                                   by = c('r_node_name', 'date')
                                 )
                                 
                                 # difference from predicted, n volume:
                                 predict_dat[, volume.diff.raw := (volume.sum - volume.predict)]
                                 
                                 # difference from predicted, in %:
                                 predict_dat[, volume.diff := round(((volume.sum - volume.predict) / volume.predict) * 100, 1)]
                                 
                                 fwrite(predict_dat,
                                        file = paste0('output/daily_model_predictions_bynode/', i))
                                 
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


# rbindlist(lapply(node_files, function(i)
#   tryCatch(fread(paste0('output/daily_model_predictions_bynode/', i)),
#                   error = function(e) {
#                     cat("\nError reading in file:",i,"\t") #Identifies problem files by name
#                     message(e) #Prints error message without stopping loop
#                     list(ERROR=i) #Adds a placeholder column so rbindlist will execute
#                     })),
#   fill = T)

multmerge = function(mypath) {
  filenames = list.files(path = mypath, full.names = TRUE)
  rbindlist(lapply(filenames, function(i)
    tryCatch(
      fread(i, colClasses = list(character = c(25))),
      error = function(e) {
        cat("\nError reading in file:", i, "\t") #Identifies problem files by name
        message(e) #Prints error message without stopping loop
        list(ERROR = i) #Adds a placeholder column so rbindlist will execute
      }
    )), #End of tryCatch and lapply
    fill = TRUE) #rbindlist arguments
} #End of function

diffs_dt <- multmerge('output/daily_model_predictions_bynode/')

diffs_dt$volume.diff <- as.numeric(diffs_dt$volume.diff)
unique(diffs_dt$r_node_name[diffs_dt$volume.diff > 10000000])
summary(diffs_dt)

stopCluster(cl)

# some very high numbers?
# hist(diffs_dt[,volume.diff])
# summary(diffs_dt[,volume.diff])
unique(diffs_dt[volume.diff < (-100), .(r_node_name, r_node_n_type)])
# r_node_name r_node_n_type
# 1:    rnd_5932          Exitsummary(diffs_dt[volume.diff > 1000])
# 2:    rnd_5948          Exit
# 3:    rnd_5950      Entrance
# 4:   rnd_85131          Exit

unique(diffs_dt[volume.predict < 0, .(r_node_name, r_node_n_type)])

# negative predicted values -- one spurious node
# diffs_dt <- diffs_dt[!r_node_name == 'rnd_86223'] -- now many more
diffs_dt <-
  diffs_dt[!r_node_name %in% unique(diffs_dt[volume.predict < 0, r_node_name])]

# get rid of NAs
diffs_dt <- diffs_dt[!is.na(volume.diff)]

# other spurious observations (8 of them)
# diffs_dt<-diffs_dt[volume.diff < (100)]
# another problematic node:
# diffs_dt <- diffs_dt[!r_node_name %in% c('rnd_86469', 'rnd_95784')]

# very high values
summary(diffs_dt[volume.diff > 100 &
                   year == 2020, .(volume.predict, volume.diff)])
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
diffs_dt <-
  diffs_dt[!r_node_name %in% unique(diffs_dt[volume.diff > 500 &
                                               year == 2020, r_node_name])]
summary(diffs_dt)


fwrite(diffs_dt, paste0("output/pred-and-act-vol-by-node.csv"))
fwrite(diffs_dt,
       paste0(
         "covid.traffic.trends/data-raw/pred-and-act-vol-by-node.csv"
       ))


# More data reshaping of model output ----
daynodes <-
  diffs_dt[r_node_n_type == "Station" &
             year == 2020 # this year's data, stations only. ####
           , lapply(.SD, mean),
           .SDcols = c("volume.predict"),
           by = .(date, dow, doy, year, woy, weekday, monthday)]


# Total difference from expected for whole metro area ----
diffs_4plot <-
  diffs_dt[r_node_n_type == "Station" &
             year == 2020 # this year's data, stations only. ####
           , lapply(
             .SD,
             FUN = function(x)
               sum(x, na.rm = T)
           ),
           .SDcols = c("volume.sum", "volume.predict"),
           by = .(date, dow, doy, year, woy, weekday, monthday)]

# vmt is 1/2 of volume ----
diffs_4plot[, c("vmt.sum", "vmt.predict") := list(volume.sum * 0.5, volume.predict * 0.5)]
diffs_4plot[, "Difference from Typical VMT (%)" := round(100 * (vmt.sum - vmt.predict) / vmt.predict, 2)]
diffs_4plot[, difference_text := ifelse(
  `Difference from Typical VMT (%)` < 0,
  paste0(abs(round(
    `Difference from Typical VMT (%)`, 1
  )), " % less than typical"),
  paste0(abs(round(
    `Difference from Typical VMT (%)`, 1
  )), " % more than typical")
)]


fwrite(diffs_4plot, paste0("output/pred-and-act-vol-region.csv"))
fwrite(
  diffs_4plot,
  paste0(
    "covid.traffic.trends/data-raw/pred-and-act-vol-region.csv"
  )
)
