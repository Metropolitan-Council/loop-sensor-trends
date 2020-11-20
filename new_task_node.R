library(doParallel)
library(foreach)
# library(devtools)
# devtools::install_github("Metropolitan-Council/tc.sensors", ref = 'main')
library(tc.sensors)
library(data.table)
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


config <-
  fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date <- NULL

sensor_files <- list.files('data/data_hourly_raw')
sensor_names <- gsub('.csv', '', sensor_files)
sensor_names <- gsub('Sensor ', '', sensor_names)

# Node-Sensor Lookup Table ----
node_lut <- config[, .(r_node_name, detector_name)]
node_lut <- node_lut[, sensor_name := as.character(detector_name)]
node_lut <- node_lut[detector_name %in% sensor_names,]

# Only nodes with existing predictions: 
predict_files <- list.files('output/daily_model_predictions_bynode/')
predict_node_names <- gsub('.csv', '', predict_files)
node_lut <- node_lut[r_node_name %in% predict_node_names,]


node_lut <- split(node_lut, node_lut$r_node_name)
# node_lut <- node_lut[100:120] # test

cores <- detectCores()
cl <- makeCluster(cores-1)
registerDoParallel(cl)



foreach(
  node_num = node_lut,
  .packages = c("data.table", "lubridate", "dplyr", "tc.sensors")
) %dopar% {
  
  # select dates:
  # date_range <- c(Sys.Date()-1) # yesterday's data
  date_range <-
    c(seq(as.Date("2020-10-15"), Sys.Date() - 1, by = "days"))
  # date_range <- c(seq(as.Date("2020-08-04"), as.Date("2020-08-09"), by = "days"))
  
  # node_info <- node_num
  # for(i in 1:length(node_lut)){
  # node_info <- node_lut[[i]] # when a for loop instead of in parallel - testing
  node_info <- node_lut[[1020]] # test - one node
  
  this_node <- node_info$r_node_name[[1]]
  
  
  # GET NEW DATA ----
  n_sensors <- length(node_info$sensor_name)
  new_node_data_ls <- vector("list", n_sensors)
  
  for (a_sensor in node_info$sensor_name) {
    n_dates <- length(date_range)
    new_data_ls <- vector("list", n_dates)
    
    for (a_date in 1:n_dates) {
      
      ####
      # test
      # a_date <- 1
      # a_sensor <- node_info$sensor_name[[1]]
      ####
      
      new_data_ls[[a_date]] <-
        tc.sensors::pull_sensor(a_sensor, date_range[[a_date]], fill_gaps = T) %>%
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
    
    old_data <- fread(paste0("data/data_hourly_raw/Sensor ", a_sensor, ".csv"), fill = T)
    old_data[,date:=as.IDate(date)]
    old_data[,sensor:=as.character(sensor)]
    
    hourlydat_sensor <- rbind(new_data, old_data, use.names = T)
    hourlydat_sensor <- unique(hourlydat_sensor)
    hourlydat_sensor <- hourlydat_sensor[!is.na(volume.sum)]
    hourlydat_sensor <- hourlydat_sensor[!duplicated(hourlydat_sensor, by = c('date', 'hour', 'sensor'), fromLast=TRUE)]
    
    
    fwrite(hourlydat_sensor,
           paste0("data/data_hourly_raw/Sensor ", a_sensor, ".csv"),
           append = F)
    
    new_node_data_ls[[a_sensor]] <- hourlydat_sensor
  }
  
  hourlydat_sensors <- rbindlist(new_node_data_ls)
  
  # CHECK FOR NODES MISSING ALL DATA
  total_zilch <- sum(!is.na(hourlydat_sensors$hour))
  if (total_zilch == 0) {
    
  } else{
    # MERGE BACK TO CONFIGURATION FILE ---
    hourlydat_sensors <-
      merge(
        hourlydat_sensors,
        config,
        all.x = T,
        all.y = F,
        by.x = 'sensor',
        by.y = 'detector_name'
      )
    
    # CALCULATE SPEED ---
    hourlydat_sensors[, occupancy.pct := (occupancy.sum / 216000)]
    hourlydat_sensors[, speed := ifelse(volume.sum != 0,
                                       (volume.sum * detector_field) / (5280 *
                                                                          occupancy.pct),
                                       0)]
    
    # AGGREGATE TO NODES ----
    # count number of lanes with data for each hour, at node ----
    hourlydat_sensors[, num_lanes_with_data_this_hr := sum(!is.na(volume.sum)),
                     by = list(r_node_name,
                               year,
                               date,
                               hour)]
    
    
    # sometimes more lanes than the node is reported to have from mndot (r_node_lanes). sometimes less. Confusing!
    # count number of unique sensors this year for this node:
    hourlydat_sensors[, num_sensors_this_year := uniqueN(sensor),
                     by = .(r_node_name, year)]
    
    
    # exclude to only those hourly observations at each node where the number of lanes
    # equals the number of detectors for this year.
    hourlydat_sensors <-
      hourlydat_sensors[num_lanes_with_data_this_hr == num_sensors_this_year]
    
    hourlydat_node <- hourlydat_sensors[, as.list(unlist(lapply(.SD,
                                                               function(x)
                                                                 list(sum = sum(x),
                                                                      mean = mean(x))))),
                                       by = .(r_node_name, year, date, hour),
                                       .SDcols = c('volume.sum', 'occupancy.sum', 'speed')]
    # remove some columns:
    hourlydat_node[, c('volume.sum.mean', 'occupancy.sum.mean', 'speed.sum') :=
                     NULL]
    setnames(hourlydat_node, old = 'volume.sum.sum', new = 'volume.sum')
    setnames(hourlydat_node, old = 'occupancy.sum.sum', new = 'occupancy.sum')
    setnames(hourlydat_node, old = 'speed.mean', new = 'speed')
    
    fwrite(
      hourlydat_node,
      paste0('data/data_hourly_node/', this_node, '.csv'),
      append = F,
      row.names = T)
    
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
    fwrite(
      dailydat,
      paste0('data/data_daily_node/', this_node, '.csv'),
      append = T,
      row.names = T
    )
    
    
    # MERGE TO PREDICTIONS
    predict_dat <-
      fread(paste0(
        'output/daily_model_predictions_bynode/',
        this_node,
        ".csv"
      ))
    predict_dat <-
      setDT(predict_dat)[dailydat, `:=`(
        volume.sum = ifelse(is.na(volume.sum) | date %in% date_range, i.volume.sum, volume.sum),
        occupancy.sum = ifelse(is.na(occupancy.sum) | date %in% date_range, i.occupancy.sum, occupancy.sum)
      ),
      on = c("r_node_name", "date")][]
    
    # difference from predicted, n volume:
    predict_dat[, volume.diff.raw := (volume.sum - volume.predict)]
    
    # difference from predicted, in %:
    predict_dat[, volume.diff := round(((volume.sum - volume.predict) / volume.predict) * 100, 1)]
    
    fwrite(
      predict_dat,
      paste0(
        'output/daily_model_predictions_bynode/',
        this_node,
        ".csv"
      )
    )
    
  }# ends check for missing data
}

  
stopCluster(cl)

node_files <- list.files('output/daily_model_predictions_bynode')
node_names <- gsub('.csv', '', node_files)



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

# get rid of these as well
diffs_dt <-
  diffs_dt[!r_node_name %in% unique(diffs_dt[volume.diff > 500 &
                                               year == 2020, r_node_name])]
summary(diffs_dt)


# fwrite(diffs_dt, paste0("output/pred-and-act-vol-by-node.csv"))

fwrite(
  diffs_dt,
  paste0(
    "covid.traffic.trends/data-raw/pred-and-act-vol-by-node.csv"
  )
)


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
  paste0(abs(
    round(`Difference from Typical VMT (%)`, 1)
  ), " % less than typical"),
  paste0(abs(
    round(`Difference from Typical VMT (%)`, 1)
  ), " % more than typical")
)]


fwrite(diffs_4plot, paste0("output/pred-and-act-vol-region.csv"))
fwrite(
  diffs_4plot,
  paste0(
    "covid.traffic.trends/data-raw/pred-and-act-vol-region.csv"
  )
)
