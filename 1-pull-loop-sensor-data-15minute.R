library(doParallel)
library(foreach)
library(tc.sensors)
library(data.table)
library(tidyverse)
library(dplyr)

# Chosen sensor List ####
chosen_sensors_dt <- read.csv('data/Configuration of Metro Detectors 2020-03-24.csv') %>%
  filter(!detector_category == "CD") %>%
  top_n(-10, detector_name)

chosen_sensors <- chosen_sensors_dt$detector_name

date_range <- c(Sys.Date()-1, Sys.Date()-2) # yesterday's data
# date_range <- c(seq(as.Date("2020-05-18"), as.Date("2020-05-21"), by = "days"))

num_dates <- length(date_range)
loops_ls <- vector("list", num_dates)

# construct an empty data frame of hours, days and minutes
empty_dates <- expand.grid(date = date_range, 
                           hour = 0:23, 
                           min = seq(from = 0, to = 59.5, by = 0.5))
empty_dates <- data.table(empty_dates)


cores <- detectCores()
cl <- makeCluster(cores-1)
registerDoParallel(cl)

# tictoc::tic()
foreach(j = chosen_sensors) %dopar% {
  library(data.table)
  
  for (i in 1:num_dates) {
    loops_ls[[i]] <- tc.sensors::pull_sensor(j, date_range[[i]])
    }
  
    
  loops_df <- data.table::rbindlist(loops_ls)
  loops_df <- merge.data.table(empty_dates, loops_df, all.x = T)
  
  loops_df$sensor <- j
  
  # Fifteen minute bins ----
  # create bins
  bins <- c(0, 15, 30, 45, 60)
  loops_df[,fifteen_min_bin:=findInterval(loops_df$min, bins)]
  loops_df[,start_min:=min(min), by = .(date, hour, fifteen_min_bin)]
  
  interval_length <- 15 # minutes of interval
  
  # aggregate to every fifteen minutes:
  loops_df <- loops_df[, as.list(unlist(lapply(.SD, function(x) list(
    pct_nulls = round(100 * sum(is.na(x))/interval_length * 2),
    sum = round(interval_length * 2 * mean(x, na.rm = T)))))),
    by=.(date, hour, start_min, sensor), 
    .SDcols=c("volume", "occupancy")]
  
  loops_df[,start_datetime:=as.POSIXct(paste(date, hour, start_min), 
                                       format = "%Y-%m-%d %H %M")]
  
  loops_df[,start_datetime:=as.character(start_datetime)]
  
  loops_df[,hour:=NULL]
  loops_df[,date:=NULL]
  loops_df[,start_min:=NULL]
  
  data.table::fwrite(loops_df, paste0("data/data_15minute_raw/Sensor ", j, ".csv"), append = T)
} 

stopCluster(cl)

# tictoc::toc()
