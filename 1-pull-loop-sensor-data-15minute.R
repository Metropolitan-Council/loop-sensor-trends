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

cores <- detectCores()
cl <- makeCluster(cores-1)
registerDoParallel(cl)

# tictoc::tic()
foreach(j = chosen_sensors) %dopar% {
  library(data.table)
  date_range <- c(Sys.Date()-1) # yesterday's data
  # date_range <- c(seq(as.Date("2020-05-18"), as.Date("2020-05-21"), by = "days"))
  #                 seq(as.Date("2019-07-01"), as.Date("2019-12-15"), by = "days"))
  
  num_dates <- length(date_range)
  loops_ls <- vector("list", num_dates)
  
  for (i in 1:num_dates) {
    loops_ls[[i]] <- tc.sensors::pull_sensor(j, date_range[[i]])
    empty_dates <- bind_cols(i, as_tibble(rep(0:23, each = 120)) %>% 
                               rename(hour = value), as_tibble(rep(seq(from = 0, 
                                                                       to = 59.5, by = 0.5), 24)) %>% rename(min = value))
    
  }

    
  loops_df <- data.table::rbindlist(loops_ls)
  loops_df[,start_datetime:=as.POSIXct(paste(date, hour, min), 
                                   format = "%Y-%m-%d %H %M")]

  bins <- c(0, 15, 30, 45, 60)
  loops_df[,fifteen_min_bin:=findInterval(loops_df$min, bins)]
  
  loops_df[,start_datetime:=min(start_datetime), by = .(date, hour, fifteen_min_bin)]
  
  interval_length <- 15 # minutes of interval
  
  # aggregate to every fifteen minutes:
  loops_df <- loops_df[, as.list(unlist(lapply(.SD, function(x) list(
    pct_nulls = round(100 * sum(is.na(x))/interval_length * 2),
    sum = round(interval_length * 2 * mean(x, na.rm = T)))))),
    by=.(start_datetime, sensor), 
    .SDcols=c("volume", "occupancy")]
  
  loops_df[,start_datetime:=as.character(start_datetime)]
  data.table::fwrite(loops_df, paste0("data/data_15minute_raw/Sensor ", j, ".csv"), append = T)
} 

stopCluster(cl)

# tictoc::toc()
