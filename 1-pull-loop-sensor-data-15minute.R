library(doParallel)
library(foreach)
library(tc.sensors)
library(data.table)
library(tidyverse)
library(dplyr)

#Opening the toolbox-------------------------------
library(DBI)
library(rstudioapi) # this package allows us to type in a password when connecting to the database.
library(ROracle) # Moment of truth...does it load?


#Connecting to the database: the other 25% of the battle -------------------------------
connect.string = '(DESCRIPTION=(ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = fth-exa-scan.mc.local  )(PORT = 1521)))(CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME =  com4te.mc.local)))'
tbidb = ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = connect.string,
  username = 'mts_planning_data',
  # mts_planning_view for viewing data only, no read/write priviliges. mts_planning_data is the username for read/write privlieges.
  password = rstudioapi::askForPassword("database password")
)


Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")


# Start test ---------------------------------------------
# Chosen sensor List ####
chosen_sensors_dt <- read.csv('data/Configuration of Metro Detectors 2020-03-24.csv') %>%
  filter(!detector_category == "CD")
config <- pull_configuration()
chosen_sensors <- chosen_sensors_dt$detector_name

date_range <- c(Sys.Date()-1) # yesterday's data
# date_range <- c(seq(as.Date("2020-05-18"), as.Date("2020-05-21"), by = "days"))

num_dates <- length(date_range)
loops_ls <- vector("list", num_dates)

# construct an empty data frame of hours, days and minutes
empty_dates <- expand.grid(date = date_range, 
                           hour = 0:23, 
                           min = seq(from = 0, to = 59.5, by = 0.5))
empty_dates <- data.table(empty_dates)



tictoc::tic()

for(s in 1:length(chosen_sensors)){
  
  for (d in 1:num_dates) {
  raw_sensor_dat <- tc.sensors::pull_sensor(chosen_sensors[[s]], date_range[[d]])
  agg_sensor_dat <- aggregate_sensor(raw_sensor_dat, interval_length = 0.25, config = chosen_sensors_dt)
  loops_ls[[d]] <- scrub_sensor(raw_sensor_dat, interval_length = 0.25)
    }
    
  loops_df <- data.table::rbindlist(loops_ls)
  loops_df <- merge.data.table(empty_dates, loops_df, all.x = T)
  
  loops_df$sensor <- chosen_sensors[[s]]
  
  # Fifteen minute bins ----
  # create bins
  bins <- c(0, 15, 30, 45, 60)
  loops_df[,fifteen_min_bin:=findInterval(loops_df$min, bins)]
  loops_df[,start_min:=min(min), by = .(date, hour, fifteen_min_bin)]
  
  interval_length_min <- 15 # minutes of interval
  n_measures <- interval_length_min * 2 # number of measures taken in interval - every 30 seconds
  
  # aggregate to every fifteen minutes:
  loops_df <- loops_df[, as.list(unlist(lapply(.SD, function(x) list(
    pct_nulls = round(100 * sum(is.na(x))/n_measures),
    sum = round(n_measures * mean(x, na.rm = T)))))),
    by=.(date, hour, start_min, sensor), 
    .SDcols=c("volume", "occupancy")]
  
  loops_df[,start_datetime:=as.POSIXct(paste(date, hour, start_min), 
                                       format = "%Y-%m-%d %H %M")]
  
  for (i in seq_along(loops_df)) suppressMessages(set(loops_df, i=which(is.nan(loops_df[[i]])), j=i, value='NULL'))
  
  # loops_df[,start_datetime:=as.character(start_datetime)]
  
  loops_df[,hour:=NULL]
  loops_df[,date:=NULL]
  loops_df[,start_min:=NULL]
  
  names(loops_df) <- c('DETECTOR_NAME', 'VOLUME_PCTNULL', 'VOLUME_SUM', 'OCCUPANCY_PCTNULL', 'OCCUPANCY_SUM', 'START_DATETIME')
  loops_df$DETECTOR_NAME <- as.integer(loops_df$DETECTOR_NAME)
  ROracle::dbWriteTable(tbidb, "RTMC_15MIN", loops_df, row.names = FALSE, append = TRUE)
  
} 

tictoc::toc()
