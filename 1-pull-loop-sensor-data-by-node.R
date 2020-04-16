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


sensor_config <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')


# # Chosen sensor List ####
chosen_sensors_dt <- sensor_config%>%
  # filter(corridor_route %in% c('I-94'))%>% # to select only I-94
  # filter(r_node_n_type %in% c('Station'))%>% want entrance/exit ramps now too
  filter(!detector_category == "CD")
  # filter(!detector_name %in% already_have_data$Sensor)

chosen_sensors_dt <- data.table(chosen_sensors_dt)

chosen_nodes <- unique(chosen_sensors_dt$r_node_name)


cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

# tictoc::tic()
foreach(this_node = chosen_nodes) %dopar% {
  
  # Find list of sensors for that node
  this_node <- "rnd_792"
  chosen_sensors <- chosen_sensors_dt$detector_name[chosen_sensors_dt$r_node_name == this_node]
  date_range <- c(Sys.Date()-1) # yesterday's data
  
  n <- length(date_range)
  loops_ls <- vector("list", n)
  
  for(a_sensor in 1:s){
    for (this_date in 1:n) {
    loops_ls[[this_date]] <- tc.sensors::pull_sensor(this_node, date_range[[this_date]])
    }
  loops_df <- data.table::rbindlist(loops_ls)
  }
  
  
  # This part me - gapfill:
  library(data.table)
  loops_df[,date:=as.IDate(date)]
  setorder(loops_df, date)
  loops_df[,year:=year(date)]
  # gapfill - hours -- this_date don't think this works? 
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
  
  data.table::fwrite(loops_df, paste0("data/data_hourly_raw/Sensor ", this_node, ".csv"), append = T)
  
  
}

stopCluster(cl)
# tictoc::toc()
