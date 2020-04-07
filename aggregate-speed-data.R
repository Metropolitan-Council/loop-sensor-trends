##############
library(data.table)
library(foreach)
library(doParallel)
library(lubridate)
library(ggplot2)
library(cowplot)
#############

# READ DATA (takes about 1.5 minutes)----
config <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date<-NULL
config <- config[r_node_n_type == 'Station']

node_files <- list.files('data/data_hourly_node')
node_names <- gsub('.csv', '', node_files)


# Node-Corridor Lookup Table ----
node_lut <- config[,r_node_name:=as.character(r_node_name)]
node_lut <- node_lut[r_node_name %in% node_names,]
node_lut <- split(node_lut, node_lut$corridor_route)

hourlydat_corr <- vector("list", length(node_lut))

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

# node_lut <- node_lut[100:120] # test

# CLEAN UP TIME -----

foreach(i = node_lut) %dopar% {
  
  library(data.table)
  library(lubridate)
  
  
  node_info <- i
  node_info <- node_lut[[2]] # test
  this_corridor <- node_info$corridor_route[[1]]
  
  these_nodes <- node_info$r_node_name
  these_node_files <- paste0(paste0('data/data_hourly_node/', these_nodes, '.csv'))
  
  hourlydat_node <- rbindlist(lapply(these_node_files, fread))

  hourlydat_node[,date:=as.IDate(fast_strptime(date, "%Y-%m-%d"))] 
  hourlydat_node[,year:=year(date)]
  hourlydat_node[,speed:=ifelse(speed>300, NA, speed)]
  hourlydat_node <- hourlydat_node[date > '2020-03-01']
  
  hourlydat_corr[[i]] <- hourlydat_node[,lapply(.SD, mean),
                                   .SDcols = 'speed', 
                                   by = .(date, hour, corridor_route)]

}


stopCluster(cl)

cordat <- rbindlist(hourlydat_corr)

ggplot(cordat[hour %in% c(8, 17, 22)], aes(x = date, y = speed, color = corridor_route))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, span = (1/7))+
  facet_wrap(~hour)+
  theme_minimal()+
  theme_cowplot()


