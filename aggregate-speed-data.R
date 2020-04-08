##############
library(data.table)
library(foreach)
library(doParallel)
library(lubridate)
library(ggplot2)
library(cowplot)
#############

# READ DATA (takes about 1.5 minutes)----
node_files <- list.files('data/data_hourly_node')
node_names <- gsub('.csv', '', node_files)

config <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date<-NULL
config <- config[r_node_n_type == 'Station']
config <- config[r_node_name %in% node_names,]

# Node-Corridor Lookup Table ----
node_lut <- config[,r_node_name:=as.character(r_node_name)]
node_lut <- split(node_lut, node_lut$corridor_route)

# match("I-94", names(node_lut))
# unique(node_lut[[15]]$r_node_name)

n <- length(node_lut)
hourlydat_corr <- vector("list", n)

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

# node_lut <- node_lut[100:120] # test

# CLEAN UP TIME -----

cordat <-rbindlist(
  foreach(i = node_lut) %dopar% {
    
    library(data.table)
    library(lubridate)
    
    node_info <- i
    # node_info <- node_lut[[15]] # test
    this_corridor <- node_info$corridor_route[[1]]
    this_corridor_index <- match(this_corridor, names(node_lut))
    
    these_nodes <- node_info$r_node_name
    these_node_files <- paste0(paste0('data/data_hourly_node/', these_nodes, '.csv'))
    
    hourlydat_node <- rbindlist(lapply(these_node_files, fread))
    
    if( nrow(hourlydat_node) == 0) {
      hourlydat_node <-NULL
    } else{ 
      hourlydat_node[,date:=as.IDate(fast_strptime(date, "%Y-%m-%d"))] 
      hourlydat_node <- hourlydat_node[date > '2020-03-01']
      hourlydat_node[,year:=year(date)]
      # hourlydat_node[,speed:=ifelse(speed>300, NA, speed)]
      hourlydat_node[,speed:=ifelse(speed == Inf, NA, speed)]
      hourlydat_node[,speed:=ifelse(speed > 100, NA, speed)]

      hourlydat_node <- hourlydat_node[,lapply(.SD, FUN = function(x) mean(x, na.rm = T)),
                                       .SDcols = 'speed', 
                                       by = .(date, hour, corridor_route)]
    }
    hourlydat_corr[[this_corridor_index]] <- hourlydat_node
  }
)


stopCluster(cl)

cordat <- data.table(cordat)

ggplot(cordat[hour %in% c(17)
              & corridor_route %in% c('I-94', 'I-494', 'I-694', 'T.H.280', 'T.H.36')
              ], 
       aes(x = date, y = speed, color = corridor_route))+
  geom_point()+
  geom_smooth(method = 'loess', se = F)+
  facet_wrap(~hour)+
  theme_minimal()+
  theme_cowplot()+
  scale_y_continuous(limits = c(30, 80), breaks = seq(from = 0, to = 80, by = 10))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d")


fwrite(cordat, 'output/speed-by-corridor.csv')
