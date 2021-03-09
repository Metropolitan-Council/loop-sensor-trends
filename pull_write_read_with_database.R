tictoc::tic()

#Opening the toolbox-------------------------------

# library(devtools)
# install_github("Metropolitan-Council/tc.sensors", ref = "ashley")
library(tc.sensors)
# Data manipulation packages:
library(data.table)
library(tidyverse)
# Database packages:
library(DBI)
library(rstudioapi) # this package allows us to type in a password when connecting to the database.
library(ROracle)

library(ggplot2)
load('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/councilcolors.Rdata')
ampo_theme <- source('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/ampo_theme.R')
##########################

# Pull Configuration ---------------------------------------------
raw_sensor_config <- pull_configuration()
sensor_config <- pull_configuration() %>%
  # match names to database names:
  rename_all(toupper) %>%
  rename_all(~ gsub("R_NODE", "NODE", .))

#Connecting to the database -------------------------------
connect.string = '(DESCRIPTION=(ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = fth-exa-scan.mc.local  )(PORT = 1521)))(CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME =  com4te.mc.local)))'
tbidb = ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = connect.string,
  username = 'mts_planning_data',
  # mts_planning_view for viewing data only, no write privileges. 
  # mts_planning_data is the username for write privileges.
  password = rstudioapi::askForPassword("database password")
)


#Configure database time zone -------------------------------
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")

# Get Nodes Without Data -------------------------------------
# Either fill in from sensors with no data at all -- 
need_data1 <- ROracle::dbReadTable(tbidb, 'RTMC_SENSORS_WITHOUT_DATA') # 284,117
need_data1 <- data.table(need_data1)
need_data1 <- need_data1[,.(PREDICT_DATE, DETECTOR_NAME, NODE_NAME)]
need_data1[,PREDICT_DATE:=as.Date(PREDICT_DATE)]
# some missing detector names -- no longer in config file? 
uniqueN(need_data1$NODE_NAME[is.na(need_data1$DETECTOR_NAME)]) # just 1, rnd 46
need_data1 <- need_data1[!is.na(DETECTOR_NAME),]
# And/or fill in the gaps from sensors that have some data for a date, but it's incomplete -- 
need_data2 <- ROracle::dbReadTable(tbidb, 'RTMC_SENSORS_MISSING_HOURS') # 36,889
need_data2 <- data.table(need_data2)
need_data2[,PREDICT_DATE:=as.Date(PREDICT_DATE)]
need_data2 <- need_data2[,.(PREDICT_DATE, DETECTOR_NAME, NODE_NAME)]

need_data <- merge(need_data1, need_data2, all = T)

need_data_raw <- need_data  # making a copy
# need_data <- need_data_raw???


# anything missing from yesterday, and the past two weeks:
need_data <- need_data[need_data$PREDICT_DATE >= Sys.Date()-21 & need_data$PREDICT_DATE < Sys.Date(),]


# for a month (overnight data downloads): 
# need_data <- need_data[need_data$PREDICT_DATE >= '2020-02-01' & 
#                          need_data$PREDICT_DATE < '2020-03-01']
# 31707/(60 * 60) = 8.8 hours/month of data, = 4.4 days per year of data!

pb <-
  txtProgressBar(
    min = 0,
    max = nrow(need_data),
    style = 3
  )
k <- 0


for (s in 1:nrow(need_data)) {
    k <- k + 1
    setTxtProgressBar(pb, k)
    sensor_day_dat <-
      pull_sensor(
        sensor = need_data$DETECTOR_NAME[[s]],
        pull_date = need_data$PREDICT_DATE[[s]],
        fill_gaps = T,
        .quiet = T
      ) %>%
      scrub_sensor() %>%
      aggregate_sensor(
        config = raw_sensor_config,
        interval_length = (15 / 60),
        # pull every 15 minutes
        replace_impossible = T,
        interpolate_missing =  T,
        occupancy_pct_threshold = 0.002
      ) %>%
      filter(volume.pct.null < 100 & occupancy.pct.null < 100)
    
    if (nrow(sensor_day_dat) == 0) {
    } else{
      sensor_day_dat %>%
        select(
          sensor,
          volume.pct.null,
          volume.sum,
          occupancy.pct.null,
          occupancy.sum,
          start_datetime,
          volume.mean,
          occupancy.mean,
          occupancy.pct,
          speed
        ) %>%
        filter(!is.na(start_datetime)) %>%
        filter(!start_datetime == "") %>%
        rename(
          detector_name = sensor,
          volume_sum = volume.sum,
          volume_mean = volume.mean,
          volume_pctnull = volume.pct.null,
          occupancy_sum = occupancy.sum,
          occupancy_mean = occupancy.mean,
          occupancy_pct = occupancy.pct,
          occupancy_pctnull = occupancy.pct.null
        ) %>%
        mutate(
          # detector_name = as.integer(detector_name),
          start_datetime = as.POSIXct(start_datetime),
          volume_pctnull = as.integer(round(volume_pctnull)),
          volume_sum = as.integer(round(volume_sum)),
          occupancy_pctnull = as.integer(round(occupancy_pctnull)),
          occupancy_sum = as.integer(round(occupancy_sum)),
          
          volume_mean = round(volume_mean, digits = 1),
          occupancy_mean = round(occupancy_mean, digits = 1),
          occupancy_pct = round(occupancy_pct, digits = 3),
          speed = round(speed, digits = 1)
        ) %>%
        rename_all(toupper) %>%
        mutate_all( ~ case_when(!is.nan(.x) ~ .x)) %>%
        ROracle::dbWriteTable(
          conn = tbidb,
          name = "RTMC_5MIN_TEMP",
          row.names = FALSE,
          append = TRUE
        )
    }
    
  }



# # Delete data in permanent table where more complete data are now available -------------------
ROracle::dbSendQuery(
  tbidb,
  paste0(
    "delete from rtmc_5min where rowid in (",
    " select rtmc_5min.rowid from rtmc_5min",
    " inner join rtmc_5min_temp on",
    " (rtmc_5min_temp.start_datetime = rtmc_5min.start_datetime",
    " and rtmc_5min_temp.detector_name = rtmc_5min.detector_name)",
    " where rtmc_5min_temp.volume_sum <> rtmc_5min.volume_sum",
    " and rtmc_5min.volume_pctnull> rtmc_5min_temp.volume_pctnull)"
  )
)

ROracle::dbSendQuery(tbidb, "commit")


# # Insert new data from temporary -> permanent table ---------------------------------------------
ROracle::dbSendQuery(tbidb,
                     paste0(
                       "insert into rtmc_5min",
                       " select * from rtmc_5min_temp",
                       " where",
                       " not exists (",
                       " select * from rtmc_5min",
                       " where  rtmc_5min_temp.start_datetime = rtmc_5min.start_datetime",
                       " and rtmc_5min_temp.detector_name = rtmc_5min.detector_name",
                       ") and",
                       " rowid in (",
                       "select max(rowid)",
                       " from   rtmc_5min_temp",
                       " group  by detector_name, start_datetime)"
                     )
)

ROracle::dbSendQuery(tbidb, "commit")

# Truncate Temporary Table Here ---------------------------------------------
# # ??????????
ROracle::dbSendQuery(tbidb,
                     "delete from rtmc_5min_temp where extract(year from start_datetime) = 2020"
)


# Pull Daily Node Data ---------------------------------------------
node_diffs <- ROracle::dbReadTable(tbidb, "RTMC_DAILY_NODE_DIFF")
node_diffs <- data.table(node_diffs)

setnames(node_diffs, old = c('NODE_NAME', 'DATA_DATE', 'TOTAL_VOLUME', 'VOLUME_PREDICT', 'VOLUME_DIFF'),
         new = c('r_node_name', 'date', 'total_volume', 'predicted_volume', 'volume_difference'))
sort(unique(node_diffs$date))
node_config <- raw_sensor_config %>%
  select(-detector_name, -detector_label, -detector_category, -detector_lane, -detector_field, -detector_abandoned, 
         -r_node_lanes, -r_node_shift, -r_node_attach_side, -date)%>%
  unique()
node_diffs <- merge(node_diffs, node_config, all.x = T, all.y = F)
node_diffs[,volume_difference:=(volume_difference/predicted_volume) * 100]


# write to flat files: 
fwrite(node_diffs, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/pred-and-act-vol-by-node.csv"))
fwrite(node_diffs, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/data-raw/pred-and-act-vol-by-node.csv"))



# Pull Daily System Data ---------------------------------------------
system_diffs <- ROracle::dbReadTable(tbidb, "RTMC_DAILY_SYSTEM_DIFF")
system_diffs <- data.table(system_diffs)
setnames(system_diffs, old = c('DATA_DATE', 'TOTAL_VOLUME', 'TOTAL_PREDICTED_VOLUME', 'TOTAL_VOLUME_DIFF'),
         new = c('date', 'volume.sum', 'volume.predict', 'volume.diff'))


system_diffs[, c("vmt.sum", "vmt.predict") := list(volume.sum * 0.5, volume.predict * 0.5)]
system_diffs[, "Difference from Typical VMT (%)" := round(100 * (vmt.sum - vmt.predict) / vmt.predict, 2)]
system_diffs[, difference_text := ifelse(`Difference from Typical VMT (%)` < 0, paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % less than typical"),
                                        paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % more than typical")
)]

# write to flat files: 
fwrite(system_diffs, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/pred-and-act-vol-region.csv"))
fwrite(system_diffs, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/data-raw/pred-and-act-vol-region.csv"))

system_diffs[,date:=as.Date(date)]

# Plot System Data (Check) ---------------------------------------------
ggplot(system_diffs, aes(x = date))+
  geom_point(aes(y = volume.predict, color = "Predicted VMT"))+
  geom_line(aes(y = volume.predict, color = "Predicted VMT"))+
  geom_point(aes(y = volume.sum, color = "Actual VMT"))+
  geom_line(aes(y= volume.sum, color = "Actual VMT"))+
  scale_x_date(date_breaks = "months")+
  scale_color_manual(values = c(mtsRed, 'black')) + 
  theme_minimal()

# Download/Re-Shape MnDOT Data ---------------------------------------------
mndot_files <- list.files('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/data/mndot-data/')
# max = most recent mndot file
mndotdat <- fread(paste0('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/data/mndot-data/', 
                         max(mndot_files)), 
                  header = T)
mndotdat <- mndotdat[District %in% c("MnDOT Statewide")]
mndotdat <- melt(mndotdat, id.vars = c("District"), variable.name = "date", value.name = "Difference from Typical VMT (%)")
mndotdat[, date := as.IDate(date, format = "%Y-%m-%d")]

fwrite(mndotdat, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/diff-vol-state.csv"), row.names = F)
fwrite(mndotdat, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/data-raw/diff-vol-state.csv"), row.names = F)

mndotdat[,date:=as.IDate(date)]

# 7-Day Rolling Average Calculations  ---------------------------------------------
holidays <- c(as.Date('2020-07-03'), as.Date('2020-07-04'), as.Date('2020-09-07'), 
              as.Date('2020-11-26'))

system_diffs[,diffvol_use:=ifelse(date %in% holidays, NA, `Difference from Typical VMT (%)`)]
system_diffs[,rollingavg:=shift(frollapply(diffvol_use, 7, mean, align = 'right', na.rm = T))]

mndotdat[,diffvol_use:=ifelse(date %in% holidays, NA, `Difference from Typical VMT (%)`)]
mndotdat[,rollingavg:=shift(frollapply(diffvol_use, 7, mean, align = 'right', na.rm = T))]

# Static Daily Plot  ---------------------------------------------


static_plot <-
  ggplot(system_diffs, 
         aes(x = date, y = (`Difference from Typical VMT (%)`), color = 'MnDOT Metro Freeways\n(1000+ Stations)\n'))+
  
  # shaded rectangle for stay-at-home order:
  annotate("rect", xmin = as.Date('2020-03-28'), xmax = as.Date('2020-05-18'), ymin = -Inf, ymax = Inf, 
           alpha = .15)+
  
   # horizontal line at zero:
  geom_hline(yintercept = 0)+
  
  # lines and points for MnDOT: 
  geom_point(data = mndotdat[mndotdat$date > '2020-03-06',], size = 1)+
  geom_point(data = mndotdat[mndotdat$date > '2020-03-06',], aes(color = 'MnDOT Statewide\n(105 Stations)\n'), size = 1)+
  # geom_line(data = mndotdat[mndotdat$date > '2020-03-06',], aes(color = 'MnDOT Statewide\n(105 Stations)\n'), size = 0.5, linetype = 'dotted', show.legend = F)+
  geom_line(data = mndotdat[mndotdat$date > '2020-03-06',], aes(y = rollingavg, color = 'MnDOT Statewide\n(105 Stations)\n'), size = 1, show.legend = F)+
  
  # lines and points for Metro:
  geom_point(size = 1)+
  # geom_line(size = 0.5, linetype = 'dotted', show.legend = F)+
  geom_line(aes(y = rollingavg), size = 1, show.legend = F)+
  # global options: 
  theme(legend.position = 'right')+
  theme(panel.grid.major.x = element_line(color = 'gray90'),
        panel.grid.major.y = element_line(color = 'gray90'))+
  # ggtitle(paste0("Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
  # axes: 
  labs(x = "Date", y = "% difference from typical traffic")+
  scale_x_date(breaks = seq(as.Date('2020-03-08'), Sys.Date()+3,by="2 weeks"),
               date_labels = '%b\n%d',
               limits = c(as.Date('2020-03-06'), Sys.Date()+3))+
  scale_y_continuous(limits = c(-70, 15), breaks = seq(from = -70, to = 10, by = 10))+
  #  colors:
  scale_color_manual(values = c(councilBlue, 'black'), name = "Traffic Sensor Group")+
  theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.justification = 'center') + 
  theme_minimal() +
  theme(legend.position = 'right') +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )



ggsave('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/traffic-trends-actions.png',static_plot, width = 10, height = 4, bg = "transparent")
ggsave('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/inst/app/www/traffic-trends-actions.png',static_plot, height = 7, width = 10, units = 'in', dpi = 300)


ROracle::dbDisconnect(tbidb)
tictoc::toc()
