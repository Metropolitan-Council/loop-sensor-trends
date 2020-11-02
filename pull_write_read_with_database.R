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
  # mts_planning_view for viewing data only, no write priviliges. mts_planning_data is the username for write privlieges.
  password = rstudioapi::askForPassword("database password")
)


#Configure database time zone -------------------------------
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")

# Get Nodes Without Data -------------------------------------
need_data <- ROracle::dbReadTable(tbidb, 'RTMC_SENSORS_WITHOUT_DATA')
need_data$new_date <- substr(need_data$PREDICT_DATE, start = 1, stop = 11)
#239,702
need_data <- need_data[need_data$new_date == Sys.Date()-1,]
# 6658 rows

pb <-
  txtProgressBar(
    min = 0,
    max = nrow(need_data),
    style = 3
  )
k <- 0

tictoc::tic()
for (s in 1:nrow(need_data)) {
    k <- k + 1
    setTxtProgressBar(pb, k)
    sensor_day_dat <-
      pull_sensor(
        sensor = need_data$DETECTOR_NAME[[s]],
        pull_date = need_data$new_date[[s]],
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

tictoc::toc()

# Insert new data from temporary -> permanent table ---------------------------------------------
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
ROracle::dbSendQuery(tbidb,
                     "commit"
)


# Truncate Temporary Table Here ---------------------------------------------
# ??????????

# Pull Daily Node Data ---------------------------------------------
diffs_dt <- ROracle::dbReadTable(tbidb, "RTMC_DAILY_NODE_DIFF")
diffs_dt <- data.table(diffs_dt)
setnames(diffs_dt, old = c('NODE_NAME', 'DATA_DATE', 'TOTAL_VOLUME', 'VOLUME_PREDICT', 'VOLUME_DIFF'),
         new = c('r_node_name', 'date', 'total_volume', 'predicted_volume', 'volume_difference'))
node_config <- raw_sensor_config %>%
  select(-detector_name, -detector_label, -detector_category, -detector_lane, -detector_field, -detector_abandoned, 
         -r_node_lanes, -r_node_shift, -r_node_attach_side, -date)%>%
  unique()


fwrite(diffs_dt, paste0("output/pred-and-act-vol-by-node.csv"))
fwrite(diffs_dt, paste0("covid.traffic.trends/data-raw/pred-and-act-vol-by-node.csv"))



# Pull Daily System Data ---------------------------------------------
diffs_4plot <- ROracle::dbReadTable(tbidb, "RTMC_DAILY_SYSTEM_DIFF")
diffs_4plot <- data.table(diffs_4plot)
setnames(diffs_4plot, old = c('DATA_DATE', 'TOTAL_VOLUME', 'TOTAL_PREDICTED_VOLUME', 'TOTAL_VOLUME_DIFF'),
         new = c('date', 'volume.sum', 'volume.predict', 'volume.diff'))


diffs_4plot[, c("vmt.sum", "vmt.predict") := list(volume.sum * 0.5, volume.predict * 0.5)]
diffs_4plot[, "Difference from Typical VMT (%)" := round(100 * (vmt.sum - vmt.predict) / vmt.predict, 2)]
diffs_4plot[, difference_text := ifelse(`Difference from Typical VMT (%)` < 0, paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % less than typical"),
                                        paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % more than typical")
)]


fwrite(diffs_4plot, paste0("output/pred-and-act-vol-region.csv"))
fwrite(diffs_4plot, paste0("covid.traffic.trends/data-raw/pred-and-act-vol-region.csv"))

diffs_4plot[,date:=as.Date(date)]

# Plot System Data (Check) ---------------------------------------------
ggplot(diffs_4plot, aes(x = date))+
  geom_point(aes(y = volume.predict, color = "Predicted VMT"))+
  geom_line(aes(y = volume.predict, color = "Predicted VMT"))+
  geom_point(aes(y = volume.sum, color = "Actual VMT"))+
  geom_line(aes(y= volume.sum, color = "Actual VMT"))+
  scale_x_date(date_breaks = "months")+
  scale_color_manual(values = c(mtsRed, 'black')) + 
  theme_minimal()

# Download/Re-Shape MnDOT Data ---------------------------------------------
# yesterday <- as.Date('2020-10-25')
yesterday <- Sys.Date() - 1# change back to -1 when new data available

mndotdat <- fread(paste0('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/data/mndot-data/Daily_Volume_Change_', yesterday, '_update.csv'), 
                  header = T)
mndotdat <- mndotdat[District %in% c("MnDOT Statewide")]
mndotdat <- melt(mndotdat, id.vars = c("District"), variable.name = "date", value.name = "Difference from Typical VMT (%)")
mndotdat[, date := as.IDate(date, format = "%Y-%m-%d")]

fwrite(mndotdat, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/diff-vol-state.csv"), row.names = F)
fwrite(mndotdat, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/data-raw/diff-vol-state.csv"), row.names = F)

mndotdat[,date:=as.IDate(date)]

# 7-Day Rolling Average Calculations  ---------------------------------------------
holidays <- c(as.Date('2020-07-03'), as.Date('2020-07-04'), as.Date('2020-09-07'))

diffs_4plot[,diffvol_use:=ifelse(date %in% holidays, NA, `Difference from Typical VMT (%)`)]
diffs_4plot[,rollingavg:=shift(frollapply(diffvol_use, 7, mean, align = 'left', na.rm = T))]

mndotdat[,diffvol_use:=ifelse(date %in% holidays, NA, `Difference from Typical VMT (%)`)]
mndotdat[,rollingavg:=shift(frollapply(diffvol_use, 7, mean, align = 'left', na.rm = T))]

# Static Daily Plot  ---------------------------------------------
static_plot <-
  ggplot(diffs_4plot, 
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
               date_labels = '%m/%d',
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



static_plot
ggsave('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/traffic-trends-actions.png',static_plot, width = 10, height = 4, bg = "transparent")
ggsave('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/inst/app/www/traffic-trends-actions.png',static_plot, height = 7, width = 10, units = 'in', dpi = 300)