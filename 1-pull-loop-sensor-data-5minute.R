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


# Truncate temporary table here

ROracle::dbDisconnect(tbidb)


