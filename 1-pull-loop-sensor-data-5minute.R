#Opening the toolbox-------------------------------
library(doParallel)
library(foreach)
library(tc.sensors)
library(data.table)
library(tidyverse)
library(dplyr)
library(DBI)
library(rstudioapi) # this package allows us to type in a password when connecting to the database.
library(ROracle)


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


# Pull Configuration ---------------------------------------------
raw_sensor_config <- pull_configuration()
sensor_config <- pull_configuration() %>%
  # match names to database names:
  rename_all(toupper) %>%
  rename_all( ~ gsub("R_NODE", "NODE", .))

# Update Configuration ---------------------------------------------
# Write configuration to database
ROracle::dbWriteTable(
  tbidb,
  value = sensor_config,
  name = "RTMC_CONFIGURATION",
  # replace the configuration file: 
  overwrite = TRUE,
  row.names = FALSE
)

# Insert New Corridors:
ROracle::dbSendQuery(
  tbidb,
  paste0(
    "INSERT INTO RTMC_CORRIDOR (CORRIDOR_ROUTE)",
    " SELECT DISTINCT(CORRIDOR_ROUTE) FROM RTMC_CONFIGURATION",
    " LEFT JOIN RTMC_CORRIDOR USING(CORRIDOR_ROUTE)",
    " WHERE CORRIDOR_ROUTE IS NULL"
  )
)

# Insert New Nodes:
ROracle::dbSendQuery(
  tbidb,
  paste0(
    "INSERT INTO RTMC_NODE(NODE_NAME, NODE_TYPE, NODE_TRANSITION, NODE_LABEL,",
    " NODE_LON, NODE_LAT, NODE_LANES, NODE_SHIFT,",
    " NODE_SPEEDLIMIT, NODE_STATION_ID, NODE_ATTACHSIDE,",
    " CORRIDOR_ROUTE, CORRIDOR_DIR)",
    " SELECT DISTINCT ",
    " RTMC_CONFIGURATION.NODE_NAME, RTMC_CONFIGURATION.NODE_N_TYPE,",
    " RTMC_CONFIGURATION.NODE_TRANSITION, RTMC_CONFIGURATION.NODE_LABEL,",
    " RTMC_CONFIGURATION.NODE_LON, RTMC_CONFIGURATION.NODE_LAT,",
    " RTMC_CONFIGURATION.NODE_LANES, RTMC_CONFIGURATION.NODE_SHIFT,",
    " RTMC_CONFIGURATION.NODE_S_LIMIT, RTMC_CONFIGURATION.NODE_STATION_ID,",
    " RTMC_CONFIGURATION.NODE_ATTACH_SIDE,",
    " RTMC_CONFIGURATION.CORRIDOR_ROUTE, RTMC_CONFIGURATION.CORRIDOR_DIR",
    " FROM RTMC_CONFIGURATION",
    " LEFT JOIN RTMC_NODE",
    " ON(RTMC_CONFIGURATION.NODE_NAME = RTMC_NODE.NODE_NAME)",
    " WHERE RTMC_NODE.NODE_NAME IS NULL"
  )
)

# Update Node Info:
ROracle::dbSendQuery(
  tbidb,
  paste0(
    "UPDATE RTMC_NODE",
    " SET (NODE_TYPE, NODE_TRANSITION, NODE_LABEL,",
    " NODE_LON, NODE_LAT, NODE_LANES, NODE_SHIFT,",
    " NODE_SPEEDLIMIT, NODE_STATION_ID, NODE_ATTACHSIDE,",
    " CORRIDOR_ROUTE, CORRIDOR_DIR)=(SELECT",
    " DISTINCT RTMC_CONFIGURATION.NODE_N_TYPE,",
    " RTMC_CONFIGURATION.NODE_TRANSITION,",
    " RTMC_CONFIGURATION.NODE_LABEL,",
    " RTMC_CONFIGURATION.NODE_LON,",
    " RTMC_CONFIGURATION.NODE_LAT,",
    " RTMC_CONFIGURATION.NODE_LANES,",
    " RTMC_CONFIGURATION.NODE_SHIFT,",
    " RTMC_CONFIGURATION.NODE_S_LIMIT,",
    " RTMC_CONFIGURATION.NODE_STATION_ID,",
    " RTMC_CONFIGURATION.NODE_ATTACH_SIDE,",
    " RTMC_CONFIGURATION.CORRIDOR_ROUTE,",
    " RTMC_CONFIGURATION.CORRIDOR_DIR",
    " FROM RTMC_CONFIGURATION",
    " WHERE RTMC_NODE.NODE_NAME = RTMC_CONFIGURATION.NODE_NAME)",
    " WHERE EXISTS (SELECT 1 FROM RTMC_CONFIGURATION ",
    " WHERE RTMC_NODE.NODE_NAME = RTMC_CONFIGURATION.NODE_NAME)"
  )
)

# Insert New Detectors:
ROracle::dbSendQuery(
  tbidb,
  paste0(
    "INSERT INTO RTMC_DETECTOR(DETECTOR_NAME, DETECTOR_LABEL, DETECTOR_CATEGORY, DETECTOR_LANE, DETECTOR_ABANDONED, NODE_NAME)",
    " SELECT DISTINCT ",
    " RTMC_CONFIGURATION.DETECTOR_NAME, RTMC_CONFIGURATION.DETECTOR_LABEL,",
    " RTMC_CONFIGURATION.DETECTOR_CATEGORY, RTMC_CONFIGURATION.DETECTOR_LANE,",
    " RTMC_CONFIGURATION.DETECTOR_ABANDONED, RTMC_CONFIGURATION.NODE_NAME",
    " FROM RTMC_CONFIGURATION",
    " LEFT JOIN RTMC_DETECTOR",
    " ON(RTMC_CONFIGURATION.DETECTOR_NAME = RTMC_DETECTOR.DETECTOR_NAME)",
    " WHERE RTMC_DETECTOR.DETECTOR_NAME IS NULL"
  )
)


#Update Detectors:
ROracle::dbSendQuery(
  tbidb,
  paste0(
    "UPDATE RTMC_DETECTOR",
    " SET (DETECTOR_NAME, DETECTOR_LABEL, DETECTOR_CATEGORY, DETECTOR_LANE, DETECTOR_ABANDONED, NODE_NAME)=(SELECT",
    " DISTINCT RTMC_CONFIGURATION.DETECTOR_NAME,",
    " RTMC_CONFIGURATION.DETECTOR_LABEL,",
    " RTMC_CONFIGURATION.DETECTOR_CATEGORY,",
    " RTMC_CONFIGURATION.DETECTOR_LANE,",
    " RTMC_CONFIGURATION.DETECTOR_ABANDONED,",
    " RTMC_CONFIGURATION.NODE_NAME",
    " FROM RTMC_CONFIGURATION",
    " WHERE RTMC_DETECTOR.DETECTOR_NAME = RTMC_CONFIGURATION.DETECTOR_NAME)",
    " WHERE EXISTS (SELECT 1 FROM RTMC_CONFIGURATION ",
    " WHERE RTMC_DETECTOR.DETECTOR_NAME = RTMC_CONFIGURATION.DETECTOR_NAME)"
  )
)


chosen_sensors_dt <- raw_sensor_config %>% 
  filter(detector_abandoned == 'f') %>% 
  filter(!detector_category == 'CD') %>%
  # filter(corridor_route == "I-494")%>%
  filter(r_node_n_type == 'Station')

chosen_sensors <- chosen_sensors_dt$detector_name

# date_range <- c(Sys.Date() - 1) # yesterday's data
date_range <- c(seq(as.Date("2020-09-01"), as.Date("2020-09-15"), by = "days"))
num_dates <- length(date_range)

# 0.43 seconds per sensor per day
# 42 minutes for one day for all 5966 non-CD station sensors

# what is slow? reading or writing?
# 0.39 seconds to read data

# for loop on the outside, upload a day of all the sensor data at a

tictoc::tic()
for (s in 1:length(chosen_sensors)) {
  for (d in 1:num_dates) {
    print(chosen_sensors[[s]])
    pull_sensor(
      sensor = chosen_sensors[[s]],
      pull_date = date_range[d],
      fill_gaps = T,
      .quiet = F
    ) %>% 
      scrub_sensor() %>% 
      aggregate_sensor(
        config = raw_sensor_config,
        interval_length = (15 / 60),
        replace_impossible = T,
        interpolate_missing =  T,
        occupancy_pct_threshold = 0.002
      ) %>%
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
      mutate_all(~ case_when(!is.nan(.x) ~ .x)) %>%
      ROracle::dbWriteTable(
        conn = tbidb,
        name = "RTMC_5MIN",
        row.names = FALSE,
        append = TRUE
      )
    flush.console()
  }
}

tictoc::toc()

ROracle::dbDisconnect(tbidb)
