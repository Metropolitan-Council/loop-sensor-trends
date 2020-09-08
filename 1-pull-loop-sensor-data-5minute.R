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
  # mts_planning_view for viewing data only, no read/write priviliges. mts_planning_data is the username for read/write privlieges.
  password = rstudioapi::askForPassword("database password")
)



#Configure database time zone -------------------------------
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")


# Start test ---------------------------------------------
# Chosen sensor List ####

sensor_config <- pull_configuration()

 %>%
  ROracle::dbWriteTable(
    conn = tbidb,
    name = "RTMC_DET_CATEGORY",
    row.names = FALSE,
    overwrite = TRUE
  )


# RETRIEVE table2 DATA
db_category <- dbGetQuery(tbidb, "SELECT * FROM RTMC_DET_CATEGORY")

# APPEND BOTH DATAFRAMES
stackeddf <- sensor_config %>%
  select(detector_category) %>%
  filter(!detector_category == "") %>%
  rename_all(toupper) %>%
  rbind(db_category) %>%
  unique() %>%
  as.data.frame(as.matrix())

# OVERWRITE DESTINATION TABLE EACH TIME
chosen_sensors <- sensor_config$detector_name

date_range <- c(Sys.Date() - 1) # yesterday's data
# date_range <- c(seq(as.Date("2020-05-18"), as.Date("2020-05-21"), by = "days"))
num_dates <- length(date_range)


tictoc::tic()

for (s in 1:length(chosen_sensors)) {
  for (d in 1:num_dates) {
    
    tc.sensors::pull_sensor(
      sensor = chosen_sensors[s],
      pull_date = as.character(date_range[[d]]),
      fill_gaps = T,
      .quiet = T
    ) %>%
      scrub_sensor() %>%
      aggregate_sensor(
        config = sensor_config,
        interval_length = (5 / 60),
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
        detector_name = as.integer(detector_name),
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
        name = "RTMC_5MIN",
        row.names = FALSE,
        append = TRUE
      )
    print(sensor) 
    flush.console()
  }
}

tictoc::toc()

ROracle::dbDisconnect(tbidb)