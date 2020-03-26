## code to prepare `nodes` dataset goes here

pkgload::load_all()
library(covid.traffic.trends)
library(dplyr)
library(sf)

# get configuration data
sensor_dt <- data.table::fread("./data-raw/Configuration of Metro Detectors 2020-03-24.csv")
node_dt <- unique(sensor_dt[, .(r_node_name, r_node_n_type,
                                r_node_lon, r_node_lat, 
                                r_node_station_id, corridor_route, 
                                corridor_dir)])

# conver to sf object
nodes <- st_as_sf(node_dt, coords = c("r_node_lon", "r_node_lat"), 
                    crs = 4326)

usethis::use_data(nodes, overwrite = TRUE, compress = "xz")
