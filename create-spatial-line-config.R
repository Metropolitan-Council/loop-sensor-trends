
detector_r_node_corrs_tidy <- read.csv('data/Configuration of Metro Detectors 2020-03-22.csv')
detector_r_node_corrs_tidy <- unique(detector_r_node_corrs_tidy[,c('r_node_name', 'r_node_n_type', 'r_node_lat', 'r_node_lon', 'corridor_route', 'corridor_dir')])
detector_r_node_corrs_tidy_ii <- detector_r_node_corrs_tidy %>%
  mutate(r_node_lat = as.character(r_node_lat),
         r_node_lon = as.character(r_node_lon)) %>%
  mutate(r_node_lat = as.numeric(r_node_lat),
         r_node_lon = as.numeric(r_node_lon)) %>%
  group_by(corridor_route, corridor_dir) %>%
  mutate(corridor_index = row_number())


# Select stations (want distance between stations, not between ramps etc.)
config_stations <- detector_r_node_corrs_tidy_ii %>%
  filter(r_node_n_type == "Station")

# Conflate lanes so they have the same upstream detector; create index
config_conflated <- detector_r_node_corrs_tidy_ii %>%
  filter(r_node_n_type == "Station") %>%
  select(corridor_route, corridor_dir, r_node_lat, r_node_lon) %>%
  unique() %>%
  group_by(corridor_route, corridor_dir) %>%
  mutate(corridor_conflated_index = row_number())

# Join index to full dataset
config_pre_lag <- left_join(config_stations, config_conflated, by = c("corridor_route", "corridor_dir", "r_node_lat", "r_node_lon"))

# Create lagged index in order to connect lat/longs of upstream detector to detector of interest
config_lag <- config_conflated %>%
  ungroup() %>%
  mutate(corridor_conflated_index = corridor_conflated_index - 1) %>%
  rename(r_node_lat_upstream = r_node_lat,
         r_node_lon_upstream = r_node_lon)

# Join upstream detectors to full detector dataset
config_full <- left_join(config_pre_lag, config_lag, by = c("corridor_route", "corridor_conflated_index", "corridor_dir"))

# Create SpatialLinesDataFrame configuration using inlabru

library(inlabru)

# Conversion to SpatialLinesDataFrame without CRS
config_sline <- config_full %>%
  filter(!is.na(r_node_lat_upstream) & !is.na(r_node_lon_upstream)) # Remove nodes with no upstream detector (no line can be created)

spl <- sline(config_sline, start.cols = c("r_node_lon", "r_node_lat"),
             end.cols = c("r_node_lon_upstream", "r_node_lat_upstream"))

# Add CRS projection
proj4string(spl) <- CRS("+init=epsg:4267")

rgdal::writeOGR(spl, "Configuration Data", "Sline test", "ESRI Shapefile")

# # test
# slinesf<-st_read('Configuration Data/Sline test.shp')
# plot(slinesf)

