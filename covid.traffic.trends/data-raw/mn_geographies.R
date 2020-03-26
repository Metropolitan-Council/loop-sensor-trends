# code for developing Minnesota geographies here

pkgload::load_all()
library(covid.traffic.trends)
library(dplyr)
library(sf)
library(tigris)


mn_counties <- tigris::counties(state = "MN",
                                class = "sf") %>% 
  dplyr::select(NAME) %>% 
  sf::st_transform(4326) 

usethis::use_data(mn_counties, overwrite = TRUE, compress = "xz")


mn_cities <- tigris::places(state = "MN",
                            class = "sf") %>% 
  dplyr::select(NAME, NAMELSAD) %>% 
  sf::st_transform(4326)

usethis::use_data(mn_cities, overwrite = TRUE, compress = "xz")