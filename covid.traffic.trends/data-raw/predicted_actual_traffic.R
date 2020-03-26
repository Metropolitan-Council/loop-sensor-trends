## code to prepare `predicted_actual` dataset series goes here

library(covid.traffic.trends)
library(sf)
library(data.table)

predicted_actual_by_node <- fread("./data-raw/pred-and-act-vol-by-node-2020-03-26.csv") # our golden ticket!
predicted_actual_by_node[,date:=as.IDate(date)]
predicted_actual_by_node <- predicted_actual_by_node[date>'2020-03-01',]
predicted_actual_by_node[,scl_volume:=scale(volume.predict, center= F)]


usethis::use_data(predicted_actual_by_node, overwrite = TRUE, compress = "xz")

## predicted and actual summarized to the region (mostly sensors in the metro and Fargo/Moorehead) from March 1
predicted_actual_by_region <- fread("./data-raw/pred-and-act-vol-region-2020-03-26.csv") 

usethis::use_data(predicted_actual_by_region, overwrite = TRUE, compress = "xz")