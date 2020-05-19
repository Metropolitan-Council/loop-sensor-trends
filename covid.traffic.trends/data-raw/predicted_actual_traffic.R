## code to prepare `predicted_actual` dataset series goes here

library(covid.traffic.trends)
library(sf)
library(data.table)
library(dplyr)

## by node -----

predicted_actual_by_node_orig <- fread(paste0("./data-raw/pred-and-act-vol-by-node.csv")) # our golden ticket!



predicted_actual_by_node <- predicted_actual_by_node_orig %>%
  mutate(date := as.IDate(date)) %>%
  filter(
    r_node_n_type != "Intersection",
    date > "2020-03-01"
  ) %>%
  mutate(
    corridor_route = stringr::str_replace(stringr::str_replace(stringr::str_replace(stringr::str_replace(corridor_route, "\\.", " "), "\\.", " "), "T H", "TH"), "U S", "US"),

    # scl_volume = scale(volume.predict, center = F),
    node_type = case_when(
      r_node_n_type == "Station" ~ "Freeway Segment",
      TRUE ~ r_node_n_type
    ),
    hover_text = paste(
      sep = "", "<b>", format.Date(date, "%A, %B %d"), "</b>", "<br>",
      node_type, " on ", corridor_route, " at ", r_node_label, "<br>",
      volume.diff, "%"
    ),
    District = "MnDOT Metro Freeways"
  )


unique_corridors <- unique(predicted_actual_by_node$corridor_route)

predicted_actual_by_node <- predicted_actual_by_node %>%
  group_by(node_type) %>%
  dplyr::group_split(keep = TRUE)


names(predicted_actual_by_node) <- c("Entrance", "Exit", "Freeway_Segment")

usethis::use_data(predicted_actual_by_node, overwrite = TRUE, compress = "xz")
usethis::use_data(unique_corridors, overwrite = TRUE, compress = "xz")



## by ctu -----

# ctu_diffs_join <- ctu_diffs_join%>%
#   group_by(CTU_NAME, date, year, dow, doy, woy, weekday, monthday)%>%
#   summarise(avg.diff = mean(volume.diff),
#             total.vol = sum(volume.sum),
#             pred.vol = sum(volume.predict),
#             num.nodes = uniqueN(r_node_name))

## by region -----
## predicted and actual summarized to the region (mostly sensors in the metro and Fargo/Moorehead) from March 1


predicted_actual_by_region <- fread(paste0("./data-raw/pred-and-act-vol-region.csv")) %>%
  mutate(typical_vmt_diff = `Difference from Typical VMT (%)`) %>%
  select(-`Difference from Typical VMT (%)`) %>%
  mutate(
    hover_text = paste(
      sep = "", "<b>", format.Date(date, "%A, %B %d"), "</b>", "<br>",
      round(typical_vmt_diff, digits = 1), "%"
    ),
    District = "MnDOT Metro Freeways"
  )

usethis::use_data(predicted_actual_by_region, overwrite = TRUE, compress = "xz")


# MnDOT Traffic Trends -----

predicted_actual_by_state <- fread(paste0("./data-raw/diff-vol-state.csv")) %>%
  mutate(typical_vmt_diff = `Difference from Typical VMT (%)`) %>%
  select(-`Difference from Typical VMT (%)`) %>%
  mutate(
    date = as.IDate(date),
    hover_text = paste(
      sep = "", "<b>", format.Date(date, "%A, %B %d"), "</b>", "<br>",
      round(typical_vmt_diff, digits = 1), "%"
    ),
    District = "MnDOT Statewide"
  )

# predicted_actual_by_state <- fread(paste0(
#       "https://mn.gov/covid19/assets/StateofMNResponseDashboardCSV_tcm1148-427143.csv"
#     )) %>%
#   janitor::clean_names() %>%
#   filter(covid_team == "Social Distancing",
#          geographic_level == "State") %>%
#   select(data_date_mm_dd_yyyy, value_number) %>%
#   mutate(District = "MnDOT Statewide",
#          date = as.IDate(data_date_mm_dd_yyyy, "%m/%d/%Y"),
#          typical_vmt_diff = as.numeric(value_number) * 100,
#          hover_text = paste(
#            sep = "", "<b>", format.Date(date, "%A, %B %d"), "</b>", "<br>",
#            typical_vmt_diff, "%"
#          )) %>%
#   select(District, date, typical_vmt_diff, hover_text)


usethis::use_data(predicted_actual_by_state, overwrite = TRUE, compress = "xz")
