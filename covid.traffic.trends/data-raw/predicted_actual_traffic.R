## code to prepare `predicted_actual` dataset series goes here

library(covid.traffic.trends)
library(sf)
library(data.table)
library(dplyr)

## by node -----

try_today <- try(fread(paste0("./data-raw/pred-and-act-vol-by-node-", Sys.Date(), ".csv")), silent = TRUE)
if(class(try_today[1]) == "try-error"){
  message("Node data for ", Sys.Date(), " is not yet available")
  message("Keeping previous data")
} else {
  predicted_actual_by_node <- fread(paste0("./data-raw/pred-and-act-vol-by-node-", Sys.Date(), ".csv")) # our golden ticket!
  predicted_actual_by_node[, date := as.IDate(date)]
  predicted_actual_by_node <- predicted_actual_by_node[date > "2020-03-01", ][r_node_n_type != "Intersection",]
  
  
  predicted_actual_by_node <- predicted_actual_by_node[, scl_volume := scale(volume.predict, center = F)] %>%
    mutate(
      corridor_route = stringr::str_replace(stringr::str_replace(stringr::str_replace(stringr::str_replace(corridor_route, "\\.", " "),"\\.", " "), "T H", "TH"), "U S", "US"),
      node_type = case_when(r_node_n_type == "Station" ~ "Freeway Segments",
                            TRUE ~ r_node_n_type),
      hover_text = paste(
        sep = "", "<b>", format.Date(date, "%A, %B %d"), "</b>", "<br>",
        node_type, " on ", corridor_route, " at ", r_node_label, "<br>",
        volume.diff, "%"
      ),
      District = "MnDOT Metro Freeways"
    ) %>% 
    group_by(node_type) %>% 
    dplyr::group_split(keep = TRUE)
  
  
  names(predicted_actual_by_node) <- c("Entrance", "Exit", "Freeway_Segment")
  
  usethis::use_data(predicted_actual_by_node, overwrite = TRUE, compress = "xz")
  
}


## by ctu -----

# ctu_diffs_join <- ctu_diffs_join%>%
#   group_by(CTU_NAME, date, year, dow, doy, woy, weekday, monthday)%>%
#   summarise(avg.diff = mean(volume.diff),
#             total.vol = sum(volume.sum),
#             pred.vol = sum(volume.predict),
#             num.nodes = uniqueN(r_node_name))

## by region -----
## predicted and actual summarized to the region (mostly sensors in the metro and Fargo/Moorehead) from March 1

try_today <- try(fread(paste0("./data-raw/pred-and-act-vol-region-", Sys.Date(), ".csv")), silent = TRUE)

if(class(try_today)[1] == "try-error"){
  message("Region data for ", Sys.Date(), " is not yet available")
  message("Keeping previous data")
} else {
  predicted_actual_by_region <- fread(paste0("./data-raw/pred-and-act-vol-region-", Sys.Date(), ".csv")) %>%
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
}


# MnDOT Traffic Trends -----



get_mndot_data <- function() {
  yesterday <- Sys.Date() - 1
  yesterday <- as.IDate(yesterday)
  yesterday <- paste0(month(yesterday), "-", mday(yesterday), "-", year(yesterday))
  
  day_before_yesterday <- Sys.Date() - 2
  day_before_yesterday <- as.IDate(day_before_yesterday)
  day_before_yesterday <- paste0(month(day_before_yesterday), "-", mday(day_before_yesterday), "-", year(day_before_yesterday))
  
  try_today <- try(suppressWarnings(fread(paste0("http://www.dot.state.mn.us/traffic/data/reports/COVID19/Daily_Volume_Change_", yesterday, "_update.csv"),
                                          verbose = FALSE,
                                          showProgress = FALSE
  )),
  silent = TRUE
  )
  if (class(try_today)[1] == "try-error") {
    message("State data for ", yesterday, " is unavailable")
    message("Returning state data for ", day_before_yesterday)
    final <- fread(paste0(
      "http://www.dot.state.mn.us/traffic/data/reports/COVID19/Daily_Volume_Change_",
      day_before_yesterday, "_update.csv"
    ),
    verbose = FALSE,
    showProgress = FALSE
    )
  } else {
    final <- try_today
  }
  return(final)
}

predicted_actual_by_state <- get_mndot_data()

predicted_actual_by_state <- predicted_actual_by_state[District %in% c("MnDOT Statewide")]
predicted_actual_by_state <- melt(predicted_actual_by_state,
                                  id.vars = c("District"),
                                  variable.name = "date",
                                  value.name = "typical_vmt_diff"
)
predicted_actual_by_state <- predicted_actual_by_state[, date := as.IDate(date, format = "%m/%d/%Y")] %>%
  mutate(hover_text = paste(
    sep = "", "<b>", format.Date(date, "%A, %B %d"), "</b>", "<br>",
    typical_vmt_diff, "%"
  ))

usethis::use_data(predicted_actual_by_state, overwrite = TRUE, compress = "xz")
