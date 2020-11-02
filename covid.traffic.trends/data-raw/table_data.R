## code to prepare `table_data` dataset goes here

library(covid.traffic.trends)
library(sf)
library(data.table)
library(dplyr)
## table data


table_data <- rbind(
  predicted_actual_by_region %>%
    mutate(
      `date` = as.character(`date`),
      weekday = lubridate::wday(date)
    ) %>%
    select(
      date,
      weekday,
      District,
      vmt.sum,
      vmt.predict,
      typical_vmt_diff
    ),
  mutate(predicted_actual_by_state,
    weekday = weekdays(`date`),
    vmt.sum = NA,
    vmt.predict = NA,
    `date` = as.character(`date`)
  ) %>%
    select(
      date,
      weekday,
      District,
      vmt.sum,
      vmt.predict,
      typical_vmt_diff
    )
)

usethis::use_data(table_data, overwrite = TRUE, compress = "xz")
