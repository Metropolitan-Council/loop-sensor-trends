## code to prepare `covid_events` dataset goes here

library(dplyr)
library(data.table)

covid_events <- cbind(
  date = c("2020-03-19",
           "2020-03-21"),
  death_count = c(1,
                  1)
  ) %>% 
  data.table::as.data.table()
  


usethis::use_data(covid_events, overwrite = TRUE, compress = "xz")
