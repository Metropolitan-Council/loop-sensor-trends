## code to prepare `mn_actions` dataset goes here
## actions taken by the governor and MDH

mn_actions <- cbind(
  date = c(
    "2020-03-06", # MN Dept. of Health confirms 1st COVID-19 case in MN
    # '2020-03-11', # UMN Suspends In-Person Classes
    "2020-03-13", # Gov. Walz declares peacetime emergency; calls for cancellation of events >250 ppl
    # "2020-03-18", # Gov. Walz & MDH ask all gyms, bars, public spaces to close, restaurants limit to take-out
    "2020-03-27", # Gov. Walz & MDH ask everyone to stay home except for essential needs
    # "2020-04-08", # Gov. Walz & MDH extend stay-at-home order to May 4
    # "2020-04-12",
    "2020-04-27",
    "2020-05-18",
    # '2020-04-30',
    "2020-06-10" # restaurants, salons, and other workplaces open with restrictions
  ),
  action = c(
    "MDH confirms 1st COVID-19 case in MN",
    # 'UMN Suspends In-Person Classes',
    "Peacetime emergency declared",
    # "Public schools close. In-person dining suspended",
    "Stay at home order begins",
    # "Stay at home order extends to May 4",
    # "Easter Sunday snowstorm",
    "Some workplaces open with restrictions",
    "Stay at home order expires",
    "Restaurants, salons, and other workplaces open with restrictions"
  )
  # link = c('https://www.health.state.mn.us/diseases/coronavirus/situation.html',
  #          # 'https://safe-campus.umn.edu/public-health-alerts',
  #          'https://mn.gov/governor/assets/EO%2020-01_tcm1055-422957.pdf',
  #          'https://mn.gov/governor/news/?id=1055-423071',
  #          'https://mn.gov/governor/assets/2020_03_16_EO_20_04_Bars_Restaurants_tcm1055-423380.pdf',
  #          'https://mn.gov/governor/assets/3a.%20EO%2020-20%20FINAL%20SIGNED%20Filed_tcm1055-425020.pdf',
  #          'https://mn.gov/governor/assets/2a.%20EO%2020-33%20Final_tcm1055-427370.pdf',
  #          'https://mn.gov/governor/assets/EO%2020-40%20Final_tcm1055-429564.pdf')
) %>%
  data.table::as.data.table() %>%
  dplyr::mutate(date = as.character(date)) %>%
  dplyr::left_join(predicted_actual_by_region %>%
    mutate(date = as.character(date)), by = "date")



usethis::use_data(mn_actions, overwrite = TRUE, compress = "xz")
