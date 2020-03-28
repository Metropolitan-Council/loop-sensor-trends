## code to prepare `mn_actions` dataset goes here
## actions taken by the governor and MDH

mn_actions <- cbind(
  date = c('2020-03-06', # MN Dept. of Health confirms 1st COVID-19 case in MN
           '2020-03-11', # UMN Suspends In-Person Classes
           '2020-03-13', # Gov. Walz declares peacetime emergency; calls for cancellation of events >250 ppl
           '2020-03-15', # Gov. Walz announces public schools will close by Mar. 18
           '2020-03-18', # Gov. Walz & MDH ask all gyms, bars, public spaces to close, restaurants limit to take-out
           '2020-03-27' # Gov. Walz & MDH ask everyone to stay home except for essential needs
           ),
  action = c('MN Dept. of Health confirms 1st COVID-19 case in MN', 
             'UMN Suspends In-Person Classes', 
             'Gov. Walz declares peacetime emergency', 
             'Gov. Walz & MDH announce public schools will close by March 18',
             'Gov. Walz & MDH ask all gyms, bars, public spaces to close, restaurants limit to take-out',
             'Gov. Walz & MDH ask everyone to stay home except for essential needs'
  ),
  link = c('https://www.health.state.mn.us/diseases/coronavirus/situation.html',
           'https://safe-campus.umn.edu/public-health-alerts',
           'https://mn.gov/governor/assets/EO%2020-01_tcm1055-422957.pdf',
           'https://mn.gov/governor/news/?id=1055-423071',
           'https://mn.gov/governor/assets/2020_03_16_EO_20_04_Bars_Restaurants_tcm1055-423380.pdf',
           'https://mn.gov/governor/covid-19/'
           )
) %>% 
  data.table::as.data.table() %>% 
  dplyr::left_join(predicted_actual_by_region, by = "date") 



usethis::use_data(mn_actions, overwrite = TRUE, compress = "xz")
