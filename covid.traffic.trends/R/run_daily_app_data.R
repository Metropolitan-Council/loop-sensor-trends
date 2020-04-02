#' Run all scripts in data-raw to update data for the app
#'
#' @return no output
#' @export
#'
run_daily_app_data <- function() {
  source("data-raw/predicted_actual_traffic.R")
  source("data-raw/mn_actions.R")
  source("data-raw/covid_events.R")
  source("data-raw/table_data.R")
}