#' Run all scripts in data-raw to update data for the app
#'
#' @return no output
#' @export
#'
run_daily_app_data <- function() {
  scripts <- list.files("data-raw/", pattern = "*.R", full.names = TRUE)
  o <- purrr::map(scripts, source)
}