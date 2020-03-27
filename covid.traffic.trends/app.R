# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all()
options(
  warn = -1,
  "golem.app.prod" = TRUE,
  "golem.pkg.name" = "covid.traffic.trends"
)
covid.traffic.trends::run_app() # add parameters here (if any)

## run code below in console only. DO NOT UN-COMMENT.

# rsconnect::deployApp(appDir = "A:/Employment/MetC/MTS/research.COVID19",
#                      account = "metrotransitmn",
#                      server = "shinyapps.io",
#                      appId = 1988757,
#                      appName = "research-covid19",
#                      appTitle = "research-covid19",
#                      launch.browser = function(url) {
#                        message("Deployment completed: ", url)},
#                      lint = FALSE, metadata = list(asMultiple = FALSE,
#                                                    asStatic = FALSE,
#                                                    ignoredFiles = "LICENSE|LICENSE.md|README.md|dev/01_start.R|dev/02_dev.R|dev/03_deploy.R|dev/run_dev.R|man/run_app.Rd|tests/testthat.R|tests/testthat/test-golem-recommended.R|data-raw"),
#                      logLevel = "verbose")