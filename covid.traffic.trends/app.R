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

# rsconnect::deployApp(appDir = '.',
#                      account = 'metrotransitmn',
#                      server = 'shinyapps.io',
#                      appName = 'covid-traffic-trends', 
#                      appId = 2004244, lint = FALSE, 
#                      metadata =  list(asMultiple = FALSE, 
#                                       asStatic = FALSE, 
#                                       ignoredFiles = 'LICENSE|LICENSE.md|README.md|README.Rmd|dev/01_start.R|dev/02_dev.R|dev/03_deploy.R|dev/run_dev.R|man/run_app.Rd|tests/testthat.R|tests/testthat/test-app.R|tests/testthat/test-golem-recommended.R|vignettes/potential_streetlight_analyses.Rmd'), 
#                      logLevel = 'verbose')