
#!/bin/sh -l

echo $PWD

Rscript -e "rsconnect::setAccountInfo(name='metrotransitmn', token='${SHINYAPPSIO_TOKEN}', secret='${SHINYAPPSIO_SECRET}')"
Rscript -e "rsconnect::deployApp(appDir = '/__w/loop-sensor-trends/covid.trafffic.trends', account = 'metrotransitmn', server = 'shinyapps.io', appName = 'covid-traffic-trends', appId = 2004244, lint = FALSE, metadata =  list(asMultiple = FALSE, asStatic = FALSE, ignoredFiles = 'LICENSE|LICENSE.md|README.md|README.Rmd|dev/01_start.R|dev/02_dev.R|dev/03_deploy.R|dev/run_dev.R|man/run_app.Rd|tests/testthat.R|tests/testthat/test-app.R|tests/testthat/test-golem-recommended.R|vignettes/potential_streetlight_analyses.Rmd'), logLevel = 'verbose')"
