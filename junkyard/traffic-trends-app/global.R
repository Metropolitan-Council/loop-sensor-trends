## global

#### Toolbox #### -----
library(shiny)
library(data.table)
library(leaflet)
library(sf)
library(ggplot2)
library(plotly)

#### Load Data #### -----
load("appdata.RData") # takes six seconds, not bad.
options(shiny.launch.browser = TRUE,
        scipen = 99999)


