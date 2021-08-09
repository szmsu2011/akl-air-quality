library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(htmlwidgets)
library(shinyWidgets)
library(rlang)
library(echarts4r)
library(reactable)
library(leaflet)
library(tsibble)
library(lubridate)
library(colorspace)
library(janitor)

## Load shiny modules
invisible(map(
  list.files("shiny", pattern = ".R", recursive = TRUE, full.names = TRUE),
  source
))

shinyApp(app_ui, app_server)
