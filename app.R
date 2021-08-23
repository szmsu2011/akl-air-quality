library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(htmlwidgets)
library(shinyWidgets)
library(shinyjs)
library(rlang)
library(echarts4r)
library(reactable)
library(sparkline)
library(leaflet)
library(tsibble)
library(lubridate)
library(colorspace)
library(janitor)
library(tseries)

invisible(map(
  list.files("shiny",
    pattern = ".R",
    recursive = TRUE, full.names = TRUE
  ),
  source
))

Sys.setlocale(locale = "C")

shinyApp(app_ui, app_server)
