app_ui <- dashboardPage(
  dashboardHeader(title = "AKL Environment"),
  dashboardSidebar(sidebarMenu(
    menuItem("Air Quality", tabName = "aqi", icon = icon("dashboard")),
    menuItem("Weather", tabName = "weather", icon = icon("cloud-sun")),
    menuItem("Wind", tabName = "wind", icon = icon("wind")),
    menuItem("Time Series", tabName = "tseries", icon = icon("chart-line"))
  )),
  dashboardBody(
    useShinyjs(),
    shinyDashboardThemes(theme = "blue_gradient"),
    tabItems(
      tabItem("aqi", fluidPage(
        fluidRow(
          column(width = 8),
          column(selectInput("year", "Year", ""), width = 4)
        ),
        fluidRow(
          column(map_aqi_ui("map_aqi"), width = 4),
          column(aqi_heatmap_ui("aqi_heatmap"), width = 4),
          column(aqi_details_ui("aqi_details"), width = 4)
        )
      )),
      tabItem("weather", fluidPage(
        fluidRow(
          column(width = 4),
          column(selectInput("met_loc", "Site", station[["site"]]), width = 4),
          column(airMonthpickerInput("yrmth", "Select Month"), width = 4)
        ),
        fluidRow(met_info_ui("met_info"))
      )),
      tabItem("wind", fluidPage(
        fluidRow(
          column(width = 4),
          column(textOutput("wind_loc"), width = 4),
          column(selectInput("year2", "Year", ""), width = 4),
          tags[["head"]](tags[["style"]]("#wind_loc{
            padding-top:40px;padding-left:30px;font-weight:bold;font-size:20px;
          }"))
        ),
        fluidRow(
          column(map_wind_ui("map_wind"), width = 4),
          column(wind_rose_ui("wind_rose"), width = 8)
        )
      )),
      tabItem("tseries", fluidPage(
        fluidRow(
          column(selectInput("ts_loc", "Site", station[["site"]]), width = 3),
          column(airYearpickerInput("ts_yr", "Years", range = TRUE), width = 3),
          column(selectInput("ts_var", "Parameter", ""), width = 3),
          column(selectInput(
            "ts_mu", "Mean Structure",
            c("Arithmetic", "Geometric"),
            selected = "Arithmetic"
          ), width = 3)
        ),
        fluidRow(
          column(selectInput(
            "ts_trend", "Trend Model",
            c("Null", "Linear"),
            selected = "Linear"
          ), width = 3),
          column(selectInput(
            "ts_autocor", "Autocorrelation Model",
            c("Null", paste0("AR(", seq_len(3), ")")),
            selected = "Null"
          ), width = 3),
          column(selectInput(
            "ts_vov", "Heteroscedasticity Model",
            c("Null", "GARCH(1,1)"),
            selected = "Null"
          ), width = 3),
          column(selectInput(
            "ts_int", "Interval",
            c("Null", "Confidence", "Prediction"),
            selected = "Confidence"
          ), width = 3)
        ),
        fluidRow(ts_model_ui("ts_model"))
      ))
    )
  )
)
