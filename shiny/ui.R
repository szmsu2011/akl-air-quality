app_ui <- dashboardPage(
  dashboardHeader(title = "AKL Environment"),
  dashboardSidebar(sidebarMenu(
    menuItem("Air Quality", tabName = "aqi", icon = icon("dashboard")),
    menuItem("Wind", tabName = "wind", icon = icon("wind")),
    menuItem("Weather", tabName = "weather", icon = icon("cloud-sun"))
  )),
  dashboardBody(
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
      tabItem("weather", fluidPage(
        fluidRow(
          column(width = 4),
          column(selectInput("met_loc", "Site", station[["site"]]), width = 4),
          column(airMonthpickerInput(
            "yrmth", "Select Month",
            value = ymd(20201201),
            minDate = ymd(20160101), maxDate = ymd(20211201)
          ), width = 4)
        ),
        fluidRow(met_info_ui("met_info"))
      ))
    )
  )
)
