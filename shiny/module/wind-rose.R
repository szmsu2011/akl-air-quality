wind_rose_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      tagList(echarts4rOutput(ns("wind_rose0"), height = "30px"))
    ),
    fluidRow(
      column(
        tagList(echarts4rOutput(ns("wind_rose1"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose4"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose7"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose10"), height = "230px")),
        width = 4
      ),
      column(
        tagList(echarts4rOutput(ns("wind_rose2"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose5"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose8"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose11"), height = "230px")),
        width = 4
      ),
      column(
        tagList(echarts4rOutput(ns("wind_rose3"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose6"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose9"), height = "230px")),
        tagList(echarts4rOutput(ns("wind_rose12"), height = "230px")),
        width = 4
      ),
      width = 12
    )
  )
}

wind_rose_mod <- function(id, state) {
  module <- function(input, output, session) {
    pat_wd <- "(\\(|\\[)(\\d+),(\\d+)(\\])"

    e_wind_rose <- function(wind_data, yr, mth, loc) {
      data <- wind_data %>%
        filter(
          location == make_clean_names(loc),
          year(datetime) == yr,
          month(datetime) == mth
        ) %>%
        mutate(
          wd = cut(wind_dir, seq(0, 360, 30), include.lowest = TRUE),
          wd = gsub(pat_wd, "% From \\2-\\3 deg", wd) %>%
            factor(gsub(pat_wd, "% From \\2-\\3 deg", levels(wd))),
          ws = factor(case_when(
            between(ws, -Inf, 2) ~ "0-2 m/s",
            between(ws, 2, 4) ~ "2-4 m/s",
            between(ws, 4, 6) ~ "4-6 m/s",
            between(ws, 6, Inf) ~ ">6 m/s"
          ), paste(c("0-2", "2-4", "4-6", ">6"), "m/s"))
        ) %>%
        as_tibble() %>%
        count(ws, wd, .drop = FALSE) %>%
        mutate(p = round(n / sum(n) * 100, 2)) %>%
        select(-n) %>%
        pivot_wider(names_from = ws, values_from = p, values_fill = 0)

      if (mth == 0) data[-1] <- 11

      e <- data %>%
        e_charts(wd) %>%
        e_angle_axis(wd, type = "category", axisLabel = list(formatter = "")) %>%
        e_radius_axis(max = 50, axisLabel = list(formatter = "{value}%")) %>%
        e_polar(center = c("50%", ifelse(mth == 0, "300%", "50%"))) %>%
        e_tooltip(trigger = "axis")

      for (y in names(data)[-1]) {
        e <- e %>%
          e_bar_(y, coord_system = "polar", stack = "stack")
      }

      e
    }

    map(0:12, function(i) {
      output[["wind_rose" %>% paste0(i)]] <- renderEcharts4r({
        state[["data"]] %>%
          select(ws, wind_dir, location) %>%
          drop_na() %>%
          e_wind_rose(state[["year"]], i, state[["map_onclick"]]) %>%
          e_legend(show = i == 0) %>%
          e_title(
            show = mth != 0,
            month(i + (i == 0), label = TRUE),
            padding = c(15, rep(0, 3))
          )
      }) %>%
        bindCache(state[["year"]], state[["map_onclick"]], i)
    })
  }

  moduleServer(id, module)
}
