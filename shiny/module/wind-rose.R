wind_rose_ui <- function(id) {
  ns <- NS(id)

  column(
    column(
      tagList(echarts4rOutput(ns("wind_rose1"), height = "300px")),
      tagList(echarts4rOutput(ns("wind_rose4"), height = "230px")),
      tagList(echarts4rOutput(ns("wind_rose7"), height = "230px")),
      tagList(echarts4rOutput(ns("wind_rose10"), height = "230px")),
      width = 4
    ),
    column(
      tagList(echarts4rOutput(ns("wind_rose2"), height = "300px")),
      tagList(echarts4rOutput(ns("wind_rose5"), height = "230px")),
      tagList(echarts4rOutput(ns("wind_rose8"), height = "230px")),
      tagList(echarts4rOutput(ns("wind_rose11"), height = "230px")),
      width = 4
    ),
    column(
      tagList(echarts4rOutput(ns("wind_rose3"), height = "300px")),
      tagList(echarts4rOutput(ns("wind_rose6"), height = "230px")),
      tagList(echarts4rOutput(ns("wind_rose9"), height = "230px")),
      tagList(echarts4rOutput(ns("wind_rose12"), height = "230px")),
      width = 4
    ),
    width = 12
  )
}

wind_rose_mod <- function(id, state) {
  module <- function(input, output, session) {
    pat_wd <- "(\\(|\\[)(\\d+),(\\d+)(\\])"

    e_wind_rose <- function(wind_data, yr, mth, loc) {
      e <- (data <- wind_data %>%
        filter(
          location == make_clean_names(loc),
          year(datetime) == yr,
          month(datetime) == mth
        ) %>%
        mutate(
          # wind_dir = case_when(
          #   wind_dir >= 180 ~ wind_dir - 180,
          #   TRUE ~ wind_dir + 180
          # ),
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
        pivot_wider(names_from = ws, values_from = p, values_fill = 0)) %>%
        e_charts(wd) %>%
        e_angle_axis(wd, type = "category", axisLabel = list(formatter = "")) %>%
        e_radius_axis(max = 50, axisLabel = list(formatter = "{value}%")) %>%
        e_polar(center = c("50%", ifelse(mth < 4, 200, "50%"))) %>%
        e_tooltip(trigger = "axis")

      for (y in names(data)[-1]) {
        e <- e %>%
          e_bar_(y, coord_system = "polar", stack = "stack")
      }

      if (mth != 1L) {
        e <- e %>%
          e_group("grp2") %>%
          e_connect_group("grp2")
      } else {
        e <- e %>%
          e_group("grp2")
      }

      e
    }

    map(1:12, function(i) {
      output[["wind_rose" %>% paste0(i)]] <- renderEcharts4r({
        state[["data"]] %>%
          select(ws, wind_dir, location) %>%
          drop_na() %>%
          e_wind_rose(state[["year"]], i, state[["map_onclick"]]) %>%
          e_legend(show = i == 3, right = 0, orient = "vertical") %>%
          e_title(
            month(i, label = TRUE),
            padding = ifelse(i < 4, 100, 15) %>% c(rep(0, 3))
          )
      }) %>%
        bindCache(state[["year"]], state[["map_onclick"]], i)
    })
  }

  moduleServer(id, module)
}
