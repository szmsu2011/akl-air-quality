aqi_heatmap_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("aqi_heatmap"), height = "1190px"))
}

aqi_heatmap_mod <- function(id, state) {
  module <- function(input, output, session) {
    ns <- session[["ns"]]

    output[["aqi_heatmap"]] <- renderEcharts4r({
      loc <- make_clean_names(state[["map_onclick"]])
      req(loc %in% state[["data"]][["location"]])

      data <- state[["data"]] %>%
        select(aqi, aqi_cat, location) %>%
        filter(location == loc)

      req(state[["year"]] %in% year(data[["datetime"]]))

      data <- data %>%
        filter(year(datetime) == state[["year"]]) %>%
        as_tibble() %>%
        group_by(date = date(datetime)) %>%
        summarise(agg_aqi = as.numeric(Max(aqi))) %>%
        ungroup() %>%
        mutate(
          aqi_cat = aqi_cat(agg_aqi),
          tt = paste0(
            "Date: <b>", fmt_date(date), "</b><br>",
            "Max AQI: <b>", agg_aqi, "</b><br>",
            "Max AQI Level: <b>", aqi_cat, "</b>"
          )
        ) %>%
        as_tsibble(index = date)

      level <- levels(data[["aqi_cat"]])

      data %>%
        mutate(aqi_cat = as.numeric(aqi_cat)) %>%
        e_charts(date) %>%
        e_calendar(range = state[["year"]], orient = "vertical", top = 95) %>%
        e_heatmap(aqi_cat, bind = tt, coord_system = "calendar") %>%
        e_visual_map(aqi_cat,
          show = FALSE,
          type = "piecewise",
          orient = "horizontal",
          top = "top",
          left = "center",
          pieces = map2(
            level, seq_along(level),
            function(x, i) {
              list(value = i, label = x, color = unname(aqi_pal[i]))
            }
          )
        ) %>%
        e_tooltip(borderWidth = 2, formatter = JS("
          function(params) {
            return params.name;
          }
        ")) %>%
        e_title(
          "Daily Max AQI",
          state[["map_onclick"]]
        ) %>%
        e_group("grp") %>%
        e_connect_group("grp")
    }) %>%
      bindCache(state[["map_onclick"]], state[["year"]])

    observeEvent(input[["aqi_heatmap_clicked_data"]], {
      aqi_date_selected <- input[["aqi_heatmap_clicked_data"]][["value"]][1]
      state[["aqi_date_selected"]] <- ymd(aqi_date_selected)

      output[["aqi_quantile"]] <- renderEcharts4r({
        data <- state[["data"]] %>%
          select(aqi, aqi_cat, location) %>%
          filter(location == make_clean_names(state[["map_onclick"]]))

        day_data <- data %>%
          filter(date(datetime) == state[["aqi_date_selected"]]) %>%
          mutate(hour = hour(datetime))

        e <- data %>%
          as_tibble() %>%
          group_by(hour = hour(datetime)) %>%
          summarise(
            lower = quantile(aqi, prob = .025, na.rm = TRUE) %>% round(2),
            median = quantile(aqi, prob = .5, na.rm = TRUE) %>% round(2),
            upper = quantile(aqi, prob = .975, na.rm = TRUE) %>% round(2)
          ) %>%
          select(-hour) %>%
          bind_cols(day_data) %>%
          mutate(tt = paste0("AQI: <b>", aqi, "</b>")) %>%
          e_charts(hour) %>%
          e_line(aqi, bind = tt) %>%
          e_scatter(
            median,
            symbol_size = 9.5,
            itemStyle = list(color = "#808080"),
            name = "median",
            tooltip = list(formatter = JS("
              function(x) {
                return 'Median: <b>' + x.value[1] + '</b>'
              }
            "))
          ) %>%
          e_legend(show = FALSE) %>%
          e_error_bar(lower, upper, name = "eb", tooltip = list(
            formatter = JS("
              function(x) {
                return '97.5% Quantile: <b>' + x.value[2] + '</b>' +
                  '<br>2.5% Quantile: <b>' + x.value[1] + '</b>';
              }
            ")
          )) %>%
          e_axis_labels(x = "(UTC+12:00)") %>%
          e_x_axis(nameLocation = "end") %>%
          e_title(paste(
            state[["map_onclick"]], "AQI,",
            fmt_date(state[["aqi_date_selected"]])
          )) %>%
          e_tooltip(formatter = JS("
            function(params) {
              return params.name;
            }
          "))

        for (x in c(50, 100, 150, 200, 300)) {
          e <- e %>%
            e_mark_line(
              data = list(
                yAxis = x,
                lineStyle = list(color = aqi_pal[[aqi_cat(x + 1)]]),
                tooltip = list(formatter = paste0("<b>", aqi_cat(x + 1), "</b>")),
                label = list(formatter = "")
              ),
              name = paste("mark", x),
              symbol = "none"
            )
        }

        e
      }) %>%
        bindCache(state[["map_onclick"]], state[["aqi_date_selected"]])

      showModal(modalDialog(
        echarts4rOutput(ns("aqi_quantile")),
        footer = NULL,
        size = "l",
        easyClose = TRUE
      ))
    })
  }

  moduleServer(id, module)
}
