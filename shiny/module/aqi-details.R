aqi_details_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("aqi_details"), height = "1190px"))
}

aqi_details_mod <- function(id, state) {
  module <- function(input, output, session) {
    ns <- session[["ns"]]

    output[["aqi_details"]] <- renderEcharts4r({
      loc <- make_clean_names(state[["map_onclick"]])
      req(loc %in% state[["data"]][["location"]])

      data <- state[["data"]] %>%
        select(aqi, !!sym("pm2.5"), pm10, no2, so2, co, o3, location) %>%
        filter(location == loc)

      req(state[["year"]] %in% year(data[["datetime"]]))

      data <- filter(data, year(datetime) == state[["year"]])

      pol <- names(data)[2:7]

      data <- data %>%
        mutate(aqi_pol = toupper(data[2:7] %>%
          array_branch(1) %>%
          map_chr(function(x) {
            p <- pol[which.max(x / c(25, 50, 200, 350, 10, 150))]
            if (!length(p)) NA else p
          }))) %>%
        as_tibble() %>%
        group_by(date = date(datetime)) %>%
        summarise(aqi_pol = factor(names(table(aqi_pol))[1], toupper(pol))) %>%
        ungroup() %>%
        mutate(tt = paste0(
          "Date: <b>", fmt_date(date), "</b><br>",
          "Key AQI Constituent: <b>", aqi_pol, "</b>"
        )) %>%
        as_tsibble(index = date)

      level <- levels(data[["aqi_pol"]])
      col <- qualitative_hcl(length(level))

      data %>%
        mutate(aqi_pol = as.numeric(aqi_pol)) %>%
        e_charts(date) %>%
        e_calendar(range = state[["year"]], orient = "vertical", top = 95) %>%
        e_heatmap(aqi_pol, bind = tt, coord_system = "calendar") %>%
        e_visual_map(aqi_pol,
          show = FALSE,
          type = "piecewise",
          top = "top",
          left = "center",
          pieces = map2(
            level, seq_along(level),
            function(x, i) {
              list(value = i, label = x, color = col[i])
            }
          )
        ) %>%
        e_title(
          "Key Pollutant",
          state[["map_onclick"]]
        ) %>%
        e_tooltip(formatter = JS("
          function(params) {
            return params.name;
          }
        ")) %>%
        e_group("grp")
    }) %>%
      bindCache(state[["map_onclick"]], state[["year"]])

    observeEvent(input[["aqi_details_clicked_data"]], {
      con_date_selected <- input[["aqi_details_clicked_data"]][["value"]][1]
      state[["con_date_selected"]] <- ymd(con_date_selected)

      con_selected <- input[["aqi_details_clicked_data"]][["name"]]
      con_selected <- gsub("(.*)t: <b>(.*)<(.*)", "\\2", con_selected)

      output[["con_quantile"]] <- renderEcharts4r({
        con_data <- state[["data"]] %>%
          select(aqi, !!sym("pm2.5"), pm10, no2, so2, co, o3, location)

        day_data <- (data <- con_data %>%
          filter(location == make_clean_names(state[["map_onclick"]])) %>%
          mutate(pol = !!sym(tolower(con_selected)))) %>%
          filter(date(datetime) == state[["con_date_selected"]]) %>%
          mutate(hour = hour(datetime))

        threshold <- c(25, 50, 200, 350, 10, 150)

        data %>%
          as_tibble() %>%
          group_by(hour = hour(datetime)) %>%
          summarise(
            lower = quantile(pol, prob = .025, na.rm = TRUE) %>% round(2),
            median = quantile(pol, prob = .5, na.rm = TRUE) %>% round(2),
            upper = quantile(pol, prob = .975, na.rm = TRUE) %>% round(2)
          ) %>%
          select(-hour) %>%
          bind_cols(day_data) %>%
          mutate(tt = paste0(con_selected, ": <b>", pol, "</b>")) %>%
          e_charts(hour) %>%
          e_line(pol, bind = tt) %>%
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
          e_mark_line(
            data = list(
              yAxis = threshold[tolower(con_selected) == names(con_data)[2:7]],
              lineStyle = list(color = "steelblue"),
              tooltip = list(formatter = "<b>Threshold</b>"),
              label = list(formatter = "")
            ),
            name = "mark",
            symbol = "none"
          ) %>%
          e_axis_labels(x = "(UTC+12:00)") %>%
          e_x_axis(nameLocation = "end") %>%
          e_title(paste0(
            state[["map_onclick"]], " ", con_selected, ", ",
            fmt_date(state[["con_date_selected"]])
          )) %>%
          e_tooltip(formatter = JS("
            function(params) {
              return params.name;
            }
          "))
      }) %>%
        bindCache(state[["map_onclick"]], state[["con_date_selected"]])

      showModal(modalDialog(
        echarts4rOutput(ns("con_quantile")),
        footer = NULL,
        size = "l",
        easyClose = TRUE
      ))
    })
  }

  moduleServer(id, module)
}
