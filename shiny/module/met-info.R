met_info_ui <- function(id) {
  ns <- NS(id)

  tagList(reactableOutput(ns("met_info")))
}

met_info_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["met_info"]] <- renderReactable({
      loc <- make_clean_names(state[["map_onclick"]])
      req(loc %in% state[["data"]][["location"]])

      data <- state[["data"]] %>%
        as_tibble() %>%
        mutate(
          date = as_date(datetime),
          hour = sprintf("%02d:00", hour(datetime))
        ) %>%
        select(date, hour, temp, rh, ws, aqi, location) %>%
        filter(location == loc) %>%
        select(-location)

      req(state[["yrmth"]] %in% data[["date"]])

      bar_pal <- function(x, v) {
        x[is.na(x)] <- 0

        if (v == "aqi") {
          return(unname(aqi_pal[aqi_cat(x)]))
        }

        lim <- case_when(
          v == "temp" ~ c(0, 30),
          v == "rh" ~ c(130, 0),
          TRUE ~ c(-5, 20)
        )
        pal <- rev(head(rainbow(120), 100))

        map_chr(x, function(x) {
          pal[which.min(abs(seq(lim[1], lim[2], length = 100) - x))]
        })
      }

      data %>%
        filter(
          date >= floor_date(state[["yrmth"]], "week", 1),
          date < ceiling_date(state[["yrmth"]], "month")
        ) %>%
        pivot_longer(-(1:2)) %>%
        arrange(date, name, hour) %>%
        group_by(date, name) %>%
        summarise(value = list(c(value, NA))) %>%
        mutate(
          name = factor(name, c("temp", "rh", "ws", "aqi")),
          value = case_when(
            yearmonth(date) == yearmonth(state[["yrmth"]]) ~ value,
            TRUE ~ list(rep(NA, 25))
          )
        ) %>%
        arrange(date, name) %>%
        group_by(week_start = floor_date(date, "week", 1), name) %>%
        summarise(value = list({
          val <- do.call("c", value)
          c(val, rep(NA, 175 - length(val)))
        })) %>%
        mutate(
          week_start = fmt_date(week_start),
          value = map2(value, name, function(x, y) c(as.character(y), x)),
          name = case_when(
            name == "temp" ~ "Temperature (\u00B0C)",
            name == "rh" ~ "Relative Humidity (%)",
            name == "ws" ~ "Wind Speed (m/s)",
            TRUE ~ "AQI"
          )
        ) %>%
        reactable(
          columns = list(
            week_start = colDef(name = "Week Beginning", grouped = JS("
              function(cellInfo) {
                return cellInfo.value;
              }
            "), width = 140),
            name = colDef(name = "", width = 180),
            value = colDef(
              name = wday(1:7 + 1, label = TRUE, week_start = 1) %>%
                paste(collapse = paste(rep(" ", 14), collapse = " ")),
              cell = function(values) {
                var_name <- values[1]
                values <- as.numeric(values[-1])
                sparkline(
                  values,
                  width = 912,
                  barWidth = 38,
                  colorMap = bar_pal(values, var_name),
                  height = 50,
                  type = "bar",
                  chartRangeMin = 0
                )
              }, align = "center",
              headerStyle = list(whiteSpace = "pre")
            )
          ),
          groupBy = "week_start",
          defaultExpanded = TRUE,
          pagination = FALSE,
          sortable = FALSE
        )
    }) %>%
      bindCache(state[["map_onclick"]], state[["yrmth"]])
  }

  moduleServer(id, module)
}
