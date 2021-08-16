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
        filter(yearmonth(date) == yearmonth(state[["yrmth"]])) %>%
        pivot_longer(-(1:2)) %>%
        arrange(date, name, hour) %>%
        group_by(date, name) %>%
        summarise(value = list(value)) %>%
        mutate(name = factor(name, c("temp", "rh", "ws", "aqi"))) %>%
        arrange(date, name) %>%
        mutate(
          date = fmt_date(date),
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
            date = colDef(name = "Date", grouped = JS("
              function(cellInfo) {
                return cellInfo.value;
              }
            "), width = 200),
            name = colDef(name = "", width = 200),
            value = colDef(name = "", cell = function(values) {
              var_name <- values[1]
              values <- as.numeric(values[-1])
              sparkline(
                values,
                width = 696,
                barWidth = 29,
                colorMap = bar_pal(values, var_name),
                height = 50,
                type = "bar",
                chartRangeMin = 0
              )
            }, align = "center")
          ),
          groupBy = "date",
          defaultExpanded = TRUE,
          pagination = FALSE,
          sortable = FALSE
        )
    }) %>%
      bindCache(state[["map_onclick"]], state[["yrmth"]])
  }

  moduleServer(id, module)
}
