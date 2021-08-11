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

      data %>%
        filter(yearmonth(date) == yearmonth(state[["yrmth"]])) %>%
        pivot_longer(-(1:2)) %>%
        arrange(date, name, hour) %>%
        group_by(date, name) %>%
        summarise(value = list(value)) %>%
        mutate(
          date = fmt_date(date),
          name = factor(name, c("temp", "rh", "ws", "aqi"))
        ) %>%
        arrange(date, name) %>%
        mutate(
          value = case_when(
            name == "aqi" ~ map(value, function(x) c("aqi", x)),
            TRUE ~ value
          ),
          name = case_when(
            name == "temp" ~ "Temperature (\u00B0C)",
            name == "rh" ~ "Relative Humidity (%)",
            name == "ws" ~ "Wind Speed (m/s)",
            name == "aqi" ~ "AQI"
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
              if (is.na(values[1])) {
                is_aqi <- FALSE
              } else if (values[1] == "aqi") {
                is_aqi <- TRUE
                values <- as.numeric(values[-1])
              } else {
                is_aqi <- FALSE
              }
              sparkline(values,
                width = 696,
                barWidth = 29,
                colorMap = case_when(
                  is_aqi ~ unname(aqi_pal[aqi_cat(values)]),
                  TRUE ~ "steelblue"
                ),
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
