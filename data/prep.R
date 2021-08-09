library(tidyverse)
library(lubridate)
library(tsibble)

source("shiny/app/function.R")

wind_dir_data <- "data/akl-wind-dir.csv" %>%
  read_csv(col_types = "Tcd") %>%
  mutate(datetime = datetime + hours(12))

data <- "data/akl-env-data.csv" %>%
  read_csv(col_types = "Tcdddddddddddddd") %>%
  mutate(
    datetime = datetime + hours(12),
    aqi_cat = aqi_cat(aqi)
  ) %>%
  filter(year(datetime) > 2015) %>%
  left_join(wind_dir_data, by = c("datetime", "location")) %>%
  as_tsibble(index = datetime, key = location)

for (loc in unique(data[["location"]])) {
  data %>%
    filter(location == loc) %>%
    select(-location) %>%
    write_csv(paste0("data/app/", gsub("_", "-", loc), ".csv"))
}
