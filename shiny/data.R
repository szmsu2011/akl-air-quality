append_data <- function(data, loc) {
  paste0("data/", gsub("_", "-", loc), ".csv") %>%
    read_csv(col_types = "Tddddddddddddddcd") %>%
    mutate(location = loc) %>%
    bind_rows(data) %>%
    as_tsibble(index = datetime, key = location)
}

station <- tibble(
  site = c(
    "Queen Street", "Glen Eden", "Penrose", "Henderson", "Pakuranga",
    "Takapuna", "Patumahoe", "Customs St", "Khyber Pass", "Papatoetoe"
  ),
  lng = c(
    174.7650, 174.6447, 174.8199, 174.6239, 174.8709,
    174.7651, 174.8282, 174.7683, 174.7693, 174.8645
  ),
  lat = -c(
    36.8482, 36.9225, 36.9045, 36.8679, 36.9130,
    36.7899, 37.2047, 36.8450, 36.8653, 36.9756
  )
)
