app_server <- function(input, output, session) {
  app_state <- eval_tidy(new_quosure(expr(reactiveValues(!!!initial_app_state))))

  aqi_heatmap_mod("aqi_heatmap", app_state)
  aqi_details_mod("aqi_details", app_state)
  map_aqi_mod("map_aqi", app_state)
  map_wind_mod("map_wind", app_state)
  wind_rose_mod("wind_rose", app_state)
  met_info_mod("met_info", app_state)
  # callback_mod("test", app_state)

  observeEvent(input[["year"]], {
    loc <- make_clean_names(app_state[["map_onclick"]])
    yr <- unique(year(filter(app_state[["data"]], location == loc)[["datetime"]]))
    updateSelectInput(session, "year2", "Year", sort(yr), input[["year"]])
    app_state[["year"]] <- input[["year"]]
  })

  observeEvent(input[["year2"]], {
    loc <- make_clean_names(app_state[["map_onclick"]])
    yr <- unique(year(filter(app_state[["data"]], location == loc)[["datetime"]]))
    updateSelectInput(session, "year", "Year", sort(yr), input[["year2"]])
    app_state[["year"]] <- input[["year2"]]
  })

  observeEvent(input[["yrmth"]], {
    app_state[["yrmth"]] <- ymd(input[["yrmth"]])
  })

  observeEvent(input[["met_loc"]], {
    app_state[["map_onclick"]] <- input[["met_loc"]]
  })

  observeEvent(app_state[["map_onclick"]], {
    loc <- make_clean_names(app_state[["map_onclick"]])

    if (!loc %in% app_state[["data"]][["location"]]) {
      app_state[["data"]] <- append_data(app_state[["data"]], loc)
    }

    yr <- unique(year(filter(app_state[["data"]], location == loc)[["datetime"]]))

    yr_tbl <- (app_state[["data"]] %>%
      filter(!is.na(aqi), location == loc) %>%
      as_tibble())[["datetime"]] %>%
      year() %>%
      table()

    last_yr <- as.numeric(last(names(yr_tbl)[which(yr_tbl > 4380)]))

    if (!length(last_yr)) last_yr <- max(yr)

    if (is.na(last_yr)) last_yr <- max(yr)

    if (app_state[["year"]] %in% yr) {
      updateSelectInput(session, "year", "Year", sort(yr), app_state[["year"]])
      updateSelectInput(session, "year2", "Year", sort(yr), app_state[["year"]])
    } else {
      updateSelectInput(session, "year", "Year", sort(yr), last_yr)
      updateSelectInput(session, "year2", "Year", sort(yr), last_yr)
    }

    updateSelectInput(
      session, "met_loc", "Site",
      station[["site"]],
      app_state[["map_onclick"]]
    )
  })

  output[["wind_loc"]] <- renderText(app_state[["map_onclick"]])
}
