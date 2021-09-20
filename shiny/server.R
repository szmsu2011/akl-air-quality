app_server <- function(input, output, session) {
  app_state <- eval_tidy(new_quosure(expr(reactiveValues(!!!initial_app_state))))

  aqi_heatmap_mod("aqi_heatmap", app_state)
  aqi_details_mod("aqi_details", app_state)
  map_aqi_mod("map_aqi", app_state)
  map_wind_mod("map_wind", app_state)
  wind_rose_mod("wind_rose", app_state)
  met_info_mod("met_info", app_state)
  ts_model_mod("ts_model", app_state)

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

  observeEvent(input[["ts_yr"]], {
    ts_yr <- year(ymd(input[["ts_yr"]]))
    app_state[["ts_yr"]] <- c(first(ts_yr), last(ts_yr))
  })

  observeEvent(input[["met_loc"]], {
    app_state[["map_onclick"]] <- input[["met_loc"]]
  })

  observeEvent(input[["ts_loc"]], {
    app_state[["map_onclick"]] <- input[["ts_loc"]]
  })

  observeEvent(input[["var"]], {
    app_state[["var"]] <- input[["var"]]
  })

  observeEvent(input[["ts_var"]], {
    app_state[["ts_var"]] <- input[["ts_var"]]
  })

  observeEvent(input[["ts_mu"]], {
    app_state[["ts_geomean"]] <- input[["ts_mu"]] == "Geometric"
  })

  observeEvent(input[["ts_trend"]], {
    app_state[["ts_trend"]] <- input[["ts_trend"]]
  })

  observeEvent(input[["ts_autocor"]], {
    app_state[["ts_autocor"]] <- input[["ts_autocor"]]
  })

  observeEvent(input[["ts_vov"]], {
    app_state[["ts_vov"]] <- input[["ts_vov"]]
  })

  observeEvent(input[["ts_int"]], {
    app_state[["ts_int"]] <- input[["ts_int"]]
    if (app_state[["ts_int"]] == "Null") {
      updateSelectInput(session, "ts_vov", selected = "Null")
      disable("ts_vov")
    } else {
      enable("ts_vov")
    }
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

    d_yr <- ifelse(app_state[["year"]] %in% yr, app_state[["year"]], last_yr)

    updateSelectInput(session, "year", "Year", sort(yr), d_yr)
    updateSelectInput(session, "year2", "Year", sort(yr), d_yr)

    updateAirDateInput(session, "yrmth",
      value = ymd("00000101") + years(d_yr),
      options = list(
        minDate = ymd("00000101") + years(first(sort(yr))),
        maxDate = ymd("00001201") + years(last(sort(yr)))
      )
    )
    updateAirDateInput(session, "ts_yr",
      value = ymd("00000101") + years(d_yr),
      options = list(
        minDate = ymd("00000101") + years(first(sort(yr))),
        maxDate = ymd("00000101") + years(last(sort(yr)))
      )
    )

    updateSelectInput(
      session, "met_loc", "Site",
      station[["site"]],
      app_state[["map_onclick"]]
    )

    updateSelectInput(
      session, "ts_loc", "Site",
      station[["site"]],
      app_state[["map_onclick"]]
    )

    updateSelectInput(
      session, "ts_var", "Parameter",
      gsub("X", "x", toupper(names(app_state[["data"]])[4:15])),
      selected = "AQI"
    )
  })

  output[["wind_loc"]] <- renderText(app_state[["map_onclick"]])
}
