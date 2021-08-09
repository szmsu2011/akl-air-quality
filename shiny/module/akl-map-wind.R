map_wind_ui <- function(id) {
  ns <- NS(id)

  tagList(leafletOutput(ns("map_wind"), height = "1010px"))
}

map_wind_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["map_wind"]] <- renderLeaflet({
      leaflet(data = station) %>%
        addTiles() %>%
        setView(174.7485, -37.1124, zoom = 9.5) %>%
        addMarkers(~lng, ~lat, ~site, label = ~site)
    })

    observeEvent(input[["map_wind_marker_click"]], {
      state[["map_onclick"]] <- input[["map_wind_marker_click"]][["id"]]
    })
  }

  moduleServer(id, module)
}
