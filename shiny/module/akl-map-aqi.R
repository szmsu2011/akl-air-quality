map_aqi_ui <- function(id) {
  ns <- NS(id)

  tagList(leafletOutput(ns("map_aqi"), height = "1190px"))
}

map_aqi_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["map_aqi"]] <- renderLeaflet({
      leaflet(data = station) %>%
        addTiles() %>%
        setView(174.7485, -37.2109, zoom = 9.5) %>%
        addMarkers(~lng, ~lat, ~site, label = ~site)
    })

    observeEvent(input[["map_aqi_marker_click"]], {
      state[["map_onclick"]] <- input[["map_aqi_marker_click"]][["id"]]
    })
  }

  moduleServer(id, module)
}
