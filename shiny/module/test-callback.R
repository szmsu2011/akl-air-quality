callback_ui <- function(id) {
  ns <- NS(id)

  tagList(
    column(
      width = 6,
      verbatimTextOutput(ns("test_out"))
    )
  )
}

callback_mod <- function(id, state) {
  module <- function(input, output, session) {
    observeEvent(state[["map_onclick"]], {
      output[["test_out"]] <- renderPrint(state[["yrmth"]])
    })
  }

  moduleServer(id, module)
}
