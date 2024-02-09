# Part of UI slotted into the dialog modal, that displays error messages (if any)
upload_popup_error_UI <- function(id) {
  ns <- NS(id)
  tagList(
  uiOutput(ns("error_message"))
  )
}

# error_text should be a reactiveValue that gets updated when an error is caught,
# and can be set to NULL to be removed from the UI
upload_popup_error_Server <- function(id, error_text) {
  moduleServer(
    id,
    function(input, output, session) {
      stopifnot(is.reactive(error_text))
      
      output$error_message <- renderUI({
        if (!is.null(error_text())) {
          div(
            span(class = "space-divider"),
            p(error_text(),
              class = "warn-user")
          )
        } else NULL
      })
    }
  )
}