dateSelectUI <- function(id, minDate, maxDate) {
  ns <- NS(id)
  tagList(
    airDatepickerInput(ns("startingDate"), label = "Start date:",
                       minDate = minDate, maxDate = maxDate,
                       value = minDate, autoClose = T, addon = "left") %>% span(), 
    airDatepickerInput(ns("endDate"), label = "End date:",
                       minDate = minDate, maxDate = maxDate,
                       value = maxDate, autoClose = T, addon = "none")
  )
}

dateSelectServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      date_range <- reactive({
        validate(
          need(input$startingDate <= input$endDate,
               "End date should be equal or later than the start date!")
        )
        
        list(start = input$startingDate,
             end = input$endDate)
      })
      
      date_range
    }
  )
}

## EXAMPLE USAGE
# 
# ui <- fluidPage(
#   dateSelectUI("dates", minDate = "2020-01-01", maxDate = "2021-06-01"),
#   textOutput("selected")
# )
# 
# server <- function(input, output, session) {
#   
#   output$selected <- renderText({
#     paste(dateSelectServer("dates")()$start, dateSelectServer("dates")()$end)
#     })
# }
# 
# shinyApp(ui, server)