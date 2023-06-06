dateSelectUI <- function(id, minDate, maxDate) {
  ns <- NS(id)
  tagList(
    
    airDatepickerInput(ns("startingDate"), label = "Start date:",
                         minDate = minDate, maxDate = maxDate,
                         value = minDate, autoClose = T, addon = "none"),
    span(class = "datepicker",
      actionButton(ns("earliest_date"), label = "", icon = icon("rotate-left"),
                   title = "Earliest available date"),
      actionButton(ns("start_minus"), label = "", icon = icon("minus"),
                   title = "-1 month"),
      actionButton(ns("start-plus"), label = "", icon = icon("plus"),
                   title = "+1 month")
    ),
    
    airDatepickerInput(ns("endDate"), label = "End date:",
                         minDate = minDate, maxDate = maxDate,
                         value = maxDate, autoClose = T, addon = "none") ,
    span(class = "datepicker",
      actionButton(ns("latest_date"), label = "", icon = icon("rotate-left"),
                   title = "Latest available date"),
      actionButton(ns("end_minus"), label = "", icon = icon("minus"),
                   title = "-1 month"),
      actionButton(ns("end-plus"), label = "", icon = icon("plus"),
                   title = "+1 month")
    )
  )
}

dateSelectServer <- function(id, minDate, maxDate) {
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
      
      observeEvent(input$earliest_date, {
        updateAirDateInput(session, inputId = "startingDate", value = minDate)
      })
      
      observeEvent(input$latest_date, {
        updateAirDateInput(session, inputId = "endDate", value = maxDate)
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