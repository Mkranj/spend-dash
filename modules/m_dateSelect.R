# Create a pair of datepicker, for determining the start and end of the period
# for which expenses should be analysed.

dateSelectUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("picker_start")),
    span(class = "datepicker",
      actionButton(ns("earliest_date"), label = "", icon = icon("rotate-left"),
                   title = "Earliest available date"),
      actionButton(ns("start_minus"), label = "", icon = icon("minus"),
                   title = "-1 month"),
      actionButton(ns("start_plus"), label = "", icon = icon("plus"),
                   title = "+1 month")
    ),
    
    uiOutput(ns("picker_end")),
    span(class = "datepicker",
      actionButton(ns("latest_date"), label = "", icon = icon("rotate-left"),
                   title = "Latest available date"),
      actionButton(ns("end_minus"), label = "", icon = icon("minus"),
                   title = "-1 month"),
      actionButton(ns("end_plus"), label = "", icon = icon("plus"),
                   title = "+1 month")
    )
  )
}

dateSelectServer <- function(id, minDate, maxDate) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      stopifnot(is.reactive(minDate))
      stopifnot(is.reactive(maxDate))
      
      notification_duration_sec <- 2
      # All notifications share ID so they get overwritten instead of spammed
      notification_id <- "date_change"
      
      date_range <- reactive({
        req(input$startingDate)
        req(input$endDate)
        
        list(start = input$startingDate,
             end = input$endDate)
      })
      
      output$picker_start <- renderUI({
        airDatepickerInput(ns("startingDate"), label = "Start date:",
                           minDate = minDate(), maxDate = maxDate(),
                           value = minDate(), autoClose = T, addon = "none",
                           dateFormat = "dd.MM.yyyy.")
      })
      
      output$picker_end <- renderUI({
        airDatepickerInput(ns("endDate"), label = "End date:",
                           minDate = minDate(), maxDate = maxDate(),
                           value = maxDate(), autoClose = T, addon = "none",
                           dateFormat = "dd.MM.yyyy.")
      })
      
      # Ensure start and end don't overlap
      observeEvent(input$startingDate, {
        if (input$startingDate > input$endDate) {
          updateAirDateInput(session, inputId = "startingDate", value = input$endDate)
          showNotification("Start date cannot be set later than end date.",
                           duration = notification_duration_sec,
                           id = notification_id,
                           closeButton = F)
          }
      })
      
      observeEvent(input$endDate, {
        if (input$endDate < input$startingDate) {
          updateAirDateInput(session, inputId = "endDate", value = input$startingDate)
          showNotification("End date cannot be set earlier than start date.",
                           duration = notification_duration_sec,
                           id = notification_id,
                           closeButton = F)
        }
      })
      
      
      observeEvent(input$earliest_date, {
        updateAirDateInput(session, inputId = "startingDate", value = minDate())
      })
      
      observeEvent(input$latest_date, {
        updateAirDateInput(session, inputId = "endDate", value = maxDate())
      })
      
      observeEvent(input$start_minus, {
        month_minus <- input$startingDate %>% change_month("backward", "first")

        if (month_minus < minDate()) month_minus <- minDate()
        
        updateAirDateInput(session, inputId = "startingDate", value = month_minus)
      })
      
      observeEvent(input$start_plus, {
        month_plus <- input$startingDate %>% change_month("forward", "first")
        
        if (month_plus > maxDate()) month_plus <- maxDate()
        if (month_plus > input$endDate) {
          month_plus <- input$endDate
          showNotification("Start date cannot be set later than end date.",
                           duration = notification_duration_sec,
                           id = notification_id,
                           closeButton = F)
        }
        
        updateAirDateInput(session, inputId = "startingDate", value = month_plus)
      })
      
      observeEvent(input$end_minus, {
        month_minus <- input$endDate %>% change_month("backward", "last")
        
        if (month_minus < minDate()) month_minus <- minDate()
        if (month_minus < input$startingDate) {
          month_minus <- input$startingDate
          showNotification("End date cannot be set earlier than start date.",
                           duration = notification_duration_sec,
                           id = notification_id,
                           closeButton = F)
        }
        
        updateAirDateInput(session, inputId = "endDate", value = month_minus)
      })
      
      observeEvent(input$end_plus, {
        month_plus <- input$endDate %>% change_month("forward", "last")
        
        if (month_plus > maxDate()) month_plus <- maxDate()
        
        updateAirDateInput(session, inputId = "endDate", value = month_plus)
      })
      
      
      # Return:
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
#   recieved_dates <- dateSelectServer("dates", minDate = "2020-01-01", maxDate = "2021-06-01")
# 
#   output$selected <- renderText({
#     paste(recieved_dates()$start, recieved_dates()$end)
#     })
# }
# 
# shinyApp(ui, server)