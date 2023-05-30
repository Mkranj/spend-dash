DailyExpensesPopupUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionBttn(inputId = ns("show_modal"), label = "Show dailies")
  )
}

DailyExpensesPopupServer <- function(id, input_data) {
  moduleServer(
    id,
    function(input, output, session) {
    
      stopifnot(is.reactive(input_data))
      ns <- NS(id)
    
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          plotlyOutput(ns("dailies_plot")), 
          footer = modalButton("Close")
        )
      )
    })
    
    output$dailies_plot <- renderPlotly({
      
      data <- input_data()
      
      plot_ly(data, x = ~Date, y = ~TotalAmount,
              type = "scatter", mode = "lines", name = NULL,
              hovertemplate = "%{x}<br>%{y:$.2f} USD<extra></extra>")  %>%
        layout(
          xaxis = list(
            title = list(text = NULL)
          ),
          yaxis = list(
            title = "Expenses"
          )
        )
    })
    
    }  
  )
}

## EXAMPLE USAGE
# 
# ui <- fluidPage(
#   DailyExpensesPopupUI("dailies")
# )
# 
# server <- function(input, output, session) {
#   DailyExpensesPopupServer("dailies", input_data = reactive({
#     expenses %>% group_by(Date) %>%
#       summarize(NumberOfExpenses = n(),
#                 TotalAmount = sum(Amount),
#                 AverageExpense = TotalAmount / NumberOfExpenses,
#                 .groups = "drop")
#   }))
# }
# 
# shinyApp(ui, server)