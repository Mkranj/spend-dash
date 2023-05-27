source("app_setup.R")
source("data_prep.R")

ui <- fluidPage(
  dataTableOutput("monthly_data")
)

server <- function(input, output, session) {
  
  individual_expenses <- reactive({
    expenses
  })
  
  expenses_by_month <- reactive({
    individual_expenses() %>%
      group_by(Year = year(Date), Month = month(Date)) %>%
      summarize(NumberOfExpenses = n(),
                TotalAmount = sum(Amount),
                AverageExpense = TotalAmount / NumberOfExpenses) 
  })
  
  output$monthly_data <- renderDataTable({
    datatable(expenses_by_month()) %>%
      formatRound(columns = c("TotalAmount", "AverageExpense"), digits = 2)
  })
}

shinyApp(ui, server)