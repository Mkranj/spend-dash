source("app_setup.R")
source("data_prep.R")

ui <- fluidPage(
  fluidRow(style = "margin-bottom: 1em;",
           h1("EXPENSES DASHBOARD")
          ),
  dataTableOutput("monthly_data"),
  plotlyOutput("monthly_plot")
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
                AverageExpense = TotalAmount / NumberOfExpenses,
                .groups = "drop") 
  })
  
  output$monthly_data <- renderDataTable({
    datatable(expenses_by_month()) %>%
      formatRound(columns = c("TotalAmount", "AverageExpense"), digits = 2)
  })
  
  output$monthly_plot <- renderPlotly({
    #browser()
    # We're adding a Date column from individual years and months
    plot_data <- expenses_by_month() %>%
      cbind(., Date = date_from_year_month(.$Year, .$Month))
  
    plot_ly(plot_data, x = ~Date, y = ~TotalAmount,
            type = "scatter", mode = "lines") 
  })
  
}

shinyApp(ui, server)