source("app_setup.R")
source("data_prep.R")

# UI definitions

sidebar <- dashboardSidebar(
  dateSelectUI("date_range", minDate = first_date, maxDate = last_date)
)

header <- dashboardHeader(title = "EXPENSES DASHBOARD",
                          titleWidth = "27rem")

body <- dashboardBody(
  fluidRow(dataTableOutput("monthly_data") %>% box(width = 12)),
  fluidRow(plotlyOutput("monthly_plot") %>% box(width = 12))
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  
  # Module outputs ----
  date_range <- dateSelectServer("date_range")
  
  # Data ----
  individual_expenses <- reactive({
    expenses %>% filter(
      Date >= date_range()$start,
      Date <= date_range()$end,
    )
  })
  
  expenses_by_month <- reactive({
    individual_expenses() %>%
      group_by(Year = year(Date), Month = month(Date)) %>%
      summarize(NumberOfExpenses = n(),
                TotalAmount = sum(Amount),
                AverageExpense = TotalAmount / NumberOfExpenses,
                .groups = "drop") 
  })
  
  # Outputs ----
  output$monthly_data <- renderDataTable({
    datatable(expenses_by_month()) %>%
      formatRound(columns = c("TotalAmount", "AverageExpense"), digits = 2)
  })
  
  output$monthly_plot <- renderPlotly({
    # We're adding a Date column from individual years and months
    plot_data <- expenses_by_month() %>%
      cbind(., Date = date_from_year_month(.$Year, .$Month))
  
    plot_ly(plot_data, x = ~Date, y = ~TotalAmount,
            type = "scatter", mode = "lines", name = NULL,
            hovertemplate = "%{x}<br>%{y:$.2f} USD<extra></extra>")  %>%
      layout(
        xaxis = list(
          tickformat = "%b %Y",
          dtick = "M1",
          tick0 = "2000-01-01",
          title = list(text = NULL)
        ),
        yaxis = list(
          title = "Expenses"
        )
      )
  })
  
}

shinyApp(ui, server)