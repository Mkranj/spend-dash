source("app_setup.R")
source("data_prep.R")

# UI definitions

sidebar <- dashboardSidebar(
  dateSelectUI("date_range", minDate = first_date, maxDate = last_date),
  DailyExpensesPopupUI("dailies")
)

header <- dashboardHeader(title = "SpendDash")

body <- dashboardBody(
  fluidRow(valueBoxOutput("vb_total_amount"),
           valueBoxOutput("vb_no_of_expenses")),
  fluidRow(plotlyOutput("monthly_plot") %>% box(width = 12)),
  fluidRow(dataTableOutput("monthly_data") %>% box(width = 12))
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
  
  expenses_by_day <- reactive({
    individual_expenses()  %>%
      group_by(Date) %>%
      summarize(NumberOfExpenses = n(),
                TotalAmount = sum(Amount, na.rm = T),
                AverageExpense = TotalAmount / NumberOfExpenses,
                .groups = "drop") %>%
      cover_all_dates_in_period() %>%
      # Fill in NA's after joining with 0's
      mutate(
        across(c(2:4),
               ~ ifelse(is.na(.x), 0, .x))
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
  
  output$vb_total_amount <- renderValueBox({
    valueBox(value = individual_expenses()$Amount %>% sum(na.rm = T) %>%
               round(2),
             subtitle = "Total amount spent")
  })
  
  output$vb_no_of_expenses <- renderValueBox({
    valueBox(value = individual_expenses() %>% nrow(),
             subtitle = "Number of different expenses")
  })
  
  DailyExpensesPopupServer("dailies", input_data = expenses_by_day)
  
}

shinyApp(ui, server)