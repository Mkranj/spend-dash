source("scripts/app_setup.R")
source("scripts/data_prep.R")

# UI definitions

sidebar <- dashboardSidebar(
  dateSelectUI("date_range", minDate = first_date, maxDate = last_date)
)

header <- dashboardHeader(title = "SpendDash")

body <- dashboardBody(
  includeCSS("www/styling.css"),
  fluidRow(valueBoxOutput("vb_total_amount", width = 6),
           valueBoxOutput("vb_no_of_expenses", width = 6)),
  fluidRow(expenses_over_time_plotUI("expenses_plot") %>% box(width = 12)),
  fluidRow(dataTableOutput("monthly_data") %>% box(width = 12))
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  
  # Module outputs ----
  date_range <- dateSelectServer("date_range", 
                                 minDate = first_date, maxDate = last_date)
  
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
  
  expenses_over_time_plotServer("expenses_plot", expenses_by_day = expenses_by_day,
                                expenses_by_month = expenses_by_month)
  
  output$vb_total_amount <- renderValueBox({
    valueBox(value = individual_expenses()$Amount %>% sum(na.rm = T) %>%
               round(2),
             subtitle = "Total amount spent")
  })
  
  output$vb_no_of_expenses <- renderValueBox({
    valueBox(value = individual_expenses() %>% nrow(),
             subtitle = "Number of different expenses")
  })
  
  DailyExpensesPopupServer("dailies", expenses_by_day)
  
}

shinyApp(ui, server)