library(tidyverse)
library(shiny)
library(lubridate)

# Condense income and expenses per day
finance_data <- read.csv("expense_data_1.csv")
finance_data <- select(finance_data, !c(Subcategory, Account.1, Note.1, INR))

expenses <- filter(finance_data, Income.Expense == "Expense")
income <- filter(finance_data, Income.Expense == "Income")

date_formatted_total <- finance_data$Date %>% strptime("%m/%d/%Y")

date_formatted_ex <- expenses$Date %>% strptime("%m/%d/%Y")
expenses$date_formatted <- date_formatted_ex
date_formatted_in <- income$Date %>% strptime("%m/%d/%Y")
income$date_formatted <- date_formatted_in

expenses_daily <- group_by(expenses, date_formatted) %>% 
  summarise(expense = sum(Amount), no_of_expenses = n())
income_daily <- group_by(income, date_formatted) %>% 
  summarise(income = sum(Amount), no_of_incomes = n())

# Not all dates are included - include the ones with no expense or income

all_dates <- seq(min(date_formatted_total),
                 max(date_formatted_total), "days")
all_dates <- data.frame(date_formatted = all_dates)

expense_income_daily <- left_join(
  all_dates,
  expenses_daily) %>%
  left_join(income_daily)

expense_income_daily$expense[is.na(expense_income_daily$expense)] <- 0
expense_income_daily$income[is.na(expense_income_daily$income)] <- 0
expense_income_daily$no_of_expenses[is.na(expense_income_daily$no_of_expenses)] <- 0
expense_income_daily$no_of_incomes[is.na(expense_income_daily$no_of_incomes)] <- 0

expense_income_daily <- expense_income_daily %>% 
  mutate(day_of_week = wday(date_formatted, week_start = 1) %>% as.factor(),
         is_weekend = case_when(
           as.numeric(day_of_week) < 6 ~ 0, as.numeric(day_of_week) > 5 ~ 1),
         expense_income_daily, got_income = case_when(
           income > 0 ~ 1, TRUE ~ 0))

currency <- unique(finance_data$Currency)[1]
# FIX! something is in USD, recalculate it.

# App ----
# Hovering over plot -> show precise date and amount

start_date <- min(expense_income_daily$date_formatted)
end_date <- max(expense_income_daily$date_formatted)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(sliderInput("date_considered", "Dates to show",
                             min = start_date,
                             max = end_date,
                             value = c(start_date, end_date))),
    mainPanel(plotOutput("main_plot_expenses", height = "600px"),
              verbatimTextOutput("expenses_summary")),
    position = "right"
  )
)

server <- function(input, output, session) {
  expenses_data <- reactive({
    filtered_data <- expense_income_daily %>% 
      filter(date_formatted >= input$date_considered[1],
             date_formatted <= input$date_considered[2]) 
    filtered_data
  })
  
  plot_expenses <- reactive({
    plot_data <- expenses_data()
    ggplot(plot_data, aes(as.Date(date_formatted), expense)) + geom_line() +
      scale_x_date(date_labels = "%m-%Y")}
  )
  
  output$main_plot_expenses <- renderPlot(plot_expenses())
  
  expenses_summary_data <- reactive({
    total_expense <- sum(expenses_data()$expense, na.rm = T) %>% round(2)
    n_expenses <- expenses_data() %>% filter(expense > 0) %>% nrow()
    avg_expense <- (total_expense/n_expenses) %>% round(2)
    paste0("Total expenses: ", total_expense, currency,
           "\nNumber of expenses: ", n_expenses,
           "\nAverage expense: ", avg_expense, currency)
  })
  
  output$expenses_summary <- renderText(expenses_summary_data())
}

shinyApp(ui, server)