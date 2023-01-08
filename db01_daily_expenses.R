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

# App ----
ui <- fluidPage(
  fluidRow(
    selectInput("color_by", "Highlight variable:",
                choices = c("None", "Weekend" = "weekend", 
                            "Got income that day" = "income"))
  ),
  fluidRow(
    plotOutput("expenses_plot")
  )
)

server <- function(input, output, session) {
  color_variable <- reactive({
    if (input$color_by == "weekend") {
      chosen_variable <- "is_weekend"
    } 
    if (input$color_by == "income") {
      chosen_variable <- "got_income"
    }
    if (input$color_by == "None") {
      chosen_variable <- NULL
    }
    chosen_variable
  })
  
  output$expenses_plot <- renderPlot({
    if (!is.null(color_variable())) {
    vector_color <- expense_income_daily[[color_variable()]] %>% as.factor()
    } else {
    vector_color <- NULL
    }
    ggplot(expense_income_daily, aes(date_formatted, expense)) + geom_line() +
      geom_point(aes(color = vector_color)) + 
      scale_color_discrete(type = c("grey", "red"))
  })
}

shinyApp(ui, server)