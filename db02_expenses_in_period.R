library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(plotly)

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

expenses <- left_join(
  all_dates,
  expenses)

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
  theme = shinytheme("flatly"),
  chooseSliderSkin(color = "#abb1a1"),
  tags$head(tags$style(HTML("#filter_words {border-color: gray}"))),
  sidebarLayout(
    sidebarPanel(sliderInput("date_considered", "Dates to show",
                             min = start_date,
                             max = end_date,
                             value = c(start_date, end_date),
                             timeFormat = "%F"),
                 selectInput("table_sort_type", "Show expenses:",
                             choices = c("Most recent" = "recent",
                                         "Most expensive" = "expensive")
                             ),
                 tableOutput("table_recent")),
    mainPanel(plotlyOutput("main_plot_expenses", height = "600px"),
              column(6, textInput("filter_words", "Filter expenses containing:",
                      placeholder = "e.g. 'movie', 'drinks'...")),
              column(6, htmlOutput("expenses_summary"))
              ),
    position = "right"
  )
)

server <- function(input, output, session) {
  
  expenses_individual_data <- reactive({
    filtered_data <- expenses %>% 
      filter(date_formatted >= input$date_considered[1],
             date_formatted <= input$date_considered[2])
    # Filter by input words, if any matches, update data
    matching_filter <- grep(pattern = input$filter_words,
                            x = filtered_data$Note, ignore.case = T)
    
    # Some are blank, some are NA -- by default searches for "" string,
    # which doesn't match NA. But "" should include everything
    if (length(matching_filter) > 0 & input$filter_words != "") {
      filtered_data <- filtered_data[matching_filter, ]
      output$expenses_summary <- renderUI(expenses_summary_data() %>% HTML())
    } else if (input$filter_words != "") {
      output$expenses_summary <- renderUI(
        paste("NO MATCHES FOUND<br/>", expenses_summary_data()) %>% HTML())
    } else if (input$filter_words == "") {
      output$expenses_summary <- renderUI(expenses_summary_data() %>% HTML())
    }
    
    filtered_data
  })
  
  expenses_daily_data <- reactive({
    daily_data <- group_by(expenses_individual_data(), date_formatted) %>% 
      summarise(expense = sum(Amount, na.rm = T), no_of_expenses = n())
    
    # Days with no expenses still have one row and n of 1
    daily_data$no_of_expenses[!(daily_data$expense > 0)] <- 0
    
    filtered_data <- daily_data %>% 
      filter(date_formatted >= input$date_considered[1],
             date_formatted <= input$date_considered[2]) 
    filtered_data
  })
  
  plot_expenses <- reactive({
    plot_data <- expenses_daily_data()
    
    # Function to pass as an argument, to round y-axis
    round_y_axis <- function(y) round(y) 
    
    plot <- ggplot(plot_data, aes(as.Date(date_formatted), expense)) +
      scale_x_date(date_labels = "%m-%Y", date_breaks = "1 months") +
      xlab("Date") + ylab("Amount spent") + theme_linedraw() +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)), labels = round_y_axis)
    if (nrow(expenses_daily_data()) == 1){
      plot <- plot + geom_point()
    } else {
      plot <- plot + geom_line()
    }
    plot %>% ggplotly()
    }
  )
  
  output$main_plot_expenses <- renderPlotly(plot_expenses())
  
  expenses_summary_data <- reactive({
    total_expense <- sum(expenses_individual_data()$Amount, na.rm = T) %>% round(2)
    n_expenses <- expenses_individual_data() %>% filter(Amount > 0) %>% nrow()
    avg_expense <- (total_expense/n_expenses) %>% round(2)
    paste0("Total expenses: ", total_expense, currency,
           "<br/>Number of expenses: ", n_expenses,
           "<br/>Average expense: ", avg_expense, currency)
  })
  
  output$expenses_summary <- renderUI(expenses_summary_data() %>% HTML())
  
  # Change to individual expenses, not daily
  output$table_recent <- renderTable({
    table_data <- expenses_individual_data() %>% filter(!is.na(Amount))
    table_data$date_formatted <- as.Date(table_data$date_formatted) %>% as.character()
    if (input$table_sort_type == "recent") {
      table_data <- arrange(table_data, desc(date_formatted))
      }
    else if (input$table_sort_type == "expensive") {
      table_data <- arrange(table_data, desc(Amount), desc(date_formatted)) 
      }
    else print("Unknown arrange choice")
    table_data %>%
      select(date_formatted, Amount, Note) %>% head()
  })
}

shinyApp(ui, server)