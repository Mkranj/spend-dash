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

date_transform_total <- finance_data$Date %>% strptime("%m/%d/%Y")

date_transform_ex <- expenses$Date %>% strptime("%m/%d/%Y")
expenses$date_transform <- date_transform_ex

# Not all dates are included - include the ones with no expense or income

all_dates <- seq(min(date_transform_total),
                 max(date_transform_total), "days")
all_dates <- data.frame(date_transform = all_dates)

expenses <- left_join(
  all_dates,
  expenses)

expenses$date_format <- expenses$date_transform %>% format(format = "%d.%m.%Y")

currency <- unique(finance_data$Currency)[1]
# FIX! something is in USD, recalculate it.

# App ----

start_date <- min(date_transform_total)
end_date <- max(date_transform_total)



ui <- fluidPage(
  # Setup theme ----
  theme = shinytheme("flatly"),
  chooseSliderSkin(color = "#abb1a1"),
  tags$head(tags$style(HTML("#filter_words {border-color: gray}"))),
  
  # UI controls ----
  fluidRow(
    column(9, sliderInput("date_considered", "Dates to show",
      min = start_date,
      max = end_date,
      value = c(start_date, end_date),
      timeFormat = "%d.%m.%Y", width = "100%")),
    column(3, "PLACEHOLDER BUTTONS")
  ),
  textInput("filter_words", "Filter expenses containing:",
            placeholder = "e.g. 'movie', 'drinks'..."),
  
  # UI plot ----
  plotlyOutput("main_plot_expenses", height = "330px"),
  
  # UI table and summary ----
  fluidRow(column(6, tableOutput("table_recent")),
           column(4, htmlOutput("expenses_summary"))
           ),
  fluidRow(column(6, radioButtons("table_sort_type", label = NULL, inline= T,
                                   choices = c("Most recent" = "recent",
                                               "Most expensive" = "expensive"))))
  
)

server <- function(input, output, session) {
  
  expenses_individual_data <- reactive({
    filtered_data <- expenses %>% 
      filter(date_transform >= input$date_considered[1] - days(1),
             date_transform <= input$date_considered[2])
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
    daily_data <- group_by(expenses_individual_data(), date_transform) %>% 
      summarise(expense = sum(Amount, na.rm = T), no_of_expenses = n())
    
    # Days with no expenses still have one row and n of 1
    daily_data$no_of_expenses[!(daily_data$expense > 0)] <- 0

    daily_data
  })
  
  plot_expenses <- reactive({
    plot_data <- expenses_daily_data()
    plot_data$date_format <- plot_data$date_transform %>% format(format = "%d.%m.%Y")
    
    # Function to pass as an argument, to round y-axis
    round_y_axis <- function(y) round(y) 
    max_daily_expense <- max(plot_data$expense)
    plot <- ggplot(plot_data, aes(x = as.Date(date_transform), y = expense,
                                  # text - specific aesthetic we can later use to create tooltips
                                  text = paste("Date:", date_format,
                                              "<br>Expenses: ", expense), 
                                  # group = 1 - needed if including text - otherwise geom_line tries to group by text and doesn't display anything!
                                  group = 1)) +
      scale_x_date(date_labels = "%m.%Y", date_breaks = "1 months") +
      xlab("Date") + ylab("Amount spent") + theme_minimal() +
      scale_y_continuous(limits = c(0, NA), 
                         expand = expansion(mult = c(0, 0.05), add = c(50,0)),
                         labels = round_y_axis, 
                         breaks = seq(0, max_daily_expense, by = 1000)) +
      theme(panel.grid.major = element_line(colour="grey"))
    if (nrow(expenses_daily_data()) == 1){
      plot <- plot + geom_point()
    } else {
      plot <- plot + geom_line()
    }
    plot <- plot %>% ggplotly(tooltip = c("text")) %>% config(displayModeBar = FALSE ) %>%
      layout(margin = list(t = 0, b = 50), xaxis = list(title = list(standoff = 0)))
    # margin seems to change after 50!
    plot
    }
  )
  
  output$main_plot_expenses <- renderPlotly(plot_expenses())
  
  expenses_summary_data <- reactive({
    total_expense <- sum(expenses_individual_data()$Amount, na.rm = T) %>% round(2)
    n_expenses <- expenses_individual_data() %>% filter(Amount > 0) %>% nrow()
    avg_expense <- (total_expense/n_expenses) %>% round(2)
    summary_data <- paste0("Total expenses: ", total_expense, currency,
           "<br/>Number of expenses: ", n_expenses,
           "<br/>Average expense: ", avg_expense, currency)
    paste0("<div style='border-style: groove;
           border-color: gray;
           border-radius: 5px;
           border-width: 2px;
           padding: 5px;
           font-size: 20px;'>", summary_data, "</div>")
  })
  
  output$expenses_summary <- renderUI(expenses_summary_data() %>% HTML())
  
  output$table_recent <- renderTable({
    table_data <- expenses_individual_data() %>% filter(!is.na(Amount))
    table_data$date_transform <- as.Date(table_data$date_transform) %>% as.character()
    if (input$table_sort_type == "recent") {
      table_data <- arrange(table_data, desc(date_transform))
      }
    else if (input$table_sort_type == "expensive") {
      table_data <- arrange(table_data, desc(Amount), desc(date_transform)) 
      }
    else print("Unknown arrange choice")
    table_data %>%
      select(Date = date_format, Amount, Note, Category) %>% head()
  })
}

shinyApp(ui, server)