library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(plotly)
library(DT)

# Strings detailing UI stylings held in this file
source("www/ui_customisation.R")

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

# UI functions:
dateButton <- function(id, label) {
  actionButton(inputId = id, label = label, class = "date_button")
}

arrow_transition_image <- p(img(src = 'Dark_blue_right_arrow.png',
                                width = "30%"),
                            style = arrow_transition_css)

# For HTML output, easily apply css to the string you want to display
style_div_output <- function(displayed_string, style_options = NULL) {
  div_statement <- paste0("<div style='",
                          style_options,
                          "'>", displayed_string, "</div>")
  div_statement
}

# UI: -----
ui <- fluidPage(
  # Setup theme ----
  theme = shinytheme("flatly"),
  chooseSliderSkin(color = "blue"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css",
                      href = "fonts_css.css")),
  
  # UI controls ----
  fluidRow(class = "header_row", style = "margin-bottom: 5px;",
           column(10,
               h1("EXPENSES DASHBOARD"), class = "header_title"),
           column(2, align = "right",
                  actionButton("btn_mode_daily", label = "Daily", class = "header_row mode_button", width = "60%"),
                  actionButton("btn_mode_monthly", label = "Monthly (WIP)", class = "header_row mode_button", width = "60%")
                  )
           ),
  fluidRow(
    column(3, textInput("filter_words", "Filter expenses containing:",
                        placeholder = "e.g. 'movie', 'drinks'...")),
    column(6,div(style = "margin-left: -50px; text-align: center;", 
                 sliderInput("date_considered", "Dates to show",
                  min = start_date,
                  max = end_date,
                  value = c(start_date, end_date),
                  timeFormat = "%d.%m.%Y", width = "100%"))),
    column(3, div(style = "margin-top: 40px;",
           dateButton("button_1week", label = "1W"),
           dateButton("button_1month", label = "1M"),
           dateButton("button_3months", label = "3M"),
           dateButton("button_1year", label = "1Y"),
           dateButton("button_all_time", label = "ALL"))
           )
  ),
  
  # UI plot ----
  plotlyOutput("main_plot_expenses", height = "310px"),
  
  # UI table and summary ----
  
  
  
  fluidRow(column(6, dataTableOutput("table_recent")),
           column(1, arrow_transition_image),
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
  
  # date_button functionalities ----
  
  dateButton_change_start_time <- function(no_time_units, slider_id, time_unit = "days"){
    new_start_date <- start_date
    if (time_unit == "days") {
      new_start_date <- end_date - days(no_time_units) + seconds(1)
    }
    if (time_unit == "months") {
      new_start_date <- end_date - months(no_time_units) + seconds(1)
    }
    if (time_unit == "years") {
      new_start_date <- end_date - years(no_time_units) + seconds(1)
    }
    updateSliderInput(inputId = slider_id,
                      value = c(new_start_date, end_date),
                      timeFormat = "%d.%m.%Y")
  }
  
  observeEvent(input$button_1week, {
    dateButton_change_start_time(6, "date_considered", "days")
  })
  
  observeEvent(input$button_1month, {
    dateButton_change_start_time(1, "date_considered", "months")
  })
  
  observeEvent(input$button_3months, {
    dateButton_change_start_time(3, "date_considered", "months")
  })
  
  observeEvent(input$button_1year, {
    dateButton_change_start_time(1, "date_considered", "years")
  })
  
  observeEvent(input$button_all_time, {
    dateButton_change_start_time(1, "date_considered", "all")
    clicked_day(NULL)
  })
  
  # Making plots ----
  
  plot_expenses_gg <- reactive({
    plot_data <- expenses_daily_data()
    plot_data$date_format <- plot_data$date_transform %>%
      format(format = "%d.%m.%Y")
    
    # Function to pass as an argument, to round y-axis
    round_y_axis <- function(y) round(y) 
    max_daily_expense <- max(plot_data$expense)
    plot <- ggplot(plot_data,
                   aes(x = as.Date(date_transform, "%d.%m.%Y", tz = "NZ"),
                       y = expense,
                       # text - specific aesthetic we can later use
                       # to create tooltips
                       text = paste("Date:", date_format,
                                    "<br>Expenses: ", expense), 
                       # group = 1 - needed if including text - otherwise 
                       # geom_line tries to group by text
                       # and doesn't display anything!
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
      plot <- plot + geom_line(color = "#151759")
    }
    plot
  }
  )
  # If it's clicked on the same spot, it doesn't update by default!
  # Priority=event makes evaluation go off on every click.
  plotly_clicks <- reactive({
    data <- event_data(event = "plotly_click", source = "A", priority = "event")
    data
  })
  # plotly_clicks()$pointNumber is the row of the df, starting with 0
  # With the additional geom_point() in modifiers, clicking on that dot
  #  gives curveNumber 1
  
  clicked_day <- reactiveVal(NULL)
  
  set_clicked_point <- observeEvent(plotly_clicks(), {
    # Unselect point if clicked on the same day
      point_clicked_highlighted <- plotly_clicks()$curveNumber
      
      point_data_no <- plotly_clicks()$pointNumber + 1
      point_data <- expenses_daily_data()[point_data_no, "date_transform"][[1]]
      # %>% format(format = "%d.%m.%Y")
      if (!is.null(clicked_day())) {
        if (clicked_day() != point_data) {
          clicked_day(point_data)
        } else {
          clicked_day(NULL)
        }
      } else {
        clicked_day(point_data)
      }
      
      if (point_clicked_highlighted == 1) {
        clicked_day(NULL)
      }
  })

  # If anything changes the dates considered, nullify clicked point
  observeEvent(input$date_considered, {
    clicked_day(NULL)
  })
  
  plot_expenses_gg_modifiers <- reactive({
    plot <- plot_expenses_gg()
    plot_data <- expenses_daily_data()
    plot_data$date_format <- plot_data$date_transform %>%
      format(format = "%d.%m.%Y")
    
    if (!is.null(clicked_day())) {
      plot <- plot + 
        geom_point(data = filter(plot_data, date_transform == clicked_day()),
                                size = 3,color = "#151759")
      plot
      } else {
      plot
    }
  })
    
  plot_expenses_to_plotly <- reactive({
    plot <- plot_expenses_gg_modifiers()
    plot <- plot %>% ggplotly(tooltip = c("text"), source = "A") %>% 
      config(displayModeBar = FALSE ) %>%
      layout(margin = list(t = 0, b = 50), font = list(family = "Lato"),
             xaxis = list(title = list(text = NULL, standoff = 0),
                          fixedrange = T),
             yaxis = list(fixedrange = T))
    # margin changes after value 50
    plot
  })
    
  
  output$main_plot_expenses <- renderPlotly(plot_expenses_to_plotly())
  
  expenses_summary_data <- reactive({
    all_expenses <- expenses_individual_data()
    if (!is.null(clicked_day())) {
      all_expenses <- filter(all_expenses, date_transform == clicked_day())
    }
    
    total_expense <- sum(all_expenses$Amount, na.rm = T) %>% round(2)
    n_expenses <- all_expenses %>% filter(Amount > 0) %>% nrow()
    avg_expense <- (total_expense/n_expenses) %>% round(2)
    summary_data <- paste0("Total expenses: ", total_expense, " ", currency,
           "<br/>Number of expenses: ", n_expenses,
           "<br/>Average expense: ", avg_expense, " ", currency)
    style_div_output(summary_data, summary_div_style)
  })
  
  output$expenses_summary <- renderUI(expenses_summary_data() %>% HTML())
  
  output$table_recent <- renderDataTable({
    table_data <- expenses_individual_data() %>% filter(!is.na(Amount))
    table_data$date_transform <- as.Date(table_data$date_transform) %>%
      as.character()
    if (input$table_sort_type == "recent") {
      table_data <- arrange(table_data, desc(date_transform))
      }
    else if (input$table_sort_type == "expensive") {
      table_data <- arrange(table_data, desc(Amount), desc(date_transform)) 
      }
    else print("Unknown arrange choice")
    if (!is.null(clicked_day())) {
      table_data <- filter(table_data, date_transform == clicked_day() %>%
                             as.Date() %>% as.character())
    }
    table_data %>%
      select(Date = date_format, Amount, Note, Category)
  },
  options = list(info = F, paging = F, searching = F, scrollY = "150px",
                 initComplete = table_ind_expenses_css)
  )
}

shinyApp(ui, server)