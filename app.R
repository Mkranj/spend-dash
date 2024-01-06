source("scripts/app_setup.R")
source("scripts/data_prep.R")

# UI definitions

sidebar <- dashboardSidebar(
  actionButton("upload_new", "Read data from file", width = "90%"),
  dateSelectUI("date_range", minDate = first_date, maxDate = last_date),
  uiOutput("categories_ui")
)

header <- dashboardHeader(title = "SpendDash")

body <- dashboardBody(
  includeCSS("www/styling.css"),
  fluidRow(valueBoxOutput("vb_total_amount", width = 4),
           valueBoxOutput("vb_average_monthly_expense", width = 4),
           valueBoxOutput("vb_three_month_average", width = 4)),
  fluidRow(expenses_over_time_plotUI("expenses_plot") %>% box(width = 12)),
  fluidRow(categories_barchart_UI("categories_plot") %>% box(width = 12))
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
    data <- expenses %>% filter(
      Date >= date_range()$start,
      Date <= date_range()$end,
    )
    
    if (categories_exist()) {
      # If all checkboxes are unselected, show data for everything.
      if (!is.null(input$categories_filtered)) {
        data <- data %>% filter(
          Category %in% input$categories_filtered
        )
      }
    }
    
    # Special case: No rows matching filters. Make a single row with 0 spending
    # so other components can still consume a dataframe
    if (nrow(data) == 0) {
      data[1, ] <- NA
      data$Date <- date_range()$start
      data$Category <- "(none)"
      data$Amount <- 0
    }
    data
  })
  
  expenses_by_day <- reactive({
    req(individual_expenses())
    individual_expenses()  %>%
      group_by(Date) %>%
      summarize(NumberOfExpenses = n(),
                TotalAmount = sum(Amount, na.rm = T),
                AverageExpense = TotalAmount / NumberOfExpenses,
                .groups = "drop") %>%
      cover_all_dates_in_period(
        start = date_range()$start %>% as_date(),
        end = date_range()$end %>% as_date()
      ) %>%
      # Fill in NA's after joining with 0's
      mutate(
        across(c(2:4),
               ~ ifelse(is.na(.x), 0, .x))
      )
  })
  
  expenses_by_month <- reactive({
    req(individual_expenses())
    
    individual_expenses() %>%
      group_by(Year = year(Date), Month = month(Date)) %>%
      summarize(NumberOfExpenses = n(),
                TotalAmount = sum(Amount),
                AverageExpense = TotalAmount / NumberOfExpenses,
                .groups = "drop") %>%
      # Re-introduce the Date variable with 01 as the day
      mutate(Date = paste0(Year, "-", Month, "-01") %>%
               as_date()
      ) %>%
      cover_all_months_in_period(
        start = date_range()$start %>% as_date(),
        end = date_range()$end %>% as_date()
      ) 
  })
  
  # Single values determining averages
  average_monthly_expense <- reactive({
    expenses_by_month() %>% summarise(average = mean(TotalAmount)) %>% 
      pull(average)  
  })
  
  average_three_month_expenses <- reactive({
    expenses_by_month() %>% slice_tail(n = 3) %>% 
      summarise(average = mean(TotalAmount)) %>% 
      pull(average)  
  })
  
  # Categories found in data
  
  existing_categories <- reactive({
    categories_exist(T)
    expenses %>% pull(Category) %>% unique() %>% sort()
  })
  
  categories_exist <- reactiveVal(F)
  
  output$categories_ui <- renderUI({
    req(existing_categories())
    checkboxTag <- checkboxGroupInput("categories_filtered", "Categories",
                       choices = existing_categories(),
                       selected = NULL,
                       inline = T
    )
    
    msg <- p("Check to show ONLY selected categories.")
    
    # We want the message to show up between the title and 
    # individual categories
    finalTag <- htmltools::tagQuery(checkboxTag)$
      find(".control-label")$
      after(msg)$
      allTags()
    
    finalTag
  })
  
  
  # Outputs ----
  expenses_over_time_plotServer("expenses_plot", expenses_by_day = expenses_by_day,
                                expenses_by_month = expenses_by_month)
  
  output$vb_total_amount <- renderValueBox({
    req(individual_expenses())
    valueBox(value = individual_expenses()$Amount %>% sum(na.rm = T) %>%
               round(0),
             subtitle = "Total amount spent")
  })
  
  output$vb_average_monthly_expense <- renderValueBox({
    valueBox(value = average_monthly_expense() %>% round(),
             subtitle = "Average expenses per month")
  })
  
  output$vb_three_month_average <- renderValueBox({
    valueBox(value = span(three_month_avg_icon(),
                          average_three_month_expenses() %>% round()),
             subtitle = "Three-month average expenses")
  })
  
  three_month_avg_icon <- reactive({
    if (average_three_month_expenses() > average_monthly_expense()) {
      return(icon("arrow-up", class = "increased-expenses"))
    } else if (average_three_month_expenses() < average_monthly_expense()) {
      return(icon("arrow-down", class = "lowered-expenses"))
    } else return(NULL)
  })
  
  categories_barchart_Server("categories_plot", individual_expenses)
}

shinyApp(ui, server)