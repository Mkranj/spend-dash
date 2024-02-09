source("scripts/app_setup.R")

source("ui_definition.R")

server <- function(input, output, session) {
  
   
  # Data ----
  
  # Initial data - read from file. ReactiveVal that can be updated by user.
  expenses_data <- reactiveVal(
    # Initial value - read from .csv
    expenses
  )
  
  # Value that dictates whether category-relevant content should be shown.
  categories_exist <- reactiveVal(T)
  
  observeEvent(categories_exist(), {
    if (categories_exist()) {
      shinyjs::showElement("categories_bar_box")
    } else {
      shinyjs::hideElement("categories_bar_box")
    }
  })
  
  data_first_date <- reactive({
    expenses_data()$Date %>% min(na.rm = T)
  })
  
  data_last_date <- reactive({
    expenses_data()$Date %>% max(na.rm = T)
  })
  
  # Extract how many months are in the selected datespan.
  # We need to pass this to modules that don't interact with date_range()
  num_selected_months <- reactive({
    expenses_data() %>%
      cover_all_dates_in_period(
        start = date_range()$start %>% as_date(),
        end = date_range()$end %>% as_date()
        ) %>%
      group_by(year(Date), month(Date)) %>% 
      summarise(N = n(), .groups = "drop") %>% 
      nrow()
  })

  # Setup values for the datepicker based on dates found in data
  date_range <- dateSelectServer("date_range", 
                                 minDate = data_first_date,
                                 maxDate = data_last_date)
  
  individual_expenses <- reactive({
    req(expenses_data())
    data <- expenses_data() %>% filter(
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
    if (categories_exist()) {
      expenses_data() %>% pull(Category) %>% unique() %>% sort()
    } else {
      # Empty vector indicating no categories
      character(0)
    }
  })
  
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
  
  # Uploading custom data ----
  # Message to be displayed if an error is caught. NULL to hide.
  upload_error_msg <- reactiveVal(NULL)
  
  observeEvent(input$user_sent_data, {
    file_location <- input$user_sent_data$datapath
    
    upload_success <- F
    
    tryCatch({
        imported_data <- load_and_prepare_data(file_location)
        upload_success <- T
      },
      error = function(e) {
        error_msg <- e$message
        
        # User-friendly reporting of common data format issues
        if (error_msg == "Error: 'Date' column not found") {
          user_msg <- "Warning! The data you uploaded doesn't have the required columns. 
            Please adjust it so it resembles the picture above."
          
          upload_error_msg(user_msg)
          return(NULL)
        }

        if (error_msg == "Error: 'Amount' column not found") {
          user_msg <- "Warning! The data you uploaded doesn't have the required columns. 
            Please adjust it so it resembles the picture above."
          
          upload_error_msg(user_msg)
          return(NULL)
        }
        
        # Date is not in any recognised format, or is datetime:
        if (stri_detect_fixed(error_msg, "failed to parse")) {
          user_msg <- "Warning! The 'Date' column in the data cannot be read as proper dates. 
            Please ensure it's written in a common format like '12.12.2023' 
            and doesn't include hours, minutes, seconds."
          
          upload_error_msg(user_msg)
          return(NULL)
        }
        
        # Unexpected error
        upload_error_msg(error_msg)
      }
    )
    
    # Don't proceed if the file didn't upload correctly. Don't close the popup.
    if (!upload_success) {
      return(NULL)
    }
    
    # Cleanup possible leftover error messages
    upload_error_msg(NULL)
    
    new_dataframe <- imported_data$data
    new_available_columns <- imported_data$detected_columns
    
    # Update main data source
    expenses_data(new_dataframe)
    
    # Update variable indicating whether categories are present in data
    data_has_categories <- new_available_columns$Category
    categories_exist(data_has_categories)
    
    # Close the popup
    removeModal()
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
  
  upload_popup_error_Server("data_upload", 
                            error_text = upload_error_msg)
  
  categories_barchart_Server("categories_plot",
                             individual_expenses, 
                             number_of_months = num_selected_months,
                             categories_exist = categories_exist)
  
  observeEvent(input$upload_new, {
    # A new popup will be opened - don't show any warnings on fresh open
    upload_error_msg(NULL)
    
    showModal(uploading_modal_ui)
  })
  
  observeEvent(input$cancel_upload, {
    # Dismiss any warnings
    upload_error_msg(NULL)
    
    removeModal()
  })
  
}

shinyApp(ui, server)