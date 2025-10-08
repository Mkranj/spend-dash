expenses_over_time_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(plotlyOutput(ns("expenses_plot"), height = "200px")),
    div(uiOutput(ns("view_buttons")))
  )
}

expenses_over_time_plotServer <- function(id, expenses_by_day, expenses_by_month) {
  moduleServer(
    id,
    function(input, output, session) {
      stopifnot(is.reactive(expenses_by_day))
      stopifnot(is.reactive(expenses_by_month))
      
      ns <- NS(id)
      
      current_view <- reactiveVal("Month")
      
      output$view_buttons <- renderUI({
        days_btn <- actionButton(ns("lower_lvl"), "Days")
        months_btn <- actionButton(ns("higher_lvl"), "Months")
        
        if (current_view() == "Day") {
          days_btn <- days_btn %>% tagAppendAttributes(
            class = "active-btnview"
          )
        } else if (current_view() == "Month") {
          months_btn <- months_btn %>% tagAppendAttributes(
            class = "active-btnview"
          )
        }
        
        if (current_view() == "Month" && enough_data_ma()) {
          trendline_btn <- checkboxInput(ns("trend_check"), 
                                         label = "Show trend",
                                         # Keep it consistent across showing/hiding
                                         value = isolate(show_trend())
                                         ) %>% 
            tagAppendAttributes(
              class = "checkmark-trend"
            )
          
          # The intervals allowed depend on data size
          ma_months <- c(3, 5, 7)
          
          choice_names <- paste0(ma_months, " months")
          offered_choices <- ma_months %>% setNames(choice_names) 
          
          trendline_period <- selectInput(ns("trend_period"),
                                          choices = offered_choices,
                                          selected = 7,
                                          label = "",
                                          selectize = F) %>% 
            tagAppendAttributes(class = "trend-select")
          
        } else {
          trendline_btn <- NULL
          trendline_period <- NULL
        }
        
        
        tagList(days_btn,
                months_btn,
                trendline_btn,
                trendline_period
        )
      })
      
      observeEvent(input$lower_lvl, {
        current_view("Day")
      })
      
      observeEvent(input$higher_lvl, {
        current_view("Month")
      })
      
      
      # Calculating three-month trends - only used if enough data is present - 3 months
      # TRUE/FALSE
      enough_data_ma <- reactive(nrow(expenses_by_month()) >= 3)
      
      show_trend <- reactiveVal(T)
      
      observeEvent(input$trend_check, {
        show_trend(input$trend_check)
      })
    
      
      monthly_ma <- reactive({
        req(enough_data_ma())
        
        data <- expenses_by_month()$TotalAmount
        
        # User chooses how much to smooth the lines
        req(input$trend_period)
        period <- as.numeric(input$trend_period)
        
        # rollapply to calculate means will also calculate border values,
        # e.g. first and last 3 dates for a 7-month window
        
        moving_averages <- zoo::rollapply(data,
                                          width = period,
                                          FUN = mean,
                                          align = "center",
                                          partial = TRUE)
        
        moving_averages
      })
      
      daily_plot <- reactive({
        plot_data <- expenses_by_day()
        
        plot_object <- plot_ly(plot_data, x = ~Date, y = ~TotalAmount,
                type = "scatter",
                mode = "lines",
                name = NULL,
                color = I("#3c8dbc"),
                hovertemplate = "%{x|%d.%m.%Y.}<br>Amount: %{y:.2f}<extra></extra>"
                ) %>%
          layout(
            xaxis = list(
              tickformat = "%d.%m.%Y.",
              title = list(text = NULL),
              fixedrange = TRUE
            ),
            yaxis = list(
              title = "Expenses",
              zeroline = F,
              fixedrange = TRUE
            )
          ) %>% config(displayModeBar = F)
        
        # If the plot has a single x value, lines won't be shown, so we need
        # to display points instead.
        if (nrow(plot_data) == 1) {
          plot_object <- plot_object %>% style(mode = "markers")
        }
        
        plot_object
      })
      
      monthly_plot <- reactive({
        plot_data <- expenses_by_month() %>%
          cbind(., Date = date_from_year_month(.$Year, .$Month))
        
        plot_object <- plot_ly(plot_data, x = ~Date, y = ~TotalAmount,
                type = "scatter",
                mode = "lines",
                name = NULL,
                color = I("#3c8dbc"),
                hovertemplate = "%{x|%m.%Y.}<br>Amount: %{y:.2f}<extra></extra>"
                ) %>%
          layout(
            xaxis = list(
              tickformat = "%m.%Y.",
              # dtick so that every month is shown, and only once.
              # Otherwise, it would show up multiple times when only a few
              # months are in range.
              dtick = "M1",
              title = list(text = NULL),
              fixedrange = TRUE
            ),
            yaxis = list(
              title = "Expenses",
              zeroline = F,
              fixedrange = TRUE
            )
          ) %>% config(displayModeBar = F)
        
        # If the plot has a single x value, lines won't be shown, so we need
        # to display points instead.
        if (nrow(plot_data) == 1) {
          plot_object <- plot_object %>% style(mode = "markers")
        }
        
        # Add trend line if needed
        if (enough_data_ma() && show_trend()) {
          mov_av <- monthly_ma()
          
          plot_object <- plot_object %>% 
            add_lines(y = mov_av,
                      color = I("#EB7259"), 
                      showlegend = F,
                      hovertemplate = NA,
                      line = list(dash = "dot")
                      )
        }
        
        plot_object
      })
      
      observe({
        if (current_view() == "Month") {
          output$expenses_plot <- renderPlotly(monthly_plot())
        } else if (current_view() == "Day") {
          output$expenses_plot <- renderPlotly(daily_plot())
        }
      })
      
    }
  )
}