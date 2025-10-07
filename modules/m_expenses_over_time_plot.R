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
          
        } else {
          trendline_btn <- NULL
        }
        
        
        tagList(days_btn,
                months_btn,
                trendline_btn
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
        
        moving_averages <- forecast::ma(data, 3)
        # For 3rd order MA, the first and last observation don't have 3-point
        # estimates
        
        # We shall calculate these trend points as weighted estimates with the
        # earliest / latest observation having more weight
        no_obs <- length(data)
        
        first_tp <- mean(c(rep(data[1], 2),
                           data[2]
                           ))
        last_tp <- mean(c(data[no_obs-1],
                          rep(data[no_obs], 2)
                          ))
        
        moving_averages[1] <- first_tp
        moving_averages[no_obs] <- last_tp
        
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