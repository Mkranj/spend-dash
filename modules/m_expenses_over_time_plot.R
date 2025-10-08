expenses_over_time_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(plotlyOutput(ns("expenses_plot"), height = "200px")),
    div(uiOutput(ns("view_buttons"), inline = T),
        make_trendline_btn(ns("trend_check")),
        make_trendline_period_drop(ns("trend_period")))
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
        
        tagList(days_btn,
                months_btn
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
      
      
      observeEvent(input$trend_check,{
        shinyjs::toggleState("trend_period", condition = input$trend_check)
      })
      
      observeEvent(enough_data_ma(),{
        shinyjs::toggleElement("trend_check", condition = enough_data_ma())
        shinyjs::toggleElement("trend_period", condition = enough_data_ma())
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
                hovertemplate = "Amount: %{y:.2f}<extra></extra>"
                ) %>%
          layout(
            hovermode = "x unified",
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
        if (enough_data_ma() && input$trend_check) {
          mov_av <- monthly_ma()
          
          plot_object <- plot_object %>% 
            add_lines(y = mov_av,
                      color = I("#EB7259"), 
                      showlegend = F,
                      hovertemplate = "Trend: %{y:.2f}<extra></extra>",
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