expenses_over_time_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(plotlyOutput(ns("expenses_plot"), height = "200px")),
    div(uiOutput(ns("view_buttons"))
        )
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
                months_btn)
      })
      
      observeEvent(input$lower_lvl, {
        current_view("Day")
      })
      
      observeEvent(input$higher_lvl, {
        current_view("Month")
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
              tickformat = "%d.%m.%y",
              title = list(text = NULL)
            ),
            yaxis = list(
              title = "Expenses",
              zeroline = F
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
              tickformat = "%m. %Y",
              # dtick so that every month is shown, and only once.
              # Otherwise, it would show up multiple times when only a few
              # months are in range.
              dtick = "M1",
              title = list(text = NULL)
            ),
            yaxis = list(
              title = "Expenses",
              zeroline = F
            )
          ) %>% config(displayModeBar = F)
        
        # If the plot has a single x value, lines won't be shown, so we need
        # to display points instead.
        if (nrow(plot_data) == 1) {
          plot_object <- plot_object %>% style(mode = "markers")
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