expenses_over_time_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(plotlyOutput(ns("expenses_plot"), height = "200px")),
    div(actionButton(ns("lower_lvl"), "Days"),
             actionButton(ns("higher_lvl"), "Months"))
  )
}

expenses_over_time_plotServer <- function(id, expenses_by_day, expenses_by_month) {
  moduleServer(
    id,
    function(input, output, session) {
      stopifnot(is.reactive(expenses_by_day))
      stopifnot(is.reactive(expenses_by_month))
      
      current_view <- reactiveVal("Month")
      
      observeEvent(input$lower_lvl, {
        current_view("Day")
      })
      
      observeEvent(input$higher_lvl, {
        current_view("Month")
      })
      
      daily_plot <- reactive({
        plot_data <- expenses_by_day()
        
        plot_ly(plot_data, x = ~Date, y = ~TotalAmount,
                type = "scatter", mode = "lines", name = NULL,
                hovertemplate = "%{x}<br>%{y:$.2f} USD<extra></extra>")  %>%
          layout(
            xaxis = list(
              tickformat = "%b %Y",
              dtick = "M1",
              tick0 = "2000-01-01",
              title = list(text = NULL)
            ),
            yaxis = list(
              title = "Expenses",
              zeroline = F
            )
          ) %>% config(displayModeBar = F)
      })
      
      monthly_plot <- reactive({
        plot_data <- expenses_by_month() %>%
          cbind(., Date = date_from_year_month(.$Year, .$Month))
        
        plot_ly(plot_data, x = ~Date, y = ~TotalAmount,
                type = "scatter", mode = "lines", name = NULL,
                hovertemplate = "%{x|%m.%Y.}<br>Amount: %{y:.2f}<extra></extra>")  %>%
          layout(
            xaxis = list(
              tickformat = "%b %Y",
              dtick = "M1",
              tick0 = "2000-01-01",
              title = list(text = NULL)
            ),
            yaxis = list(
              title = "Expenses",
              zeroline = F
            )
          ) %>% config(displayModeBar = F)
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