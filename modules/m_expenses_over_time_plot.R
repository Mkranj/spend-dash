expenses_over_time_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("expenses_plot"))
  )
}

expenses_over_time_plotServer <- function(id, expenses_by_month) {
  moduleServer(
    id,
    function(input, output, session) {
      stopifnot(is.reactive(expenses_by_month))
      
      output$expenses_plot <- renderPlotly({
        # We're adding a Date column from individual years and months
        plot_data <- expenses_by_month() %>%
          cbind(., Date = date_from_year_month(.$Year, .$Month))
        
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
              title = "Expenses"
            )
          )
      })
      
      
    }
  )
}