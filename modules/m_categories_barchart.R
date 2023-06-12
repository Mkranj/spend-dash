categories_barchart_UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("categories_bar"), height = "180px")
    )
}

categories_barchart_Server <- function(id, individual_expenses) {
  moduleServer(
    id,
    function(input, output, session) {
      stopifnot(is.reactive(individual_expenses))
      
      no_of_months <- reactive({
        individual_expenses() %>% group_by(year(Date), month(Date)) %>% 
          summarise(N = n()) %>% nrow()
      })
      
      plot_data <- reactive({
        plot_data <- individual_expenses() %>%
          group_by(Category) %>% 
          summarise(Amount = sum(Amount) %>% ceiling(),
                    Monthly_amount = (Amount / no_of_months()) %>% ceiling(),
                    No_expenses = n(), .groups = "drop")
      })
      
      plot_object <- reactive({
        req(plot_data)
        
        plot_ly(plot_data()) %>% add_bars(x = ~Category, y = ~Monthly_amount) %>%
          layout(
            xaxis = list(
              categoryorder = "total descending",
              title = list(text = "Monthly Average")
            ),
            yaxis = list(
              title = "Expenses",
              zeroline = F
            )
          ) %>% config(displayModeBar = F)
      })
      
      output$categories_bar <- renderPlotly(plot_object())
      
    }
  )
}

# Example
# ui <- fluidPage(
#   categories_barchart_UI("ex")
# )
# 
# server <- function(input, output, session) {
#   categories_barchart_Server("ex", reactive(expenses))
# }
# 
# shinyApp(ui, server)