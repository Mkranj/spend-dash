categories_barchart_UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("categories_bar"), height = "180px")
    )
}

categories_barchart_Server <- function(id, 
                                       individual_expenses,
                                       number_of_months,
                                       categories_exist = reactive(T)
                                       ) {
  moduleServer(
    id,
    function(input, output, session) {
      stopifnot(is.reactive(individual_expenses))
      stopifnot(is.reactive(number_of_months))
      stopifnot(is.reactive(categories_exist))

      plot_data <- reactive({
        req(individual_expenses())
        
        plot_data <- individual_expenses() %>%
          cover_all_dates_in_period() %>%
          group_by(Category) %>% 
          # Added empty dates resulted in NA categories
          filter(!is.na(Category)) %>%
          summarise(Amount = sum(Amount) %>% round(),
                    Monthly_amount = (Amount / number_of_months()) %>% round(),
                    No_expenses = n(), .groups = "drop")
      })
      
      plot_object <- reactive({
        req(plot_data())
        
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
      
      output$categories_bar <- renderPlotly({
        if (categories_exist()) {
          plot_object()
        } else {
          NULL
        }
      })
      
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