# Buttons for selecting trend ----
make_trendline_btn <- function(id) {
  trendline_btn <- checkboxInput(id, 
                                 label = "Show trend",
                                 # Keep it consistent across showing/hiding
                                 value = T
  ) %>% 
    tagAppendAttributes(
      class = "checkmark-trend"
    )
  
  trendline_btn
}

make_trendline_period_drop <- function(id) {
  ma_months <- c(3, 5, 7)
  
  choice_names <- paste0(ma_months, " months")
  offered_choices <- ma_months %>% setNames(choice_names) 
  
  trendline_period <- selectInput(id,
                                  choices = offered_choices,
                                  selected = 7,
                                  label = "",
                                  selectize = F) %>% 
    tagAppendAttributes(class = "trend-select")
  
  trendline_period
}
