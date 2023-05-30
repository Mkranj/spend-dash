expand_data_to_future <- function(input_data, duplicate_times = 2,
                                  max_advancement_days = 365) {
  data <- input_data
  initial_rows <- nrow(data)
  
  new_data <- data %>% slice_sample(prop = duplicate_times - 1, replace = T)
  
  advances_in_days <- runif(nrow(new_data), 1, max_advancement_days) %>% round()
  
  new_data$Date <- new_data$Date + days(advances_in_days)
  
  rbind(data, new_data)
}

