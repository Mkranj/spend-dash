cover_all_dates_in_period <- function(data) {
  right_join(data,
             data.frame(Date = (min(data$Date) : max(data$Date)) %>%
                          as_date()),
             by = "Date"
  ) %>% arrange(Date)
}