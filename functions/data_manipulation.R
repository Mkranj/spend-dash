cover_all_dates_in_period <- function(data) {
  right_join(data,
             data.frame(Date = (min(expenses$Date) : max(expenses$Date)) %>%
                          as_date()),
             by = "Date"
  ) %>% arrange(Date)
}