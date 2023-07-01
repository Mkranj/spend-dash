cover_all_dates_in_period <- function(data) {
  right_join(data,
             data.frame(Date = (min(data$Date) : max(data$Date)) %>%
                          as_date()),
             by = "Date"
  ) %>% arrange(Date)
}

#' helper for change_month
#' 
#' Check if the day part of date is a certain number
#'
#' @param date Date object
#' @param day numeric
#'
#' @return TRUE/FALSE
check_is_day <- function(date, day) {
  day(date) == day
}

#' Change to beggining/end of month
#' 
#' Move to next/previous month if needed
#'
#' @param date Date object
#' @param direction One of `forward` or `backward`. Which direction in time to shift to?
#' @param target_day One of `first` or `last`. Do you need the start or end of the month?
#'
#' @return Date object.
change_month <- function(date, direction = c("forward", "backward"),
                         target_day = c("first", "last")) {
  direction <- match.arg(direction)
  target_day <- match.arg(target_day)
  
  new_date <- date
  
  already_month_start <- check_is_day(new_date,
                                      floor_date(new_date, unit = "months") %>% day()
  )
  
  already_month_end <- check_is_day(new_date,
                                    ceiling_date(new_date, unit = "months") %m-% days(1)
                                    %>% day()
  )
  
  if (direction == "forward") {
    if (target_day == "first") {
      new_date <- new_date %m+% months(1) %>% floor_date(unit = "months")
    } else if (target_day == "last") {
      month_amount <- already_month_end %>% as.numeric()
      
      new_date <- new_date %m+% months(month_amount) %>%
        ceiling_date(unit = "months") %m-% days(1)
    }
  } else if (direction == "backward") {
    if (target_day == "first") {
      month_amount <- already_month_start %>% as.numeric()
      new_date <- new_date %m-% months(month_amount) %>%
        floor_date(unit = "months")
    } else if (target_day == "last") {
      new_date <- new_date %m-% months(1) %>%
        ceiling_date(unit = "months") %m-% days(1)
    }
  }
  
  new_date
}