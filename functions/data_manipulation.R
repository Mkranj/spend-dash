cover_all_dates_in_period <- function(data,
                                      start = NA,
                                      end = NA) {
  
  if (is.na(start)) {
    starting_date <- min(data$Date)
  } else {
    starting_date <- start 
  }
  
  if (is.na(end)) {
    ending_date <- max(data$Date)
  } else {
    ending_date <- end
  }
  
  right_join(data,
             data.frame(Date = (starting_date : ending_date) %>%
                          as_date()),
             by = "Date"
  ) %>% arrange(Date)
}

cover_all_months_in_period <- function(data,
                                       start = NA,
                                       end = NA) {
  # Make all of them be the first day of the month so it matches up
  # with summarised df
  if (is.na(start)) {
  starting_date <- min(data$Date)
  } else {
  starting_date <- start 
  day(starting_date) <- 1
  }
  
  if (is.na(end)) {
    ending_date <- max(data$Date)
  } else {
    ending_date <- end
    day(ending_date) <- 1
  }
  
  all_months <- seq(starting_date,
                    ending_date,
                    by = "months")
  
  right_join(data,
             data.frame(Date = all_months),
             by = "Date"
  ) %>% 
    arrange(Date) %>%
    mutate(across(where(is.numeric),
           ~replace_na(., 0)))
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

#' Change to beginning/end of month
#' 
#' For exploring expenses, it is desirable to be able to easily select whole
#' months, especially since we calculate a lot of monthly averages. However,
#' we also want to be able to specify exact days in range if needed. This
#' function is used for moving forward and backward a month at a time. If you're
#' in the middle of the month, it will first give a date in the same month, but
#' at the beginning or end, as specified. Only with further use will it advance
#' to next months. It also keeps the end date at the very end of the month,
#' while just substracting months would then pick only the 30th (or 28th) after
#' it has been encountered.
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