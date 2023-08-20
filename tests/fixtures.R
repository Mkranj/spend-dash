# dad = Date, Amount, Description present
df_dad1 <- structure(list(
  date = structure(c(1701993600, 1702080000, 1702339200, 1702512000),
                   class = c("POSIXct", "POSIXt"),
                   tzone = "UTC"), 
  amount = c(20, 25.5, 28.3, 222),
  description = c(NA, NA, "ručak", "renta")
), 
class = c("tbl_df", "tbl", "data.frame"),
row.names = c(NA, -4L)
)

df_dad2 <- structure(list(
  date = c("08.12.2023", "09.12.2023", "12.12.2023", "14.12.2023"),
  amount = c("20", "25,5", "28,3", "222"),
  description = c("", "", "ručak", "renta")
),
row.names = c(NA, -4L),
class = c("data.table", "data.frame")
)

fixture_dates_used <- as_date(c("2023-12-08",
                                "2023-12-09",
                                "2023-12-12",
                                "2023-12-14"))