date_from_year_month <- function(vector_years, vector_months) {
  strptime(paste0(
    vector_years, "-", vector_months, "-01"
  ), 
  format = "%Y-%m-%d")
}
