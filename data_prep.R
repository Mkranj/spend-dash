data_filename <- "expense_data_1.csv"

finance_data <- read.csv(data_filename)

# Remove unneeded columns
finance_data <- select(finance_data, -c(Subcategory,  Account, Account.1, Note.1, INR))

# We're pnly going to be focusing on the expenses
expenses <- filter(finance_data, Income.Expense == "Expense")


# The time is currently written as a timestamp, we'll keep that separate and make
# a new variable that contains just the date.
expenses <- rename(expenses,
                   "Time" = Date) %>%
  mutate(
    Date = strptime(Time, "%m/%d/%Y")
  )
  
# Some data is in INR,  other in USD. We'll recalculate everything to USD

# Checked online exchange rate on 27.5.23.
exchange_rate_INR_USD <- 0.012

expenses <- 
  mutate(expenses,
    Amount = ifelse(Currency == "INR", 
                    Amount * exchange_rate_INR_USD,
                    Amount),
    Currency = "USD"
  )
