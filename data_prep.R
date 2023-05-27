finance_data <- read.csv("expense_data_1.csv")
finance_data <- select(finance_data, !c(Subcategory, Account.1, Note.1, INR))

expenses <- filter(finance_data, Income.Expense == "Expense")

date_transform_total <- finance_data$Date %>% strptime("%m/%d/%Y")

date_transform_ex <- expenses$Date %>% strptime("%m/%d/%Y")
expenses$date_transform <- date_transform_ex

# Not all dates are included - include the ones with no expense or income

all_dates <- seq(min(date_transform_total),
                 max(date_transform_total), "days")
all_dates <- data.frame(date_transform = all_dates)

expenses <- left_join(
  all_dates,
  expenses)

expenses$date_format <- expenses$date_transform %>% format(format = "%d.%m.%Y")

currency <- unique(finance_data$Currency)[1]
# FIX! something is in USD, recalculate it.