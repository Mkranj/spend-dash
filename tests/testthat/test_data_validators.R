test_that("trim_column_ws tidies column names", {
  df <- data.frame("date" = NA, "second" = 2)
  colnames(df) <- c("date    ", "  second")
  
  tidy_names <- c("date", "second")
  expect_equal(trim_column_ws(df) |> colnames(),
               tidy_names)
  
})

test_that("column names wholly lowercase", {
  df <- data.frame("Date" = NA, "seCOND" = 2)
  
  tidy_names <- c("date", "second")
  
  cleaned_names <- colnames_to_lowercase(df) |> colnames()
  expect_equal(cleaned_names, tidy_names)
})

test_that("available columns correctly detected", {
  df_all_four <- data.frame("date" = NA, "amount" = NA, "description" = NA, "category" = NA)
  
  expect_equal(detect_data_columns(df_all_four),
               list("Date" = T, "Amount" = T, "Description" = T, "Category" = T)
  )
  
  df_date_amount <- data.frame("date" = NA, "amount" = NA)
  
  expect_equal(detect_data_columns(df_date_amount),
               list("Date" = T, "Amount" = T, "Description" = F, "Category" = F)
  )
  
  df_date_amount_desc <- data.frame("Date" = NA, "Amount" = NA, "Description" = NA)
  
  expect_equal(detect_data_columns(df_date_amount_desc),
               list("Date" = T, "Amount" = T, "Description" = T, "Category" = F)
  )
  
  # Even with lowercase name, it gets detected correctly
  df_desc <- data.frame("description" = NA)
  
  expect_equal(detect_data_columns(df_desc),
               list("Date" = F, "Amount" = F, "Description" = T, "Category" = F)
  )
  
})

test_that("validate_date_amount_present fails on df with no date/amount columns", {
  df_all_four <- data.frame("date" = NA, "amount" = NA, "description" = NA, "category" = NA)
  
  detected_columns <- detect_data_columns(df_all_four)
  expect_no_error(validate_date_amount_present(detected_columns))
  
  df_desc <- data.frame("description" = NA)
  
  detected_columns <- detect_data_columns(df_desc)
  expect_error(validate_date_amount_present(detected_columns))
})

test_that("date validation makes no changes if recognised as date", {
  df <- data.frame(date = as_date("2020-01-01", x = 100))
  
  validated_df <- validate_date_column(df)
  expect_equal(df, validated_df)
  
})

test_that("date validation correctly makes dates from character columns", {
  df <- data.frame(date = as.character("2020-01-01"), x = 100)
  
  validated_df <- validate_date_column(df)
  validated_date <- pull(validated_df, date)
  expect_equal(validated_date, lubridate::date("2020-01-01"))
  
  dotted_date_df <- data.frame(date = as.character("01.01.2020."), x = 100)
  
  validated_df <- validate_date_column(dotted_date_df)
  validated_date <- pull(validated_df, date)
  expect_equal(validated_date, lubridate::date("2020-01-01"))
})

test_that("date validation correct for fixtures", {
  validated_df <- validate_date_column(df_dad1)
  validated_date <- pull(validated_df, date)
  expect_equal(validated_date, fixture_dates_used)
  
  validated_df <- validate_date_column(df_dad2)
  validated_date <- pull(validated_df, date)
  expect_equal(validated_date, fixture_dates_used)
})

test_that("amount validation correct for character recognition", {
  df <- data.frame(amount = c("2500", "110", "115.5"))
  
  validated_df <- validate_amount_column(df)
  expect_equal(pull(validated_df, amount),
               c(2500, 110, 115.5))
})

test_that("amount validation correct for decimal commas", {
  df <- data.frame(amount = c("25,00", "110", "115,5"))
  
  validated_df <- validate_amount_column(df)
  expect_equal(pull(validated_df, amount),
               c(25, 110, 115.5))
})

test_that("empty strings changed to NA in description and category", {
  df_all_four <- data.frame("date" = "2020-01-01", "amount" = 22, "description" = "", "category" = "")
  
  modified_df <- empty_string_to_na(df_all_four,
                                    detect_data_columns(df_all_four))
  expect_equal(modified_df,
               data.frame("date" = "2020-01-01", "amount" = 22, "description" = NA_character_, "category" = NA_character_)
              )
  
  df_all_na_date <- data.frame("date" = NA, "amount" = 22, "description" = "", "category" = "")
  
  modified_df <- empty_string_to_na(df_all_na_date,
                                    detect_data_columns(df_all_na_date))
  expect_equal(modified_df,
               data.frame("date" = NA, "amount" = 22, "description" = NA_character_, "category" = NA_character_)
  )
  
  df_desc_amount <- data.frame("amount" = 22, "description" = "")
  
  modified_df <- empty_string_to_na(df_desc_amount,
                                    detect_data_columns(df_desc_amount))
  expect_equal(modified_df,
               data.frame("amount" = 22, "description" = NA_character_)
  )
  
})

# Integration test
test_that("Integration: load_and_prepare_data runs with no unexpected errors on
          an appropriate dataframe", {
  # MOCKING READING DATA
  read_data_mock <- function(filename) {
    df_dad1
  }
  
  local_mock(read_data_file = read_data_mock)
  
  expect_no_error(loaded_data <- load_and_prepare_data(filename = "mock_file"))
  
  # The function returns both validated data and a list of 4 columns' presence
  expect_named(loaded_data, c("data", "detected_columns"))
  expect_equal(names(loaded_data$detected_columns),
               c("Date", "Amount", "Description", "Category"))
  
  # The columns are correctly detected
  expect_equal(loaded_data$detected_columns,
               list(Date = T,
                    Amount = T,
                    Description = T,
                    Category = F))
})