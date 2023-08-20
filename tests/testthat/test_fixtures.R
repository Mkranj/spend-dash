test_that("Test: Fixtures appropriate dimensions", {
  expect_equal(nrow(df_dad1), 4)
  expect_equal(nrow(df_dad2), 4)
  
  columns_in_data <- c("date", "amount", "description")
  expect_equal(colnames(df_dad1), columns_in_data)
  expect_equal(colnames(df_dad2), columns_in_data)
})