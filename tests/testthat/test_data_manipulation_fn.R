


test_that("change_month: moving forward works", {
  my_date <- as.Date("2023-03-16")
  expect_equal(change_month(my_date), as.Date("2023-04-01"))
  
  expect_equal(change_month(my_date,
                            direction = "forward",
                            target_day = "first"), as.Date("2023-04-01"))
  
  expect_equal(change_month(my_date,
                            direction = "forward",
                            target_day = "last"), as.Date("2023-03-31"))
  
  
})

test_that("change_month: moving forward twice works", {
  my_date <- as.Date("2023-03-16")
  
  twice_forward <- change_month(my_date,
                                direction = "forward",
                                target_day = "first") %>% 
    change_month("forward", "first")
  
  expect_equal(twice_forward, as.Date("2023-05-01"))
  
})

test_that("change_month: moving forward works - 1st and 31st day", {
  my_date1 <- as.Date("2023-03-01")
  my_date31 <- as.Date("2023-03-31")
  
  expect_equal(change_month(my_date1,
                            direction = "forward",
                            target_day = "first"), as.Date("2023-04-01"))
  
  expect_equal(change_month(my_date1,
                            direction = "forward",
                            target_day = "last"), as.Date("2023-03-31"))
  
  expect_equal(change_month(my_date31,
                            direction = "forward",
                            target_day = "first"), as.Date("2023-04-01"))
  
  expect_equal(change_month(my_date31,
                            direction = "forward",
                            target_day = "last"), as.Date("2023-04-30"))
  
})

test_that("change_month: moving backward works", {
  my_date <- as.Date("2023-03-16")
  
  expect_equal(change_month(my_date,
                            direction = "backward",
                            target_day = "first"), as.Date("2023-03-01"))
  
  expect_equal(change_month(my_date,
                            direction = "backward",
                            target_day = "last"), as.Date("2023-02-28"))
  
  
})

test_that("change_month: moving backward works - 1st and 31st day", {
  my_date1 <- as.Date("2023-03-01")
  my_date31 <- as.Date("2023-03-31")
  
  expect_equal(change_month(my_date1,
                            direction = "backward",
                            target_day = "first"), as.Date("2023-02-01"))
  
  expect_equal(change_month(my_date1,
                            direction = "backward",
                            target_day = "last"), as.Date("2023-02-28"))
  
  expect_equal(change_month(my_date31,
                            direction = "backward",
                            target_day = "first"), as.Date("2023-03-01"))
  
  expect_equal(change_month(my_date31,
                            direction = "backward",
                            target_day = "last"), as.Date("2023-02-28"))
  
})