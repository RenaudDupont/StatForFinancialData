library("testthat")
library("here")
library("PerformanceAnalytics")
library("xts")

source(here("Code/functions.R"))
# Assuming f_compute_log_returns is defined as provided
# Test for the f_compute_log_returns function
test_that("Compute log-returns works correctly", {
  # A simple vector of increasing prices
  prices_xts <- xts(c(100, 105, 110), order.by = as.Date("2020-01-01") + 0:2)
  
  # Expected log-returns, manually calculated or using another reliable method
  expected_value <- CalculateReturns(prices_xts, method = "log")
  actual_value <- f_compute_log_returns(prices_xts)
  
  print(expected_value)
  print(actual_value)
 
  # Test if the function's output matches the expected log-returns
  expect_equal(as.numeric(actual_value[,1][2]), as.numeric(expected_value[,1][2]), tolerance = 0.001)
  expect_equal(as.numeric(actual_value[,1][3]), as.numeric(expected_value[,1][3]), tolerance = 0.001)
})

