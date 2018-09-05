source("stdfunction.R")

# context with one test that groups expectations
context("Self Tests for Standardize")


test_that("standardize works with normal input mixed with logic", {
  x <- c(1, 2, 3, FALSE)
  z <- (x - mean(x)) / sd(x)
  
  expect_equal(standardize(x), z)
  expect_length(standardize(x), length(x))
  expect_type(standardize(x), 'double')
})


test_that("standardize works with logic and missing values", {
  y <- c(FALSE, TRUE, NA)
  z1 <- (y - mean(y, na.rm = FALSE)) / sd(y, na.rm = FALSE)
  z2 <- (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
  
  expect_equal(standardize(y), z1)
  expect_length(standardize(y), length(y))
  expect_equal(standardize(y, na.rm = TRUE), z2)
  expect_type(standardize(y), 'double')
})
