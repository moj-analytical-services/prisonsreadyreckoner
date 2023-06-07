# This script tests functions for general use


# Testing multiplication of two vectors containing named entries
test_that("multiply_two_named_vectors() multiplies vectors together correctly", {
  
  dummy1 <- c(senband1 = 1, senband2 = 2, senband3 = 4, senband4 = 2)
  dummy2 <- c(senband1 = 20, senband2 = 20, senband3 = 30, senband4 = 11)
  dummy3 <- c(senband1 = 20, senband2 = 20, senband3 = 30)
  dummy4 <- c(senband1 = 20, senband2 = 20, apples = 30, senband4 = 11)
  dummy5 <- c(bananas = 20, oranges = 20, apples = 30, pears = 11)
  
  expect_equal(multiply_two_named_vectors(dummy1, dummy2), c(senband1 = 20, senband2 = 40, senband3 = 120, senband4 = 22))
  expect_equal(multiply_two_named_vectors(dummy1, dummy2, arguments_to_keep = c("senband1", "senband2")), c(senband1 = 20, senband2 = 40))
  expect_equal(multiply_two_named_vectors(dummy1, dummy4), c(senband1 = 20, senband2 = 40, senband3 = NA, senband4 = 22))
  expect_equal(multiply_two_named_vectors(dummy1, dummy5), c(senband1 = NA_integer_, senband2 = NA_integer_, senband3 = NA_integer_, senband4 = NA_integer_))
  expect_equal(multiply_two_named_vectors(dummy1, dummy5, arguments_to_keep = c("bananas")), c(oranges = 1)[c("bananas")])
  
})

