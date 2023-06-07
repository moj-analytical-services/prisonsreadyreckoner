# This script tests functions for modelling of prisons.


################################################################################
# Profiles
################################################################################



# Testing lag filter construction
test_that("make_lag_filters() will correctly construct a battery of fixed delay filters from a lag vector", {
  
  #skip("Skipping lag filter construction testing. To re-test, comment out the skip() in the test-recall.R script.")
  
  lags <- c(senband1 = 0.77, senband2 = 1.25, senband3 = 3.06, senband4 = 6.72)
  
  h <- make_lag_filters(lags)

  Mh <-
    matrix(c(
      c(0.23, 0.77, 0, 0, 0, 0, 0, 0),
      c(0, 0.75, 0.25, 0, 0, 0, 0, 0),
      c(0, 0, 0, 0.94, 0.06, 0, 0, 0),
      c(0, 0, 0, 0, 0, 0, 0.28, 0.72)
    ),
    4, 8, byrow = TRUE, dimnames = list(NULL, paste0("lag", seq(0, 7))))
  h_exp <- tibble::as_tibble(cbind(senband = paste0("senband", seq(1, 4)), Mh)) %>%
             dplyr::mutate(across(-senband, as.numeric))
  
  expect_equal(h, h_exp)

})



# Testing multiplication of prisoner volumes by ratios that are sorted by sentence band
test_that("apply_ratios() multiplies prisoner volumes and ratios together correctly", {
  
  dummy_input_volumes1 <- tibble::tribble(
    ~senband, ~"2023-04-01", ~"2023-05-01", ~"2023-06-01", ~"2023-07-01", ~"2023-08-01", ~"2023-09-01", ~"2023-10-01", ~"2023-11-01",
    "senband1", 100, 100, 100, 100, 100, 100, 100, 100,
    "senband2", 100, 100, 100, 100, 100, 100, 100, 100,
    "senband3", 100, 100, 100, 100, 100, 100, 100, 100,
    "senband4", 100, 100, 100, 100, 100, 100, 100, 100
  )

  dummy_output_volumes1 <- tibble::tribble(
    ~senband, ~"2023-04-01", ~"2023-05-01", ~"2023-06-01", ~"2023-07-01", ~"2023-08-01", ~"2023-09-01", ~"2023-10-01", ~"2023-11-01",
    "senband1", 100, 100, 100, 100, 100, 100, 100, 100,
    "senband2", 200, 200, 200, 200, 200, 200, 200, 200,
    "senband3", 400, 400, 400, 400, 400, 400, 400, 400,
    "senband4", 200, 200, 200, 200, 200, 200, 200, 200
  )
  
  dummy_output_volumes2 <- tibble::tribble(
    ~senband, ~"2023-04-01", ~"2023-05-01", ~"2023-06-01", ~"2023-07-01", ~"2023-08-01", ~"2023-09-01", ~"2023-10-01", ~"2023-11-01",
    "senband1", 100, 100, 100, 2000, 2000, 2000, 2000, 2000,
    "senband2", 200, 200, 200, 2000, 2000, 2000, 2000, 2000,
    "senband3", 400, 400, 400, 3000, 3000, 3000, 3000, 3000,
    "senband4", 200, 200, 200, 1100, 1100, 1100, 1100, 1100
  )
  
  dummy_ratios1 <- c(senband1 = 1, senband2 = 2, senband3 = 4, senband4 = 2)
  dummy_ratios2 <- c(senband1 = 20, senband2 = 20, senband3 = 30, senband4 = 11)
  dummy_ratios3 <- c(senband1 = 20, senband2 = 20, senband3 = 30)
  dummy_ratios4 <- c(senband1 = 20, senband2 = 20, apples = 30, senband4 = 11)
  
  
  #apply_ratios <- function(input_values, preimpact_ratios, column_to_match = "senband", postimpact_ratios = NULL, impact_date = NULL)
  
  # Test that returned tibble is as expected when each entry in a row is
  # multiplied by the same ratio
  expect_equal(apply_ratios(dummy_input_volumes1, dummy_ratios1), dummy_output_volumes1)
  
  # Test that returned tibble is as expected when each entry in a row before a
  # certain date is multiplied by one ratio, and each entry in the row after a
  # certain date is multiplied by a second ratio
  expect_equal(apply_ratios(dummy_input_volumes1, dummy_ratios1, postimpact_ratios = dummy_ratios2, impact_date = "2023-07-01"), dummy_output_volumes2)
  
  # Test that error is returned when column_to_match is not found in the tibble
  expect_error(apply_ratios(dummy_input_volumes1, dummy_ratios3, column_to_match = "bananas"))
  
  # Test that error is returned when the number of ratios in preimpact_ratios is
  # different to the number of rows in the tibble
  expect_error(apply_ratios(dummy_input_volumes1, dummy_ratios3))
  
  # Test that error is returned when the names of ratios in preimpact_ratios are
  # different to the names of rows in the tibble
  expect_error(apply_ratios(dummy_input_volumes1, dummy_ratios4))
  
  # Test that error is returned when only postimpact_ratios, and not
  # impact_date, is supplied
  expect_error(apply_ratios(dummy_input_volumes1, dummy_ratios1, postimpact_ratios = dummy_ratios2))
  
  # Test that error is returned when only impact_date, and not
  # postimpact_ratios, is supplied
  expect_error(apply_ratios(dummy_input_volumes1, dummy_ratios1, impact_date = "2023-07-01"))
  
  # Test that error is returned when the number of ratios in postimpact_ratios
  # is different to the number of rows in the tibble
  expect_error(apply_ratios(dummy_input_volumes1, dummy_ratios1, impact_date = "2023-07-01", postimpact_ratios = dummy_ratios3))
  
  # Test that error is returned when the names of ratios in postimpact_ratios
  # are different to the names of rows in the tibble
  expect_error(apply_ratios(dummy_input_volumes1, dummy_ratios1, impact_date = "2023-07-01", postimpact_ratios = dummy_ratios4))
  
})

