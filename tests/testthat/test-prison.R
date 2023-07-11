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


# Testing remand population filter construction
test_that("make_remand_filter_out() will correctly construct a linearly declining filter, settling at a basal value", {
  
  remand_rate  <- 0.1
  no_bail_rate <- 0.2
  ctl          <- 10   # [months]
  
  
  # Test staggered application of the custody time limit.
  N            <- 15
  h <- make_remand_filter_out(remand_rate, no_bail_rate, ctl, N)
  h_exp <- tibble::tibble(casetype = "remand",
                          lag = seq(0, N-1), 
                          impact = -c(0.092, 0.084, 0.076, 0.068, 0.060, 0.052, 0.044, 0.036, 0.028, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02)) %>%
    tidyr::pivot_wider(names_from = "lag", names_prefix = "lag", values_from = "impact")
  expect_equal(h, h_exp)
  
  
  # Also test for extreme case where N is shorter than the custody time limit.
  N            <- 5
  h <- make_remand_filter_out(remand_rate, no_bail_rate, ctl, N)
  h_exp <- tibble::tibble(casetype = "remand",
                          lag = seq(0, N-1), 
                          impact = -c(0.092, 0.084, 0.076, 0.068, 0.060)) %>%
    tidyr::pivot_wider(names_from = "lag", names_prefix = "lag", values_from = "impact")
  expect_equal(h, h_exp)
  
})


# Testing remand population filter application.
test_that("calculate_pop_remand_delta() generates the expected steady state remand population", {
  
  remand_rates  <- c(receipts = 0.1, disposals = 0.25)
  no_bail_rate <- 0.2
  ctl          <- 10   # [months]
  N            <- 15
  
  x <- c(receipts = 1, disposals = 2)
  # Assign toy data to illustrative disposal types.
  mc_disposals_delta <- tibble::tibble(date = seq.Date(as.Date("2023-03-01"), by = "1 month", length.out = N),
                                       disposal_type = "mc_ind", remanded = TRUE, n_disposals_delta = c(x[['receipts']], rep(0, N-1)))
  cc_disposals_delta <- tibble::tibble(date = seq.Date(as.Date("2023-03-01"), by = "1 month", length.out = N),
                                       disposal_type = "egp", n_disposals_delta = c(x[['disposals']], rep(0, N-1)))
  
  profiles_remand_in <- make_remand_filter_in(remand_rates[['receipts']], no_bail_rate, ctl, N)
  profiles_remand_out <- make_remand_filter_out(remand_rates[['disposals']], no_bail_rate, ctl, N)
  
  pop_remand_delta <- calculate_pop_remand_delta(mc_disposals_delta, cc_disposals_delta, profiles_remand_in, profiles_remand_out)
  expect_equal(pop_remand_delta[[1,N+1]], (x[['receipts']] * remand_rates[['receipts']] - x[['disposals']] * remand_rates[['disposals']]) * no_bail_rate)
  
})


# Testing remand population against an external benchmark.
# Note that, at the time of construction, only disposal-based scenarios were
# available
test_that("calculate_pop_remand_delta() generates the expected steady state remand population", {
  
  path_target <- "s3://alpha-prison-forecasting-data/flowcast/test-files/test-sitting-day-scenario-impacts.xlsx"
  sheet_disposals <- "Disposal Difference"
  sheet_remand <- "Net Remand Impact"
  

  #remand_rates <- c(receipts = 0.187684259863295, disposals = 0.478118695083818)   # '20230706 - To Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'
  remand_rates <- c(receipts = 0.187684259863295, disposals = 0.4753537)            # Rates adjusted to create a match.
  no_bail_rate <- 0.2                                      # '20230705 - Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'
  ctl          <- 6                                        # [months] '20230705 - Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'


  disposals_delta <- import_s3_file(path_target, sheet = sheet_disposals)
  pop_remand_exp <- import_s3_file(path_target, sheet = sheet_remand)
  
  N <- nrow(disposals_delta)
  profiles_remand_in <- make_remand_filter_in(remand_rates[['receipts']], no_bail_rate, ctl, N)
  profiles_remand_out <- make_remand_filter_out(remand_rates[['disposals']], no_bail_rate, ctl, N)
  
  start_date <- min(as.Date(disposals_delta$Date))

  
  pop_remand_act <- pop_remand_exp
  pop_remand_act[, 2:ncol(pop_remand_exp)] <- 0
  for (i in 2:ncol(pop_remand_act)) {
    
    # For now, MC disposals are zero.
    mc_disposals_delta <- tibble::tibble(date = seq.Date(start_date, by = "1 month", length.out = N),
                                           disposal_type = "mc_ind", remanded = TRUE, n_disposals_delta = 0)

    cc_disposals_delta <- tibble::tibble(date = seq.Date(start_date, by = "1 month", length.out = N),
                                         disposal_type = "egp", n_disposals_delta = disposals_delta[, i])
    
    pop_remand_delta <- calculate_pop_remand_delta(mc_disposals_delta, cc_disposals_delta, profiles_remand_in, profiles_remand_out)
    pop_remand_delta <- tidyr::pivot_longer(pop_remand_delta, c(2:ncol(pop_remand_delta)),
                          names_to = "date",
                          values_to = "pop_remand"
      )
    
    #print(pop_remand_exp[, i] / pop_remand_delta$pop_remand * remand_rates[['disposals']])
    pop_remand_act[, i] <- round(pop_remand_delta$pop_remand)
    pop_remand_exp[, i] <- round(pop_remand_exp[, i])
    
  }

  expect_equal(pop_remand_act, pop_remand_exp)

})




################################################################################
# Miscellaneous
################################################################################


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

