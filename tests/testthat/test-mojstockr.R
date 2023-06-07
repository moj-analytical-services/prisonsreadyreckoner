# This script tests functions general to any stock-flow modelling.


################################################################################
# Filtering
################################################################################

# Function for creating dummy data.
get_dummy_data <- function() {
  
  # Prepare data.
  non_data_cols <-
    data.frame(
      age = c("Adult", "Adult", "Youth", "Youth"),
      sex = c("Female", "Male", "Female", "Male")
    )
  
  # Inflows
  Mx <-
    matrix(c(
      c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
      c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2),
      c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    ),
    4, 10, byrow = TRUE, dimnames = list(NULL, paste0("lag", seq(0, 9))))
  x <-
    data.frame(
      non_data_cols,
      Mx
    )
  
  # System response
  Mh <-
    matrix(c(
      c(0.5, 0.25, 0.1, 0.1, 0.05),
      c(0.5, 0.25, 0.1, 0.1, 0.05),
      c(0.6, 0.3, 0.08, 0.02, 0),
      c(0.6, 0.3, 0.08, 0.02, 0)
    ),
    4, 5, byrow = TRUE, dimnames = list(NULL, paste0("lag", seq(0, 4))))
  h <-
    data.frame(
      non_data_cols,
      Mh
    )
  
  # Outflows
  My <-
    matrix(c(
      c(0, 0, 0.5, 0.75, 0.85, 0.95, 1, 1, 1, 1),
      c(0.5, 0.25, 0.1, 0.1, 0.05, 0, 0, 0, 0, 0),
      c(0, 0, 1.2, 1.8, 1.96, 2, 2, 2, 2, 2),
      c(0, 0, 0, 0, 0.6, 0.3, 0.08, 0.02, 0, 0)
    ),
    4, 10, byrow = TRUE, dimnames = list(NULL, paste0("lag", seq(0, 9))))
  y <-
    data.frame(
      non_data_cols,
      My
    )
  
  # Population
  Mz <-
    matrix(c(
      round(cumsum(c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1) - c(0, 0, 0.5, 0.75, 0.85, 0.95, 1, 1, 1, 1)), 2),
      round(cumsum(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0) - c(0.5, 0.25, 0.1, 0.1, 0.05, 0, 0, 0, 0, 0)), 2),
      round(cumsum(c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2) - c(0, 0, 1.2, 1.8, 1.96, 2, 2, 2, 2, 2)), 2),
      round(cumsum(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0) - c(0, 0, 0, 0, 0.6, 0.3, 0.08, 0.02, 0, 0)), 2)
    ),
    4, 10, byrow = TRUE, dimnames = list(NULL, paste0("lag", seq(0, 9))))
  z <-
    data.frame(
      non_data_cols,
      Mz
    )
  
  return(list(non_data_cols = non_data_cols, x = x, h = h, y = y, z = z))
}


# Testing lag filter construction
test_that("make_lag_filter() will correctly construct a fixed delay filter", {
  
  #skip("Skipping lag filter construction testing. To re-test, comment out the skip() in the test-mojstockr.R script.")
  
  # Integer lag
  h <- mojstockr_make_lag_filter(3, 10)
  h_exp <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
  expect_equal(h, h_exp)
  
  # Non-integer lag
  h <- mojstockr_make_lag_filter(3.2, 10)
  h_exp <- c(0, 0, 0, 0.8, 0.2, 0, 0, 0, 0, 0, 0)
  expect_equal(h, h_exp)
  
  # Integer lag, unspecified length
  h <- mojstockr_make_lag_filter(3)
  h_exp <- c(0, 0, 0, 1)
  expect_equal(h, h_exp)
  
  # Non-integer lag, Unspecified length
  h <- mojstockr_make_lag_filter(3.2)
  h_exp <- c(0, 0, 0, 0.8, 0.2)
  expect_equal(h, h_exp)
  
  expect_error(mojstockr_make_lag_filter(10.1, 10))
})


# Testing multiple convolution
test_that("mconv() will correctly convolve multiple time series with multiple filters", {
  
  #skip("Skipping multiple convolution testing. To re-test, comment out the skip() in the test-mojstockr.R script.")
  
  data <- get_dummy_data()
  
  # Regular test.
  y <- mojstockr_mconv(data$x, data$h, non_data_cols = c("age", "sex"))
  y[, 3:12] <- round(y[, 3:12], 2)
  expect_equal(y, tibble::as_tibble(data$y))
  
  # Test with no non-data columns.
  y <- mojstockr_mconv(data$x[, 3:12], data$h[, 3:7], non_data_cols = NULL)
  y <- round(y, 2)
  expect_equal(y, tibble::as_tibble(data$y[, 3:12]))
  
})


# Testing stock construction
test_that("build_stock() will correctly build stock from inflows and outflows", {
  
  #skip("Skipping stock building testing. To re-test, comment out the skip() in the test-mojstockr.R script.")
  
  data <- get_dummy_data()
  
  # Regular test.
  y <- mojstockr_mconv(data$x, data$h, non_data_cols = c("age", "sex"))
  y[, 3:12] <- round(y[, 3:12], 2)
  z <- mojstockr_build_stock(data$x, y, non_data_cols = c("age", "sex"))
  z[, 3:12] <- round(z[, 3:12], 2)
  expect_equal(z, tibble::as_tibble(data$z))
  
  # Test with no non-data columns.
  y <- mojstockr_mconv(data$x[, 3:12], data$h[, 3:7], non_data_cols = NULL)
  y <- round(y, 2)
  z <- mojstockr_build_stock(data$x[, 3:12], y, non_data_cols = NULL)
  z <- round(z, 2)
  expect_equal(z, tibble::as_tibble(t(sapply(data$z[, 3:12], sum))))
  
})



################################################################################
# Calculation of probabilities
################################################################################

# Testing mojstockr_resample_profile()
test_that("mojstockr_resample_profile() resamples an input probabiltity distribution correctly", {
  
  #skip("Skipping proportions testing. To re-test, comment out the skip() in the test-mojstockr.R script.")
  
  # Prepare data
  dummy_profile1 <- data.frame(a = c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11"),
                              t = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                              proportions = c(0.1, 0.2, 0.03, 0.07, 0.05, 0.05, 0.01, 0.1, 0.1, 0.1, 0.1, 0.09)
                              )
  
  ######
  # Tests for input arguments
  ######
  # Must supply one of x_resampled or stretch_factor
  expect_error(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions))
  
  # Must supply only one, not both, of x_resampled or stretch_factor
  expect_error(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, stretch_factor = 2, x_resampled = 10))
  
  
  ######
  # Tests for x_resampled
  ######
  # Resampled probability distribution has only one value
  expect_error(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, x_resampled = 10))
  
  # Resampled probability distribution has only two values
  expect_warning(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, x_resampled = c(10,11)))
  
  # Test that supplying x_resampled = x returns an unchanged probability distribution
  expect_equal(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, x_resampled = dummy_profile1$t)$probabilities_resampled, dummy_profile1$proportions)
  expect_equal(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, x_resampled = dummy_profile1$t)$x_resampled, dummy_profile1$t)
  
  ######
  # Tests for stretch_factor
  ######
  # Cannot stretch distribution by negative factor
  expect_error(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, stretch_factor = -1))
  
  # stretch_factor must be greater than 0
  expect_error(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, stretch_factor = 0))
  
  # # Input probabilities do not sum to 1
  # expect_warning(mojstockr_resample_profile(c(0,1), c(1,1), stretch_factor = 2))
  
  # Test that stretch_factor = 1 returns an unchanged distribution
  expect_equal(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, stretch_factor = 1)$probabilities_resampled, dummy_profile1$proportions)
  expect_equal(mojstockr_resample_profile(dummy_profile1$t, dummy_profile1$proportions, stretch_factor = 1)$x_resampled, dummy_profile1$t)
})



################################################################################
# Calculation of proportions
################################################################################

# Testing mojstockr_calculate_proportions()
test_that("mojstockr_calculate_proportions() calculates proportions correctly", {
  
  input_df <- data.frame(a = c("France", "France", "France", "France", "Germany", "Germany", "Germany", "Germany", "Poland", "Poland", "Ukraine", "Ukraine", "Ukraine", "Ukraine", "Ukraine"),
                   b = c("Farm", "Farm", "Farm", "Village", "City", "City", "City", "City", "Village", "Town", "Town", "Town", "Town", "Town", "Town"),
                   c = c("male", "female", "unknown", "male", "female", "unknown", "unknown", "unknown", "unknown", "unknown", "male", "female", "female", "female", "female"),
                   d = c("A", "B", "C", "D","E", "F", "G", "H", "A", "B", "C", "D", "E", "F", "G"),
                   volume = c(12, 20, 8, 15, 25, 20, 7, 2, 38, 3, 45, 2, 9, 13, 16)
  )
  
  # Test that calculated proportion equals 1 if only one row of data is supplied
  expect_equal(mojstockr_calculate_proportions(input_df[1,], c('a', 'b'), c('c'))$proportion, 1)
  
  # Test that all rows are retained when all columns are used for grouping
  # (assuming that all rows are unique). Note that
  # mojstockr_calculate_proportions() reorders rows, even if all rows belong to
  # a different group.
  expect_equal(sort(mojstockr_calculate_proportions(input_df, c('a', 'b', 'c', 'd', 'volume'), c('c'))$a), sort(input_df$a))
  expect_equal(sort(mojstockr_calculate_proportions(input_df, c('a', 'b', 'c', 'd', 'volume'), c('c'))$b), sort(input_df$b))
  expect_equal(sort(mojstockr_calculate_proportions(input_df, c('a', 'b', 'c', 'd', 'volume'), c('c'))$c), sort(input_df$c))
  expect_equal(sort(mojstockr_calculate_proportions(input_df, c('a', 'b', 'c', 'd', 'volume'), c('c'))$d), sort(input_df$d))
  expect_equal(sort(mojstockr_calculate_proportions(input_df, c('a', 'b', 'c', 'd', 'volume'), c('c'))$volume), sort(input_df$volume))
  expect_equal(mojstockr_calculate_proportions(input_df, c('a', 'b', 'c', 'd', 'volume'), c('c'))$proportion, rep(1, length.out = 15))
  # dplyr::group_by(df, dplyr::across(dplyr::all_of(c("a", "c"))))
  
  
  test_df <- data.frame(a = c("France", "France", "France"),
                        b = c("Farm", "Farm", "Farm"),
                        c = c("unknown", "unknown", "unknown"),
                        d = c("dummy1", "dummy2", "dummy3")
  )
  
  # Test that calculated proportion equals 1 if all rows belong to the same group and subgroup
  expect_equal(mojstockr_calculate_proportions(test_df, c('a', 'b'), c('c'))$proportion, 1)
  
  # Test that calculated proportions equal 1 if group and subgroup are specified by the same collection of columns
  expect_equal(mojstockr_calculate_proportions(test_df, c('a', 'b'), c('a', 'b'))$proportion, 1)
  expect_equal(mojstockr_calculate_proportions(test_df, c('d'), c('d'))$proportion[1], 1)
  
  
  empty_df <- data.frame(a = c("France", "France", "France", "France", "Germany", "Germany", "Germany", "Germany", "Poland", "Poland", "Ukraine", "Ukraine", "Ukraine", "Ukraine", "Ukraine"),
                   b = c("Farm", "Farm", "Farm", "Village", "City", "City", "City", "City", "Village", "Town", "Town", "Town", "Town", "Town", "Town"),
                   c = c("male", "female", "unknown", "male", "female", "unknown", "unknown", "unknown", "unknown", "unknown", "male", "female", "female", "female", "female"),
                   d = c("A", "B", "C", "D","E", "F", "G", "H", "A", "B", "C", "D", "E", "F", "G"),
                   volume = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
  
  # Test that calculated proportions equal NaN if all weights equal 0
  expect_true(is.na(mojstockr_calculate_proportions(empty_df, c('a','b'), c('c'), weights_column = "volume")$proportion[1]))
})
