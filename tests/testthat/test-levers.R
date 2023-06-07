
################################################################################
# Modification of probability profiles
################################################################################


# Testing stretch_profiles()
test_that("stretch_profiles() resamples input probabiltity distributions correctly", {

  ###### PREPARE DATA ######
  ### Tibbles
  
  dummy_profile_correct_format <- tibble::tribble(
    ~senband, ~phase, ~lag0, ~lag1, ~lag2, ~lag3, ~lag4, ~lag5, ~lag6, ~lag7,
    "senband1", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband2", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband3", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband4", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband1", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband2", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband3", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband4", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2
  )
  
  dummy_profile_incorrect_format1 <- tibble::tribble(
    ~lag0, ~lag1, ~lag2, ~lag3, ~lag4, ~lag5, ~lag6, ~lag7,
    0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2
  )
  
  dummy_profile_incorrect_format2 <- tibble::tribble(
    ~senband, ~phase, ~lag0, ~lag1, ~lag2, ~lag3, ~lag4, ~lag5, ~lag6, ~lag7,
    "senband1", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband2", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband3", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband4", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
  )
  
  dummy_profile_incorrect_format3 <- tibble::tribble(
    ~senband, ~phase, ~lag0, ~lag1, ~lag2, ~lag3, ~lag4, ~lag5, ~lag6, ~lag7,
    "senband1", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband2", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband3", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband4", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
  )
  
  dummy_profile_incorrect_format4 <- tibble::tribble(
    ~senband, ~phase, ~lag0, ~lag1, ~lag2, ~lag3, ~lag4, ~lag5, ~lag6, ~lag7,
    "senband1", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband2", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband3", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband4", "post_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband1", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband2", "incorrect_name", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband3", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband4", "pre_impact", 0.1, 0.3, 0.05, 0.03, 0.02, 0.25, 0.05, 0.2
  )
  
  dummy_profile_incorrect_format5 <- tibble::tribble(
    ~senband, ~phase, ~lag0, ~lag1, ~lag2, ~lag3, ~lag4, ~lag5, ~lag6, ~lag7,
    "senband1", "post_impact", 0.1, 0.3, "vehicle", 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband2", "post_impact", 0.1, 0.3, "vehicle", 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband3", "post_impact", 0.1, 0.3, "vehicle", 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband4", "post_impact", 0.1, 0.3, "vehicle", 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband1", "pre_impact", 0.1, 0.3, "vehicle", 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband2", "pre_impact", 0.1, 0.3, "vehicle", 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband3", "pre_impact", 0.1, 0.3, "vehicle", 0.03, 0.02, 0.25, 0.05, 0.2,
    "senband4", "pre_impact", 0.1, 0.3, "vehicle", 0.03, 0.02, 0.25, 0.05, 0.2
  )
  
  ### Lists of stretch factors
  # Acceptable list of stretch factors (i.e. all stretch factors > 0)
  stretch_factors_correct1 <- c(senband1 = 0.5, senband2 = 2, senband3 = 2.5, senband4 = 3)
  
  # Acceptable list of stretch factors (i.e. all stretch factors > 0), with all
  # stretch factors equal to one
  stretch_factors_correct2 <- c(senband1 = 1, senband2 = 1, senband3 = 1, senband4 = 1)
  
  # Unacceptable list of stretch factors - one of the stretch factors equals zero
  stretch_factors_incorrect1 <- c(senband1 = 0.5, senband2 = 2, senband3 = 0, senband4 = 3)
  
  # Unacceptable list of stretch factors - one of the stretch factors is negative
  stretch_factors_incorrect2 <- c(senband1 = 0.5, senband2 = -2, senband3 = 2.5, senband4 = 3)
  
  
  min_stretch_factor <- 0.5
  max_stretch_factor <- 3
  
  
  ###### STRETCH_PROFILES RETURNS EXPECTED OUTPUTS ######
  ### No change to input profile as all stretch factors equal 1
  projection_length_months = 1000000
  expect_equal(stretch_profiles(dummy_profile_correct_format, stretch_factors_correct2, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("senband", "phase")), dummy_profile_correct_format)
  
  
  ###### INPUT TIBBLE IS IN INCORRECT FORMAT ######
  ### Input tibble does not have expected non-data columns
  expect_error(stretch_profiles(dummy_profile_incorrect_format1, stretch_factors_correct1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("phase")))
  
  ### Input tibble does not have both entries labelled "pre_impact" and entries
  ### labelled "post_impact" in column "phase"
  expect_error(stretch_profiles(dummy_profile_incorrect_format2, stretch_factors_correct1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("senband", "phase")))
  expect_error(stretch_profiles(dummy_profile_incorrect_format3, stretch_factors_correct1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("senband", "phase")))
  expect_error(stretch_profiles(dummy_profile_incorrect_format4, stretch_factors_correct1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("senband", "phase")))
  
  ### Input tibble contains non-numeric entries in the data columns (i.e. in
  ### those columns that are not specified by "non_data_cols")
  expect_error(stretch_profiles(dummy_profile_incorrect_format5, stretch_factors_correct1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("senband", "phase")))
  
  
  ###### INCORRECT ARGUMENTS TO FUNCTION ######
  ### No non-data columns are specified
  expect_error(stretch_profiles(dummy_profile_correct_format, stretch_factors_correct1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = NULL))
  
  ### One of the stretch factors is smaller than min_stretch_factor
  expect_error(stretch_profiles(dummy_profile_correct_format, stretch_factors_incorrect1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("senband", "phase")))
  
  ### One of the stretch factors is negative
  #expect_error(stretch_profiles(dummy_profile_correct_format, stretch_factors_incorrect2, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("senband", "phase")))
  
  ### Parameter non_data_cols names a column that does not exist in the tibble
  expect_error(stretch_profiles(dummy_profile_correct_format, stretch_factors_correct1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = "unused_column"))
  
  ### Parameter non_data_cols does not contain the column name "phase", which is
  ### required for stretch_profiles() to work correctly
  expect_error(stretch_profiles(dummy_profile_correct_format, stretch_factors_correct1, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = "senband"))
  
})