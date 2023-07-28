# Functions for creating tables used during development, testing or
# visualisation.


################################################################################
# Crown Court files
################################################################################

# test-ringfenced-lookup.csv
# Created for developing the sequence of functions responsible for modelling
# Crown Court disposal volumes.
make_ringfenced_lookup <- function(mode = 'save') {
  
  path_ringfenced_lookup <- "s3://alpha-prison-forecasting-data/prisons-ready-reckoner/prisonsreadyreckoner/test-files/test-ringfenced-lookup.csv"
  
  switch(
    mode,
    'save' = {
      lookup <- prisonsreadyreckonerupdater::update_ringfenced_lookup()
      botor::s3_write(lookup$data, readr::write_csv, path_ringfenced_lookup)
    },
    'report' = {},
    stop("Unrecognised mode, '", mode, "'.")
  )
  
  return(list(path_ringfenced_lookup = path_ringfenced_lookup))
}



################################################################################
# Receptions
################################################################################

# test-receptions-model-data.csv
# Created to visualise construction of a linear model for converting disposals
# to sentences.
# See visual-prison-receptions.Rmd
# This function uses disposal data from around the time of the April 2023 prison
# projection, combined with the NEW FULLSAMPLE method.
make_test_receptions_model_data <- function(mode = 'save') {

  path_receptions_model_data <- "s3://alpha-prison-forecasting-data/prisons-ready-reckoner/prisonsreadyreckoner/test-files/test-receptions-model-data.csv"

  path_rdo_data         <- "'s3://alpha-app-cjsm-monitoring/actuals_data_mags/[OFFICIAL SENSITIVE - Magistrates RDOs 19-01 to 22-12 (Jan19 to Dec22) as at 200123.xlsx]Data'!A6:G2936"   # '20230427 - Claire O'Neill - RE_ Latest sentencing files_.msg'; '20230505 - Charlotte Wallace - Updated prison projections .msg'
  path_cc_disposals     <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_0901/prisons_inputs/sentencing/_crown_actuals_and_inputs/_misc/cc_outputs/ccs_snapshot_20230315/cc_disposals.csv"   # '20230427 - Claire O'Neill - RE_ Latest sentencing files_.msg'; '20230505 - Charlotte Wallace - Updated prison projections .msg'

  # RDO data only goes back to January 2019 and the first two months of 2019
  # demonstrated extreme fluctuation not well fitted by resulting models. We,
  # therefore, start from March 2019.
  period_start          <- "2019-03-01"
  period_end            <- "2022-05-01"
  
  switch(
    mode,
    'save' = {
      disposals_mc <- import_disposals_mc(path_rdo_data, period_start, period_end)
      disposals_cc <- import_disposals_cc(path_cc_disposals, period_start, period_end)
      receptions   <- import_receptions(period_start, period_end)
      model_data <- dplyr::left_join(disposals_cc, disposals_mc, by = dplyr::join_by(date)) %>%
        dplyr::left_join(receptions, by = dplyr::join_by(date))
      botor::s3_write(model_data, readr::write_csv, path_receptions_model_data)
    },
    'report' = {},
    stop("Unrecognised mode, '", mode, "'.")
  )
  
  return(list(path_receptions_model_data = path_receptions_model_data, path_rdo_data = path_rdo_data,
              path_cc_disposals = path_cc_disposals, period_start = period_start,
              period_end = period_end))
}


# test-reception-rates.csv
# Created to save a table of coefficients for converting disposal types to
# receptions by sentence band. Used for developing code only.
# This function uses data exactly as used in the April 2023 prison projections,
# combined with the OLD FULLSAMPLE.
make_test_reception_rates <- function(mode = 'save') {
  
  #library(fullsample)
  
  path_rdo_data     <- "'s3://alpha-app-cjsm-monitoring/actuals_data_mags/[OFFICIAL SENSITIVE - Magistrates RDOs 19-01 to 22-12 (Jan19 to Dec22) as at 200123.xlsx]Data'!A6:G2936"   # '20230427 - Claire O'Neill - RE_ Latest sentencing files_.msg'
  path_cc_disposals <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_0901/prisons_inputs/sentencing/_crown_actuals_and_inputs/_misc/cc_outputs/ccs_snapshot_20230315/cc_disposals.csv"   # '20230427 - Claire O'Neill - RE_ Latest sentencing files_.msg'
  path_fullsample   <- "s3://alpha-prison-forecasting-data/FULLSAMPLE/FULLSAMPLE_230331.csv"   # '20230205 - Jordan Carroll - RE_ Which fullsample_.msg'
  period_start      <- "2019-03-01"
  period_end        <- "2022-05-01"

  path_reception_rates <- paste0("s3://alpha-prison-forecasting-data/prisons-ready-reckoner/prisonsreadyreckoner/test-files/test-reception-rates-", period_start, "-to-", period_end, ".csv")
  
  switch(
    mode,
    'save' = {
      #reception_rates <- prisonsreadyreckonerupdater::update_reception_rates(path_rdo_data, path_cc_disposals, period_start, period_end)
      reception_rates <- prisonsreadyreckonerupdater::update_reception_rates_old(path_rdo_data, path_cc_disposals, period_start, period_end, path_fullsample, visualise = FALSE)
      botor::s3_write(reception_rates$data, readr::write_csv, path_reception_rates)
    },
    'report' = {},
    stop("Unrecognised mode, '", mode, "'.")
  )
  
  return(list(path_reception_rates = path_reception_rates, path_rdo_data = path_rdo_data,
              path_cc_disposals = path_cc_disposals, period_start = period_start,
              period_end = period_end))
}


################################################################################
# Prison flow
################################################################################

# profiles_2019-03-14_to_2020-03-13.csv
# Created for testing the ability of profiles generated with the NEW FULLSAMPLE
# for producing forecasts that match an example prison projection.
# Not used in initial development because the prisons projection team were still
# using the OLD FULLSAMPLE.
# See visual-determinate-outflows.Rmd
make_test_determinate_profiles <- function(mode = 'save') {
  
  library(fullsample)
  
  period_start <- "2019-03-14"
  period_end   <- "2020-03-13"
  path_determinate_profiles <- paste0("s3://alpha-prison-forecasting-data/prisons-ready-reckoner/prisonsreadyreckoner/test-files/test-determinate-profiles-", period_start, "-to-", period_end, ".csv")
  
  switch(
    mode,
    'save' = {
      determinate_profiles <- prisonsreadyreckonerupdater::update_determinate_profiles(period_start, period_end, visualise = FALSE)
      botor::s3_write(determinate_profiles$data, readr::write_csv, path_determinate_profiles)
    },
    'report' = {},
    stop("Unrecognised mode, '", mode, "'.")
  )
  
  return(list(path_determinate_profiles = path_determinate_profiles, period_start = period_start,
              period_end = period_end))
}


# profiles_2019-03-14_to_2020-03-13.csv
# Created for testing the ability of profiles generated with the OLD FULLSAMPLE
# for producing forecasts that match an example prison projection.
# Used in initial development because the prisons projection team were still
# using the OLD FULLSAMPLE.
# See visual-determinate-outflows-old.Rmd
make_test_determinate_profiles_old <- function(mode = 'save') {
  
  period_start    <- "2022-01-01"  # '20230205 - Jordan Carroll - RE_ Which fullsample_.msg'
  period_end      <- "2022-12-31"  # '20230205 - Jordan Carroll - RE_ Which fullsample_.msg'
  path_determinate_profiles <- paste0("s3://alpha-prison-forecasting-data/prisons-ready-reckoner/prisonsreadyreckoner/test-files/test-determinate-profiles-old-", period_start, "-to-", period_end, ".csv")

  path_fullsample   <- "s3://alpha-prison-forecasting-data/FULLSAMPLE/FULLSAMPLE_230331.csv"   # '20230205 - Jordan Carroll - RE_ Which fullsample_.msg'

  switch(
    mode,
    'save' = {
      determinate_profiles <- prisonsreadyreckonerupdater::update_determinate_profiles_old(period_start, period_end, path_fullsample, visualise = FALSE)
      botor::s3_write(determinate_profiles$data, readr::write_csv, path_determinate_profiles)
    },
    'report' = {},
    stop("Unrecognised mode, '", mode, "'.")
  )
  
  return(list(path_determinate_profiles = path_determinate_profiles, path_fullsample = path_fullsample, period_start = period_start,
              period_end = period_end))
}




