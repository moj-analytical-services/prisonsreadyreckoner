# This script tests functions for predicting prison receptions from court
# disposals.

library(magrittr)
#library(fullsample)


################################################################################
# Linear regression
################################################################################


# Function for creating dummy data.
# This function simply reads in data exactly as performed by
# update_reception_rates().
test_that("functions for reading receptions data will complete without error", {

  path_rdo_data <- "'s3://alpha-app-cjsm-monitoring/actuals_data_mags/[OFFICIAL SENSITIVE - Magistrates RDOs 19-01 to 22-12 (Jan19 to Dec22) as at 200123.xlsx]Data'!A6:G2936"
  path_cc_disposals <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_0901/prisons_inputs/sentencing/_crown_actuals_and_inputs/_misc/cc_disposals_ccs_snapshot_20230215_fixed.csv"
  path_fullsample   <- "s3://alpha-prison-forecasting-data/FULLSAMPLE/FULLSAMPLE_230331.csv"   # '20230502 - Jordan Carroll - RE_ Which fullsample_.msg'
  
  # The following may be commented to refresh the data. However, doing so every
  # is time-consuming.
  skip("Skipping receptions data reading. To re-test, comment out the skip() in the test-receptions.R script.")
  
  period_start <- "2019-02-01"
  period_end   <- "2022-05-01"    # We restrict to avoid the period of temporarily raised magistrates' sentencing powers.

  disposals_mc <- expect_no_error(import_disposals_mc(path_rdo_data, period_start, period_end))
  disposals_cc <- expect_no_error(import_disposals_cc(path_cc_disposals, period_start, period_end))
  #receptions   <- expect_no_error(import_receptions(period_start, period_end))
  receptions   <- expect_no_error(import_receptions_old(period_start, period_end, path_fullsample))
  
})


# The following is commented only because it relies on functions in what is now
# the updater package.

# Applying a linear model to predict prison receptions.
# The predicted prison receptions are compared with the model fit on which the
# model is based. Equality demonstrates that the model has been applied
# correctly.
# This function simultaneously tests the consistency of
#   * estimate_lm_receptions(model_data)
#   * make_reception_rates_table(lm_receptions)
#   * calculate_receptions(disposals, reception_rates)
# as only if they all work will the output equal the values fitted from the
# intended linear regression model.
# test_that("calculate_inflows_det_delta() will correctly apply a linear model to generate a prison reception forecast", {
# 
#   path_receptions_model <- "s3://alpha-prison-forecasting-data/flowcast/test-files/test-receptions-model-data.csv"
#   
#   silence_botor()
#   
#   # Read data calculated in previous test.
#   model_data  <- botor::s3_read(path_receptions_model, readr::read_csv, show_col_types = FALSE)
#   
#   # Construct a linear regression from the data.
#   lm_sb1 <- lm(senband1 ~ 0 + cc_effective + cc_egp + cc_lcgp + cc_sent + mc_sm + mc_snm + mc_tew, model_data)
#   lm_sb2 <- lm(senband2 ~ 0 + cc_effective + cc_egp + cc_lcgp + cc_sent + mc_tew, model_data)
#   lm_sb3 <- lm(senband3 ~ 0 + cc_effective + cc_gp + cc_sent, model_data)
#   lm_sb4 <- lm(senband4 ~ 0 + cc_effective + cc_gp + cc_sent, model_data)
# 
#   # Separately construct a prediction using package functions. The following
#   # tests rely on the functions estimate_lm_receptions() and
#   # make_reception_rates_table() from the prisonsreadyreckonerupdater package.
#   lm_receptions <- estimate_lm_receptions(model_data)
#   reception_rates <- make_reception_rates_table(lm_receptions)
# 
#   disposal_types <- c("cc_effective", "cc_egp", "cc_gp_cracked", "cc_lgp",
#                       "cc_sent", "mc_tew", "mc_ind", "mc_sm", "mc_snm")
#   disposals <- dplyr::select(model_data, c("date", dplyr::all_of(disposal_types))) %>%
#                  tidyr::pivot_longer(tidyr::all_of(disposal_types),
#                                      names_to = "disposal_type",
#                                      values_to = "n_disposals_delta")
#   receptions <- calculate_inflows_det_delta_SUB(disposals, reception_rates)
#   
#   
#   # Compare.
#   y_hat = dplyr::filter(receptions, senband == "senband1") %>% dplyr::select(-c("senband"))
#   expect_equal(as.numeric(y_hat), as.numeric(lm_sb1$fitted.values))
# 
#   y_hat = dplyr::filter(receptions, senband == "senband2") %>% dplyr::select(-c("senband"))
#   expect_equal(as.numeric(y_hat), as.numeric(lm_sb2$fitted.values))
#   
#   y_hat = dplyr::filter(receptions, senband == "senband3") %>% dplyr::select(-c("senband"))
#   expect_equal(as.numeric(y_hat), as.numeric(lm_sb3$fitted.values))
#   
#   y_hat = dplyr::filter(receptions, senband == "senband4") %>% dplyr::select(-c("senband"))
#   expect_equal(as.numeric(y_hat), as.numeric(lm_sb4$fitted.values))
#   
# })
