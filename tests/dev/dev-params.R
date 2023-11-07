# This script can be used to quickly load into the environment all base and
# scenario variables (i.e. those arguments into the run_model function). Useful
# if individual functions need to be run or run_model needs
# to be run step by step using individual functions (e.g. for debugging).

# Developed for prisonsreadyreckoner, version 3.0.0.


dev_set_params <- function() {
  
  params <- list()
  
  ################################################################################
  # Default model levers and impact dates.
  # Levers are intended for use by a package user. The Shiny app will make use
  # of defaults only and set reactive values based on either defaults or inputs
  # from the Shiny GUI.
  ################################################################################
  
  # Number of extra police charges
  params$lever_police_charges_scenario           <- "central"   # FYI, the default is always 'central'.
  
  # Number of extra court sitting days per month
  params$lever_extra_cc_sitting_days             <- 0 / 12             # [month^-1]
  params$lever_extra_cc_sitting_days_impact_date <- "2024-01-01"
  
  # Number of extra prison receptions per band per month
  params$lever_extra_inflows_det                 <- c(senband1 = 0, senband2 = 0, senband3 = 0, senband4 = 0)
  params$lever_extra_inflows_det_impact_date     <- "2024-01-01"
  
  # Factors for stretching profiles of time served in prison, broken down by sentence band
  params$lever_profiles_det_stretch_factors      <- c(senband1 = 1, senband2 = 1, senband3 = 1, senband4 = 1)
  params$lever_profiles_det_stretch_impact_date  <- "2024-01-01"
  
  # Recall. Default recall rate should be the same as in the recall rate table.
  params$lever_recall_rate                       <- signif(c(senband1 = 0.22383363, senband2 = 0.172520025, senband3 = 0.053505765, senband4 = 0.022550146), 3)   # '20231025 - Alex Dickinson - Re_ New files for ready reckoner.msg'
  params$lever_recall_rate_impact_date           <- "2024-01-01"
  
  # Factors for stretching profiles of time in recall, broken down by sentence band
  params$lever_profiles_recall_stretch_factors      <- c(senband1 = 1, senband2 = 1, senband3 = 1, senband4 = 1)
  params$lever_profiles_recall_stretch_impact_date  <- "2024-01-01"
  
  
  ################################################################################
  # Shiny app parameters. For use by the Shiny app only. 
  ################################################################################
  
  # Instructions:
  # 1. Copy and paste from prisonsreadyreckoner sandbox output.
  
  params$tot_pop_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/populations_sep23_s2_flat_3m_linear_6m_sep23_central_scenario_dands_231011_081331-s039-OFFICIAL.xlsx"
  # params$capacity_file <- paste0("s3://alpha-app-prisonsreadyreckonerapp/2023-04B/May-23 Supply Forecasts_V1.5_Shared.xlsx")
  params$prison_population_hist_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/historical-prison-population-20231016-shiny-v3.0.0-OFFICIAL.csv"
  params$police_charges_hist_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/historical-police-charges-20231016-shiny-v3.0.0-OFFICIAL.csv"
  params$sitting_days_hist_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/sitting_days_actuals.csv"
  params$prison_inflows_hist_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/historical-prison-inflows-20231016-shiny-v3.0.0-OFFICIAL.csv"
  params$time_served_det_hist_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/historical-time-served-20231016-shiny-v3.0.0-OFFICIAL.csv"
  params$recall_rate_hist_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/historic_recall_rate_band_aug23.csv"
  params$time_served_recall_hist_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/historical-recall-time-served-20231016-shiny-v3.0.0-OFFICIAL.csv"
  params$police_charges_mc_central_plot_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/src/201023_mags_sensitivity_output.csv"
  params$police_charges_mc_central_plot_scenario <- "sep23_central"
  
  
  ################################################################################
  # General parameters.
  # General parameters are intended for use by a package user and the Shiny app.
  # A user may wish to change these but they will be fixed for individual
  # releases of the Shiny app.
  ################################################################################
  
  # Instructions:
  # 1. Copy and paste from prisonsreadyreckoner sandbox output.
  
  params$ringfenced_lookup_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/ringfenced-lookup-20231016-shiny-v3.0.0-OFFICIAL.csv"
  params$police_charges_mc_files <- c("s3://alpha-app-prisonsreadyreckonerapp/2023-09/police-charges-mc001-20231016-shiny-v3.0.0-OFFICIAL.csv")
  params$police_charges_cc_files <- c("s3://alpha-app-prisonsreadyreckonerapp/2023-09/police-charges-cc001-20231016-shiny-v3.0.0-OFFICIAL.csv")
  params$police_charges_cc_route_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/routes.csv"
  params$cc_output_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/crown-output-sep23_s2.csv"
  params$cc_capacity_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/sitting-days-output-sep23_s2.csv"
  params$sentencing_rates_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/sentencing-rates-20231016-shiny-v3.0.0-OFFICIAL.csv"
  params$prison_inflows_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/detailed_sep23_s2_flat_3m_linear_6m_sep23_central_scenario_dands_231011_081331-s039-OFFICIAL.xlsx"
  #params$profiles_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/profiles-det-20231016-shiny-v3.0.0-OFFICIAL.csv"
  params$profiles_file <- "s3://alpha-prison-forecasting-data/prisons-ready-reckoner/prisonsreadyreckoner/test-files/test-determinate-profiles-old-2022-01-01-to-2022-12-31.csv"
  params$recall_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/recall_excl_pss_output_sep23_s2_flat_3m_linear_6m_sep23_central_scenario_dands_231011_081331-s039-OFFICIAL.xlsx"
  params$gender_splits_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-09/gender-splits-20231016-shiny-v3.0.0-OFFICIAL.csv"

  # Expected initial dates in various data files. The forecast will start from the
  # last consistent date.
  # DEVELOPMENT NOTE: The first dates in relevant files are currently tested on
  # reading for consistency with these dates as a simple way of confirming that
  # the intended input files are being provided. The value of this measure is
  # diminished now that path parameters may be generated automatically with the
  # prisonsreadyreckonerupdater package. Consider replacing with simply the
  # maximum start date from all relevant files and using this for setting the
  # forecast start date. Would require updating several loading functions.
  params$start_date$police_charges_mc  <- "2023-06-01"    # First month of the forecast. Used for verifying inputs.
  params$start_date$police_charges_cc  <- "2023-05-01"
  params$start_date$inflows_det        <- "2023-09-30"
  params$start_date$recall_rate        <- NA
  params$start_date$cc_files           <- "2023-04-01"
  
  params$projection_length_months <- 49         # To September 2027. '20230525 - To Ceri Cooper - Actions from our ready reckoner meeting just now_.msg'
  
  # Sentencing parameters
  # Remand rates.
  params$published_remand_pop <- 14815 + 1255                    # OMSQ Apr-Jun 2023, Table 1.1, Adults + 18-20 year olds, 30 September 2023. For QA purposes only.
  params$remand_rates <- c(receipts = 0.187429985453694, 
                           disposals = 0.475353729743915)  # '20230712 - To Jordan Carroll - RE_ Remand model comparison.msg'
  params$no_bail_rate <- 0.2                               # '20230705 - Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'
  params$ctl          <- 6                                 # [months] '20230705 - Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'
  params$mc_remand_lookup <- tibble::tribble(
    ~disposal_type, ~remanded,
    "mc_ind",       TRUE,
    "mc_tew",       TRUE,
    "mc_sm",        FALSE,
    "mc_snm",       FALSE)   # '20230705 - Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'
  
  # Mean monthly inflow of indeterminate prisoners, taken from OMSQ. We assume
  # that indeterminates are never released
  params$inflows_indet_mean   <- 289 / 12   # OMSQ Apr-Jun 2023, Table 2.5a, Adults + 18-20 year olds, last four quarters.

  return(params) 
}
