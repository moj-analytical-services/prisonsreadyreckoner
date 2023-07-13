# Quickstart

# This script can be used to quickly load into the environment all base and
# scenario variables (i.e. those arguments into the run_model function). The idea
# behind this is that if individual functions need to be run or run_model needs
# to be run step by step using individual functions (e.g. for debugging), these
# variables can quickly be loaded

dev_set_params <- function() {
  
  params <- list()


  ################################################################################
  # Model levers and impact dates.
  # Levers are intended for use by a package user. The Shiny app will make use
  # of defaults only and set reactive values based on either defaults or inputs
  # from the Shiny GUI.
  ################################################################################
  
  # Number of extra police charges
  params$lever_police_charges_scenario           <- "central"
  
  # Number of extra court sitting days per month
  params$lever_extra_cc_sitting_days             <- 0 / 12             # [month^-1]
  params$lever_extra_cc_sitting_days_impact_date <- "2024-01-01"
  
  # Number of extra prison receptions per band per month
  params$lever_extra_inflows_det                 <- c(senband1 = 0, senband2 = 0, senband3 = 0, senband4 = 0)
  params$lever_extra_inflows_det_impact_date     <- "2024-01-01"
  
  # Factors for stretching profiles of time served in prison, broken down by sentence band
  params$lever_profiles_det_stretch_factors      <- c(senband1 = 1, senband2 = 1, senband3 = 1, senband4 = 1)
  params$lever_profiles_det_stretch_impact_date  <- "2024-01-01"
  
  # Recall
  params$lever_recall_rate                       <- signif(c(senband1 = 0.234774346793349, senband2 = 0.1003465003465, senband3 = 0.048357569686712, senband4 = 0.0213297380452349), 3)   # 20230505 - Charlotte Wallace - Updated prison projections .msg
  params$lever_recall_rate_impact_date           <- "2024-01-01"
  
  
  
  ################################################################################
  # General parameters.
  # General parameters are intended for use by a package user and the Shiny app.
  # A user may wish to change these but they will be fixed for individual
  # releases of the Shiny app.
  ################################################################################
  
  # Expected initial dates in various data files. The forecast will start from the
  # last consistent date.
  params$start_date$police_charges_cc  <- "2022-11-01"    # First month of the forecast. Used for verifying inputs.
  params$start_date$police_charges_mc  <- "2022-12-01"
  params$start_date$inflows_det        <- "2023-03-31"
  params$start_date$recall_rate        <- "2023-03-01"
  params$start_date$cc_files           <- "2022-11-01"
  
  
  params$projection_length_months <- 49         # To March 2027. '20230525 - To Ceri Cooper - Actions from our ready reckoner meeting just now_.msg'
  
  
  # Police charge parameters
  # Magistrates' courts
  params$police_charges_mc_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/020623_mags_sensitivity_output.xlsx"   # '202306026- Katie Mahon - RE_ mags outputs template.eml'
  params$police_charges_mc_scenarios <- list(central = "apr23_central",
                                             ramp_12m = "apr23_central_12m",
                                             ramp_36m = "apr23_central_36m",
                                             ramp_48m = "apr23_central_48m"
  )
  params$police_charges_mc_sheet <- "020623_mags_sensitivity_output-"

  # Crown Court
  params$police_charges_cc_files <- list(central = "s3://alpha-app-prisonsreadyreckonerapp/2023-04/April23_mid_pandemic_real_for_ready_reckoner.csv", # Records the total number of Crown Court receipts used as a baseline in the April 2023 projections. Includes background values from various sources, and an increase in charge numbers over a 24-month period. It has non-zero values throughout.
                                      ramp_12m = "s3://alpha-app-prisonsreadyreckonerapp/2023-04/April23_mid_pandemic_real_12m_ramp_for_ready_reckoner.csv", # Records how Crown Court receipts would change if police charge numbers were increased over a 12-month period instead of over a 24-month period. Records differences relative to the values in April23_mid_pandemic_real_for_ready_reckoner.csv.
                                      ramp_36m = "s3://alpha-app-prisonsreadyreckonerapp/2023-04/April23_mid_pandemic_real_36m_ramp_for_ready_reckoner.csv", # Records how Crown Court receipts would change if police charge numbers were increased over a 36-month period instead of over a 24-month period. Records differences relative to the values in April23_mid_pandemic_real_for_ready_reckoner.csv.
                                      ramp_48m = "s3://alpha-app-prisonsreadyreckonerapp/2023-04/April23_mid_pandemic_real_48m_ramp_for_ready_reckoner.csv" # Records how Crown Court receipts would change if police charge numbers were increased over a 48-month period instead of over a 24-month period. Records differences relative to the values in April23_mid_pandemic_real_for_ready_reckoner.csv.
  )   # '20230525 - David Verry - RE_ Next steps on the CJS ready reckoner.eml'

  # Table from Chun-Yee for converting Crown Court police charge tables
  params$police_charges_cc_route_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/routes.csv"   # '20230518 - Chun-yee Cheng - cjst pre-covid route distribution.msg'  
  
  
  # Crown Court parameters
  # Input paths
  params$cc_output_file          <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/crown-output-apr23_s4.csv"   # '20230502 - Chun-yee Cheng - RE_ Testing our assumptions with a second Crown Court scenario_.msg'
  params$cc_capacity_file        <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/sd_htpsd_adjusted_apr23_s4.csv"   # '20230502 - Chun-yee Cheng - RE_ Testing our assumptions with a second Crown Court scenario_.msg'
  
  # Paths to parameter tables
  params$ringfenced_lookup_file  <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/ringfenced-lookup-20230502-shiny-v0.0.0-OFFICIAL.csv"
  
  
  # Sentencing parameters
  # Remand rates.
  # # FYI, the Prisons team provided an intercept but this is not relevant for
  # # marginal changes. The following co-efficients were derived from backlog
  # # volumes calibrated to match HMCTS outstanding caseload but, for simplicity,
  # # we are applying them to unadjusted changes in non-ringfenced disposal
  # # volumes.
  # params$remand_rates <- tibble::tribble(
  #   ~receipt_type_desc, ~remand_rate,
  #   "ind",                     0.580,
  #   "tew",                     0.277,
  #   "app",                         0,
  #   "sent",                        0)   # '20230503 - Charlotte Wallace - RE_ Gender split and remand ratio_.msg'
  params$remand_rates <- c(receipts = 0.187429985453694, 
                          disposals = 0.475353729743915)   # '20230712 - To Jordan Carroll - RE_ Remand model comparison.msg'
  params$no_bail_rate <- 0.2                               # '20230705 - Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'
  params$ctl          <- 6                                 # [months] '20230705 - Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'
  params$mc_remand_lookup <- tibble::tribble(
    ~disposal_type, ~remanded,
    "mc_ind",       TRUE,
    "mc_tew",       TRUE,
    "mc_sm",        FALSE,
    "mc_snm",       FALSE)   # '20230705 - Jordan Carroll - RE_ Sitting Day Remand Impact Method.msg'
  
  params$sentencing_rates_file  <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/reception-rates-20230502-shiny-v0.0.0-OFFICIAL.csv"
  
  
  # Prison and licence parameters
  params$prison_inflows_file   <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/detailed_apr23_s4_apr23_flat_3m_linear_6m_msim_EDS_change_os_adj_DandS_3m_current_6m_avg_longterm_230426_150555.xlsx"   # '20230505 - Charlotte Wallace - Updated prison projections .msg'
  params$profiles_file         <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/profiles-det-20230502-shiny-v0.0.0-OFFICIAL.csv"
  
  # Recall/licence variables
  # Put back to previous version so recall rates are fixed values rather than time series
  params$recall_file           <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/recall_excl_PSS_output_apr23_s4_apr23_flat_3m_linear_6m_msim_EDS_change_os_adj_DandS_3m_current_6m_avg_longterm_230426_150555.xlsx"   # '20230505 - Charlotte Wallace - Updated prison projections .msg'
  #params$licence_times_file    <- "s3://alpha-probation-forecasting-data/Licence/time_spent_on_licence/average_time_on_licence_excl_pss_2023-02-28.csv"   # '20230419 - Danielle Kelly - RE_ Licence and recall parameters_.msg'
  params$licence_profiles_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/adjusted_license_profile.csv"   # '20230601 - Jordan Carroll - RE_ Your licence profiles_.msg'
  
  
  
  # Mean monthly inflow of indeterminate prisoners, taken from OMSQ. We assume
  # that indeterminates are never released
  params$inflows_indet_mean   <- 299 / 12   # OMSQ Oct-Dec 2022, Table 2.5a, last four quarters
  
  
  # Gender splits
  params$gender_splits_file      <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/gender-splits-20230502-shiny-v0.0.0-OFFICIAL.csv"
 
  return(params) 
}
