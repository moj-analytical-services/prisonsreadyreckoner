# This script performs general tests on the behaviour of the whole mode (i.e.
# run_prisonsreadyreckoner()) with respect to levers pulled.

source("../dev/dev-params.R")

# For loading standard parameters and setting the levers to 
set_test_params <- function() {
  
  silence_botor()
  
  # params <- list()
  # 
  # # Number of extra police charges
  # params$lever_police_charges_scenario           <- "central"
  # 
  # # Number of extra court sitting days per month
  # params$lever_extra_cc_sitting_days             <- 0             # [month^-1]
  # params$lever_extra_cc_sitting_days_impact_date <- "2024-01-01"
  # 
  # # Number of extra prison receptions per band per month
  # params$lever_extra_inflows_det                 <- c(senband1 = 0, senband2 = 0, senband3 = 0, senband4 = 0)
  # params$lever_extra_inflows_det_impact_date     <- "2024-01-01"
  # 
  # # Factors for stretching profiles of time served in prison, broken down by sentence band
  # params$lever_profiles_det_stretch_factors      <- c(senband1 = 1, senband2 = 1, senband3 = 1, senband4 = 1)
  # params$lever_profiles_det_stretch_impact_date  <- "2024-01-01"
  # 
  # # Recall
  # params$lever_recall_rate                       <- c(senband1 = 0.2, senband2 = 0.1, senband3 = 0.05, senband4 = 0.02)
  # params$lever_recall_rate_impact_date           <- "2024-01-01"
  # 
  # 
  # 
  # # Expected initial dates in various data files. The forecast will start from the
  # # last consistent date.
  # params$start_date$police_charges_cc  <- "2022-11-01"    # First month of the forecast. Used for verifying inputs.
  # params$start_date$police_charges_mc  <- "2022-12-01"
  # params$start_date$inflows_det        <- "2023-03-31"
  # params$start_date$recall_rate        <- "2023-03-01"
  # params$start_date$cc_files           <- "2022-11-01"
  # 
  # params$projection_length_months <- 60
  # 
  # 
  # # Police charge parameters
  # # Magistrates' courts
  # params$police_charges_mc_file <- "s3://alpha-app-prisonsreadyreckonerapp/2023-04/020623_mags_sensitivity_output.xlsx"
  # params$police_charges_mc_scenarios <- list(central = "apr23_central",
  #                                            ramp_12m = "apr23_central_12m",
  #                                            ramp_36m = "apr23_central_36m",
  #                                            ramp_48m = "apr23_central_48m"
  # )
  # params$police_charges_mc_sheet <- "020623_mags_sensitivity_output-"
  # 
  # # Crown Court
  # params$police_charges_cc_files <- list(central = "s3://alpha-prison-forecasting-data/flowcast/data/police-demand-scenarios/April23_mid_pandemic_real_for_ready_reckoner.csv", # Records the total number of Crown Court receipts used as a baseline in the April 2023 projections. Includes background values from various sources, and an increase in charge numbers over a 24-month period. It has non-zero values throughout.
  #                                        ramp_12m = "s3://alpha-prison-forecasting-data/flowcast/data/police-demand-scenarios/April23_mid_pandemic_real_12m_ramp_for_ready_reckoner.csv", # Records how Crown Court receipts would change if police charge numbers were increased over a 12-month period instead of over a 24-month period. Records differences relative to the values in April23_mid_pandemic_real_for_ready_reckoner.csv.
  #                                        ramp_36m = "s3://alpha-prison-forecasting-data/flowcast/data/police-demand-scenarios/April23_mid_pandemic_real_36m_ramp_for_ready_reckoner.csv", # Records how Crown Court receipts would change if police charge numbers were increased over a 36-month period instead of over a 24-month period. Records differences relative to the values in April23_mid_pandemic_real_for_ready_reckoner.csv.
  #                                        ramp_48m = "s3://alpha-prison-forecasting-data/flowcast/data/police-demand-scenarios/April23_mid_pandemic_real_48m_ramp_for_ready_reckoner.csv" # Records how Crown Court receipts would change if police charge numbers were increased over a 48-month period instead of over a 24-month period. Records differences relative to the values in April23_mid_pandemic_real_for_ready_reckoner.csv.
  # )   # '20230525 - David Verry - RE_ Next steps on the CJS ready reckoner.eml'
  # 
  # # Table from Chun-Yee for converting Crown Court police charge tables
  # params$police_charges_cc_route_file <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_apr23_v1/crown_court_modelling/ccs_snapshot_20230125_jan23version/routes.csv"   # '20230518 - Chun-yee Cheng - cjst pre-covid route distribution.msg'  
  # 
  # 
  # # Crown Court parameters
  # # Input paths
  # params$cc_output_file          <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_apr23_v1/model_outputs/sensitivity_baseline_230412_0819/crown-output-apr23_s4.csv"   # '20230502 - Chun-yee Cheng - RE_ Testing our assumptions with a second Crown Court scenario_.msg'
  # params$cc_capacity_file        <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_apr23_v1/model_outputs/sensitivity_baseline_230412_0819/sd_htpsd_adjusted_apr23_s4.csv"   # '20230502 - Chun-yee Cheng - RE_ Testing our assumptions with a second Crown Court scenario_.msg'
  # 
  # # Paths to parameter tables
  # params$ringfenced_lookup_file  <- "s3://alpha-prison-forecasting-data/flowcast/data/ringfenced-lookup-20230502-shiny-v0.0.0-OFFICIAL.csv"
  # 
  # 
  # # Sentencing parameters
  # # Remand rates. FYI, the Prisons team provided an intercept but this is not
  # # relevant for marginal changes. The following co-efficients were derived from
  # # backlog volumes calibrated to match HMCTS outstanding caseload but, for
  # # simplicity, we are applying them to unadjusted changes in non-ringfenced
  # # disposal volumes.
  # params$remand_rates <- tibble::tribble(
  #   ~receipt_type_desc, ~remand_rate,
  #   "ind",                     0.580,
  #   "tew",                     0.277,
  #   "app",                         0,
  #   "sent",                        0)   # 20230503 - Charlotte Wallace - RE_ Gender split and remand ratio_.msg
  # 
  # params$sentencing_rates_file <- "s3://alpha-prison-forecasting-data/flowcast/data/reception-rates-20230502-shiny-v0.0.0-OFFICIAL.csv"
  # 
  # 
  # # Prison and licence parameters
  # params$prison_inflows_file  <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_0901/model_outputs/_prison_outputs/_sensitivity/extra_outputs/detailed_apr23_s4_apr23_flat_3m_linear_6m_msim_EDS_change_os_adj_DandS_3m_current_6m_avg_longterm_230426_150555.xlsx"   # '20230505 - Charlotte Wallace - Updated prison projections .msg'
  # params$profiles_file        <- "s3://alpha-prison-forecasting-data/flowcast/data/profiles-det-20230502-shiny-v0.0.0-OFFICIAL.csv"
  # 
  # # Recall/licence variables
  # # Put back to previous version so recall rates are fixed values rather than time series
  # #params$recall_file          <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_0901/model_outputs/_prison_outputs/_sensitivity/extra_outputs/recall_excl_PSS_output_apr23_s4_apr23_flat_3m_linear_6m_msim_EDS_change_os_adj_230424_134433.xlsx"
  # params$recall_file          <- "s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_0901/model_outputs/_prison_outputs/_sensitivity/extra_outputs/recall_excl_PSS_output_apr23_s4_apr23_flat_3m_linear_6m_msim_EDS_change_os_adj_DandS_3m_current_6m_avg_longterm_230426_150555.xlsx"   # '20230505 - Charlotte Wallace - Updated prison projections .msg'
  # #params$licence_times_file   <- "s3://alpha-probation-forecasting-data/Licence/time_spent_on_licence/average_time_on_licence_excl_pss_2023-02-28.csv"   # '20230419 - Danielle Kelly - RE_ Licence and recall parameters_.msg'
  # params$licence_profiles_file <- "s3://alpha-prison-forecasting-data/JC_test/recall/adjusted_license_profile.csv"   # '20230601 - Jordan Carroll - RE_ Your licence profiles_.msg'
  # 
  # 
  # # Mean monthly inflow of indeterminate prisoners, taken from OMSQ. We assume
  # # that indeterminates are never released
  # params$inflows_indet_mean   <- 299 / 12   # OMSQ Oct-Dec 2022, Table 2.5a
  # 
  # 
  # # Gender splits
  # params$gender_splits_file      <- "s3://alpha-prison-forecasting-data/flowcast/data/gender-splits-20230502-shiny-v0.0.0-OFFICIAL.csv"
  
  
  params <- dev_set_params()
  params <- format_params(params)

  # Reset recall to whatever is in the source file.
  recall_params <- load_recall_params(params$recall_file, params$start_date$recall_rate, params$forecast_start_date, params$forecast_end_date)
  params$lever_recall_rate <- signif(recall_params$recall_rate_exclPSS, 3)

  return(params)
}


test_that("run_prisonsreadyreckoner() prison populations respond linearly when sitting days are added", {

  # Background:
  # In the 2021 calendar year 99,855 sitting days were used (the allocation for
  # 2021/22 was 105,000)
  # [https://researchbriefings.files.parliament.uk/documents/CBP-8372/CBP-8372.pdf].
  #
  # Chun-yee's file,
  # s3://alpha-app-criminal-scenario-tool/developmentFolder/Dev_apr23_v1/model_outputs/sensitivity_baseline_230412_0819/sd_htpsd_adjusted_apr23_s4.csv
  # suggests 101,884 sitting days in the first twelve months of the forecast.
  
  params <- set_test_params()
  
  silence_botor()
  
  cc_capacity <- load_cc_capacity(params$cc_capacity_file, params$start_date$cc_files, params$forecast_start_date, params$forecast_end_date)
  
  sitting_days_base <- sum(cc_capacity$sitting_days) / params$projection_length_months
  
  
  # Sense check: Add 10% / 12 of the annual sitting days per month and observe
  # the sentenced prison population by band (not remand or recall). Add the same
  # again and check that the additional prison population has doubled.
  params$lever_extra_cc_sitting_days <- sitting_days_base * 0.1
  pop_combined0 <- run_prisonsreadyreckoner(params)
  #dev_plot_population(pop_combined0, "determinate", "Determinate, 10% more sitting days")
  
  params$lever_extra_cc_sitting_days <- sitting_days_base * 0.2
  pop_combined1 <- run_prisonsreadyreckoner(params)
  #dev_plot_population(pop_combined1, "determinate", "Determinate, 20% more sitting days")
  
  pop_delta0 <- sum(pop_combined0$population[pop_combined0$run == "scenario" & pop_combined0$casetype == "determinate" & pop_combined0$sex == "All"]) - sum(pop_combined0$population[pop_combined0$run == "baseline" & pop_combined0$casetype == "determinate" & pop_combined0$sex == "All"])
  pop_delta1 <- sum(pop_combined1$population[pop_combined1$run == "scenario" & pop_combined1$casetype == "determinate" & pop_combined1$sex == "All"]) - sum(pop_combined1$population[pop_combined1$run == "baseline" & pop_combined1$casetype == "determinate" & pop_combined1$sex == "All"])
  expect_equal(pop_delta1, 2*pop_delta0)
  
  pop_delta0 <- sum(pop_combined0$population[pop_combined0$run == "scenario"]) - sum(pop_combined0$population[pop_combined0$run == "baseline"])
  pop_delta1 <- sum(pop_combined1$population[pop_combined1$run == "scenario"]) - sum(pop_combined1$population[pop_combined1$run == "baseline"])
  expect_equal(pop_delta1, 2*pop_delta0)

})


test_that("run_prisonsreadyreckoner() will respond appropriately when additional inflows are added", {
  
  params <- set_test_params()
  
  silence_botor()
  
  inflows_det <- load_inflows_det(params$prison_inflows_file, params$start_date$inflows_det, params$forecast_start_date, params$forecast_end_date)

  # Find the inflows in the last month of the file.
  inflows_det_base <- tibble::deframe(inflows_det[, c(1,params$projection_length_months + 1)])
  
  
  # Sense check: Add 10% inflows per month and observe the sentenced prison
  # population by band (not remand or recall).
  params$lever_extra_inflows_det <- inflows_det_base * 0.1
  pop_combined <- run_prisonsreadyreckoner(params)
  #dev_plot_population(pop_combined, "determinate", "Determinate, 10% more inflows")
  
  # NB, we ignore sendband 4 as it is unlikely to be close to steady state.
  pop_combined <- dplyr::filter(pop_combined, casetype == "determinate", senband != "senband4", sex == "All", date == max(date)) %>%
                    dplyr::group_by(run, senband) %>%
                    dplyr::summarise(population = sum(population), .groups = "drop")
  
  # We expect final prison population to increase by the same factor as inflows.
  # Here we are testing within a rounding tolerance or a percent error.
  expect_equal(round(pop_combined$population[pop_combined$run == "scenario" & pop_combined$senband == "senband1"], -1),
               round(1.1 * pop_combined$population[pop_combined$run == "baseline" & pop_combined$senband == "senband1"], -1))
  expect_equal(round(pop_combined$population[pop_combined$run == "scenario" & pop_combined$senband == "senband2"], -1),
               round(1.1 * pop_combined$population[pop_combined$run == "baseline" & pop_combined$senband == "senband2"], -1))
  expect_true(pop_combined$population[pop_combined$run == "scenario" & pop_combined$senband == "senband3"] /
              (1.1 * pop_combined$population[pop_combined$run == "baseline" & pop_combined$senband == "senband3"]) - 1 < 0.01)
                
  
})


test_that("run_prisonsreadyreckoner() will respond appropriately when profiles are stretched", {
  
  params <- set_test_params()
  
  silence_botor()
  
  profiles_det <- load_profiles_det(params$profiles_file, params$projection_length_months, params$lever_profiles_det_stretch_factor_min)
  
  # Sense check: Stretch determinate sentence profiles and observe the sentenced
  # prison population by band (not remand or recall).
  params$lever_profiles_det_stretch_factors <- c(senband1 = 1.1, senband2 = 1.1, senband3 = 1.1, senband4 = 1.1)
  pop_combined <- run_prisonsreadyreckoner(params)
  #dev_plot_population(pop_combined, "determinate", "Determinate, 10% longer profiles")
  
  # NB, we ignore sendband 4 as it is unlikely to be close to steady state.
  pop_combined <- dplyr::filter(pop_combined, casetype == "determinate", senband != "senband4", sex == "All", date == max(date)) %>%
    dplyr::group_by(run, senband) %>%
    dplyr::summarise(population = sum(population), .groups = "drop")
  
  # We expect final prison population to increase by the same factor as the
  # stretch.
  # Here we are testing within a 5% percent error.
  expect_true(pop_combined$population[pop_combined$run == "scenario" & pop_combined$senband == "senband1"] /
                (1.1 * pop_combined$population[pop_combined$run == "baseline" & pop_combined$senband == "senband1"]) - 1 < 0.05)
  expect_true(pop_combined$population[pop_combined$run == "scenario" & pop_combined$senband == "senband2"] /
                (1.1 * pop_combined$population[pop_combined$run == "baseline" & pop_combined$senband == "senband2"]) - 1 < 0.05)
  expect_true(pop_combined$population[pop_combined$run == "scenario" & pop_combined$senband == "senband3"] /
                (1.1 * pop_combined$population[pop_combined$run == "baseline" & pop_combined$senband == "senband3"]) - 1 < 0.05)
  
  
})

