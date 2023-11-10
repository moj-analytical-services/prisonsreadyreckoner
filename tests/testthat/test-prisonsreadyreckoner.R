# This script performs general tests on the behaviour of the whole mode (i.e.
# run_prisonsreadyreckoner()) with respect to levers pulled.

source("../sandbox/dev-params.R")

# For loading standard parameters and setting the levers to 
set_test_params <- function() {
  
  silence_botor()
  
  params <- dev_set_params()
  params <- format_params(params)
  
  # Reset recall to whatever is in the source file.
  recall_rate_exclPSS <- load_recall_rate_exclPSS(params$recall_file, params$start_date$recall_rate, params$forecast_start_date, params$forecast_end_date)
  params$lever_recall_rate <- signif(recall_rate_exclPSS, 3)

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
  
  
  # Sense check: Add 5% / 12 of the annual sitting days per month and observe
  # the sentenced prison population by band (not remand or recall). Add the same
  # again and check that the additional prison population has doubled.
  params$lever_extra_cc_sitting_days <- sitting_days_base * 0.05
  pop_combined0 <- run_prisonsreadyreckoner(params)
  #dev_plot_population(pop_combined0, "determinate", "Determinate, 5% more sitting days")
  
  params$lever_extra_cc_sitting_days <- sitting_days_base * 0.1
  pop_combined1 <- run_prisonsreadyreckoner(params)
  #dev_plot_population(pop_combined1, "determinate", "Determinate, 10% more sitting days")
  
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
  # Here we are testing within a 1% error.
  expect_true(pop_combined$population[pop_combined$run == "scenario" & pop_combined$senband == "senband1"] /
               (1.1 * pop_combined$population[pop_combined$run == "baseline" & pop_combined$senband == "senband1"]) - 1 < 0.01)
  expect_true(pop_combined$population[pop_combined$run == "scenario" & pop_combined$senband == "senband2"] /
               (1.1 * pop_combined$population[pop_combined$run == "baseline" & pop_combined$senband == "senband2"]) - 1 < 0.01)
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

