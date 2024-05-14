source('~/projects/prisonsreadyreckoner/tests/sandbox/dev-params.R')
source("~/projects/prisonsreadyreckoner/R/zzz.R")
source("~/projects/prisonsreadyreckoner/R/utils.R")
source("~/projects/prisonsreadyreckoner/R/sentencing.R")
source("~/projects/prisonsreadyreckoner/R/run.R")
source("~/projects/prisonsreadyreckoner/R/prison.R")
source("~/projects/prisonsreadyreckoner/R/police-charges.R")
source("~/projects/prisonsreadyreckoner/R/params.R")
source("~/projects/prisonsreadyreckoner/R/mojstockr.R")
source("~/projects/prisonsreadyreckoner/R/load.R")
source("~/projects/prisonsreadyreckoner/R/levers.R")
source("~/projects/prisonsreadyreckoner/R/crown.R")

silence_botor()
params <- format_params(dev_set_params())

### Load datasets that are fixed throughout the model ###
loaded_datasets_list             <- load_datasets(params)

cc_receipts_delta_loaded_list    <- loaded_datasets_list$cc_receipts_delta_loaded_list
mc_disposals_delta_loaded_list   <- loaded_datasets_list$mc_disposals_delta_loaded_list
cc_backlog_loaded                <- loaded_datasets_list$cc_backlog_loaded
mc_backlog_loaded                <- loaded_datasets_list$mc_backlog_loaded
remand_coefficients              <- loaded_datasets_list$remand_coefficients
cc_output_loaded                 <- loaded_datasets_list$cc_output
cc_capacity_loaded               <- loaded_datasets_list$cc_capacity
sentencing_rates_loaded          <- loaded_datasets_list$sentencing_rates
inflows_det_loaded               <- loaded_datasets_list$inflows_det
profiles_det_loaded              <- loaded_datasets_list$profiles_det
nomis_out_delius_in_ratio        <- loaded_datasets_list$nomis_out_delius_in_ratio
profiles_lic                     <- loaded_datasets_list$profiles_lic
recall_rate_exclPSS              <- loaded_datasets_list$recall_rate_exclPSS
average_time_on_recall           <- loaded_datasets_list$average_time_on_recall
recall_profile_adjustments       <- loaded_datasets_list$recall_profile_adjustments
gender_splits                    <- loaded_datasets_list$gender_splits

# If the below is set to "central" then get 0s for cc_disposals_delta$n_receipts_delta 
# and cc_disposals_delta$n_disposals_delta. If either of the other two scenarios are 
# selected ("sep23_high" or "sep23_low"), then there are non-zero values present
params$lever_police_charges_scenario <- "sep23_high"

cc_receipts_delta  <- cc_receipts_delta_loaded_list[[params$lever_police_charges_scenario]]
mc_disposals_delta <- mc_disposals_delta_loaded_list[[params$lever_police_charges_scenario]]

cc_capacity_levered  <- add_cc_sitting_days(cc_capacity_loaded, params$lever_extra_cc_sitting_days, params$lever_extra_cc_sitting_days_impact_date)

if (is.data.frame(cc_receipts_delta)) {
  cc_output <- add_cc_receipts_delta(cc_output_loaded, cc_receipts_delta)
  cc_capacity <- calculate_hours_ringfenced_delta(cc_output, cc_capacity_levered)
}else{
  cc_output <- cc_ouput_loaded
  cc_capacity <- cc_capacity_levered
}

cc_disposals_delta <- calculate_cc_disposals_delta(cc_output, cc_capacity)


#Put into the variable names for the calculate_pop_demand_delta function
cc_base_backlog   <- cc_backlog_loaded
mc_base_backlog   <- mc_backlog_loaded
remand_coeff      <- remand_coefficients
cc_scenario_delta <- cc_disposals_delta

