

#' Run \code{prisonsreadyreckoner} in its entirety.
#' 
#' @export
run_prisonsreadyreckoner <- function(params) {
  
  silence_botor()
  
  ### Set up parameters in correct format ###
  params <- format_params(params)
  
  ### Load datasets that are fixed throughout the model ###
  loaded_datasets_list             <- load_datasets(params)
  
  cc_receipts_delta_loaded_list    <- loaded_datasets_list$cc_receipts_delta_loaded_list
  mc_disposals_delta_loaded_list   <- loaded_datasets_list$mc_disposals_delta_loaded_list
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

  # # Defaults not used by the package but made available for the Shiny app.
  # # Shiny equivalent to assign reactives to defaults here.
  # defaults <- set_defaults(params, recall_rate_exclPSS)

  
  # ### Calculate variables that are fixed throughout the model ###
  # Make filters for remand population impacts.
  profiles_remand_in  <- make_remand_filter_in(params$remand_rates[['receipts']], params$no_bail_rate, params$ctl, params$projection_length_months)
  profiles_remand_out <- make_remand_filter_out(params$remand_rates[['disposals']], params$no_bail_rate, params$ctl, params$projection_length_months)
  
  # Make filters for recall outflows.
  recall_time     <- multiply_two_named_vectors(average_time_on_recall, recall_profile_adjustments, arguments_to_keep = c("senband1", "senband2", "senband3", "senband4"))
  profiles_recall <- make_lag_filters(recall_time)

  # Calculate baseline with only original inflows.
  pop_baseline <- run_baseline(params, inflows_det_loaded, profiles_det_loaded,
                               nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, profiles_recall)

  
  t0 <- Sys.time()
  
  # Run scenario and find overall population changes.
  #pop_scenario <- run_scenario(params, cc_receipts_delta_loaded_list, cc_output_loaded, cc_capacity_loaded, mc_disposals_delta_loaded_list, sentencing_rates_loaded, inflows_det_loaded, profiles_det_loaded, nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, profiles_recall)
  pop_scenario <- run_scenario(params, cc_receipts_delta_loaded_list, cc_output_loaded, cc_capacity_loaded, mc_disposals_delta_loaded_list, profiles_remand_in, profiles_remand_out, sentencing_rates_loaded, inflows_det_loaded, profiles_det_loaded, nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, profiles_recall)
  
  # Combine with the baseline and pivot for ease of splitting by gender.
  pop_combined <- rbind(pop_baseline, pop_scenario) %>%
                    tidyr::pivot_longer(-c("run", "casetype", "senband"), names_to = "date", values_to = "population") %>%
                    dplyr::mutate(date = as.Date(date))
  
  # Add splits by gender
  pop_combined <- split_populations_by_gender(pop_combined, gender_splits)
  
  print(paste0("pop_scenario and gender split took ", Sys.time() - t0, " seconds"))

  # Plotting routines to be used in development to assess model output.
  dev_plot_population(pop_combined, "remand", "Remand delta")
  dev_plot_population(pop_combined, "determinate", "Determinate")
  dev_plot_population(pop_combined, "indeterminate", "Indeterminate")
  dev_plot_population(pop_combined, "recall", "Recall")

  return(pop_combined)
}


#' A baseline run without any levers.
#' 
#' 
#' @return A tibble providing, for every date and case type (remand,
#'   determinate, indeterminate and recall), a baseline
#'   population based on baseline prison inflows. The exception is recall,
#'   which is zero.
#' @export
run_baseline <- function(params, inflows_det_loaded, profiles_det_loaded,
                         nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, profiles_recall) {
  
  # Remand assumed to be zero.
  pop_remand_delta <- tibble::tibble(casetype = rep("remand", params$projection_length_months), date = seq(params$forecast_start_date, params$forecast_end_date, "months"), pop_remand_delta = 0) %>%
    tidyr::pivot_wider(names_from = date,
                       values_from = pop_remand_delta,
                       names_sort = TRUE)
  
  
  prison_det_outputs <- run_prison_determinate_module(inflows_det_loaded, params$forecast_start_date, profiles_det_loaded)
    outflows_det     <- prison_det_outputs$outflows_det
    pop_det          <- prison_det_outputs$pop_det
  

  month_names        <- names(dplyr::select(inflows_det_loaded, -c("senband")))
  pop_indet          <- run_prison_indeterminate_module(month_names, params$inflows_indet_mean)
  
  
  pop_recall         <- run_prison_recall_module(outflows_det, nomis_out_delius_in_ratio, profiles_lic,
                                                 recall_rate_exclPSS, recall_rate_exclPSS, params$forecast_start_date, profiles_recall)
  
  pop_baseline <- combine_casetypes(pop_remand_delta, pop_det, pop_indet, pop_recall, "baseline")
}


# A scenario run to be compared with the baseline.
# run_scenario <- function(params, cc_receipts_delta_loaded_list, cc_output_loaded, cc_capacity_loaded, mc_disposals_delta_loaded_list, sentencing_rates_loaded,
#                          inflows_det_loaded, profiles_det_loaded,
#                          nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, profiles_recall) {
run_scenario <- function(params, cc_receipts_delta_loaded_list, cc_output_loaded, cc_capacity_loaded, mc_disposals_delta_loaded_list,
                         profiles_remand_in, profiles_remand_out,
                         sentencing_rates_loaded,
                         inflows_det_loaded, profiles_det_loaded,
                         nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, profiles_recall) {
    
  # LEVER: Add police charges.
  cc_receipts_delta  <- cc_receipts_delta_loaded_list[[params$lever_police_charges_scenario]]
  mc_disposals_delta <- mc_disposals_delta_loaded_list[[params$lever_police_charges_scenario]]
  
  # LEVER: Add (or subtract) sitting days.
  cc_capacity_levered  <- add_cc_sitting_days(cc_capacity_loaded, params$lever_extra_cc_sitting_days, params$lever_extra_cc_sitting_days_impact_date)

  # run_courts_module() issues a warning if extra ring-fenced disposals outstrip
  # remaining capacity. Do what you want when a warning occurs. Here we are just
  # echoing it.
  withCallingHandlers(warning = function(msg) {},
                      #{courts_outputs <- run_courts_module(cc_output_loaded, cc_capacity_levered, cc_receipts_delta, mc_disposals_delta, sentencing_rates_loaded, inflows_det_loaded)})
                     {courts_outputs <- run_courts_module(cc_output_loaded, cc_capacity_levered, cc_receipts_delta, mc_disposals_delta, profiles_remand_in, profiles_remand_out, sentencing_rates_loaded, inflows_det_loaded)})
    pop_remand_delta <- courts_outputs$pop_remand_delta
    inflows_det_adj  <- courts_outputs$inflows_det_adj
  


  # LEVER: Add delta from extra inflows lever to other inflows
  inflows_det_levered <- add_inflows_det_delta_lever(inflows_det_adj, params$lever_extra_inflows_det, params$lever_extra_inflows_det_impact_date)
  
  # LEVER: Change distribution of time served by sentence band
  #t0_stretch <- Sys.time()
  profiles_det_levered <- stretch_profiles(profiles_det_loaded, params$lever_profiles_det_stretch_factors, params$lever_profiles_det_stretch_factor_min, params$lever_profiles_det_stretch_factor_max, params$projection_length_months)
  profiles_det_stretch_impact_date_levered <- params$lever_profiles_det_stretch_impact_date
  #print(paste0("stretch_profiles took ", Sys.time() - t0_stretch, " seconds"))
  
  prison_det_outputs <- run_prison_determinate_module(inflows_det_levered, profiles_det_stretch_impact_date_levered, profiles_det_levered)
    outflows_det     <- prison_det_outputs$outflows_det
    pop_det          <- prison_det_outputs$pop_det

  month_names        <- names(dplyr::select(inflows_det_loaded, -c("senband")))
  pop_indet          <- run_prison_indeterminate_module(month_names, params$inflows_indet_mean)
  
  
  
  # LEVER: Set recall rates
  recall_rate_levered             <- params$lever_recall_rate
  recall_rate_impact_date_levered <- params$lever_recall_rate_impact_date
  
  pop_recall         <- run_prison_recall_module(outflows_det, nomis_out_delius_in_ratio, profiles_lic,
                                                 recall_rate_exclPSS, recall_rate_levered, recall_rate_impact_date_levered, profiles_recall)
  
  
  pop_scenario <- combine_casetypes(pop_remand_delta, pop_det, pop_indet, pop_recall, "scenario")
}



#' Run Crown Court and magistrates' court modelling to generate change in prison
#' inflows owing to changes in the number of court disposals.
#'
#' @export
#run_courts_module <- function(cc_output, cc_capacity, cc_receipts_delta, mc_disposals, sentencing_rates, inflows_det) {
run_courts_module <- function(cc_output, cc_capacity, cc_receipts_delta, mc_disposals_delta,
                              profiles_remand_in, profiles_remand_out,
                              sentencing_rates, inflows_det) {

  # Add additional Crown Court receipts (and disposals for ring-fenced cases).
  cc_output <- add_cc_receipts_delta(cc_output, cc_receipts_delta)

  # Add extra ring-fenced hours to the capacity table.
  cc_capacity <- calculate_hours_ringfenced_delta(cc_output, cc_capacity)
  check_cc_capacity(cc_capacity)
  
  # Join the capacity table with the crown output table and calculate disposals
  # delta for non-ring-fenced cases, assuming current non-ring-fenced disposal
  # case mix.
  cc_disposals_delta <- calculate_cc_disposals_delta(cc_output, cc_capacity)
  
  # Calculate remand population from court disposals.
  #pop_remand_delta  <- calculate_pop_remand_delta(cc_disposals_delta)
  pop_remand_delta  <- calculate_pop_remand_delta(mc_disposals_delta, cc_disposals_delta, profiles_remand_in, profiles_remand_out)
  
  # Calculate determinate inflows from court disposals.
  inflows_det_delta <- calculate_inflows_det_delta(cc_disposals_delta, mc_disposals_delta, sentencing_rates)
  
  # Add delta from court disposals to background inflows.
  inflows_det_adj <- add_inflows_det_delta_courts(inflows_det, inflows_det_delta)

  return(list(pop_remand_delta = pop_remand_delta,
              inflows_det_adj = inflows_det_adj))
}


#' @export
run_prison_determinate_module <- function(inflows_det, lever_profiles_det_stretch_impact_date, profiles_det_levered) {
  
  inflows_det              <- add_phases(inflows_det, lever_profiles_det_stretch_impact_date)
  outflows_det             <- mojstockr_mconv(inflows_det, profiles_det_levered, c("senband", "phase"))
  inflows_det              <- combine_phases(inflows_det)
  outflows_det             <- combine_phases(outflows_det)
  pop_det                  <- mojstockr_build_stock(inflows_det, outflows_det, c("senband"))
  
  return(list(outflows_det = outflows_det, pop_det = pop_det))
}


#' @export
run_prison_indeterminate_module <- function(month_names, inflows_indet_mean) {
  
  inflows_indet            <- calculate_inflows_indet(inflows_indet_mean, month_names)
  outflows_indet           <- calculate_outflows_indet(month_names)
  pop_indet                <- mojstockr_build_stock(inflows_indet, outflows_indet, c("casetype"))
  
  return(pop_indet)
}


#' @export
run_prison_recall_module <- function(outflows_det, nomis_out_delius_in_ratio, profiles_lic,
                                     recall_rate_exclPSS, lever_recall_rate, lever_recall_rate_impact_date, profiles_recall) {
  
  inflows_lic              <- apply_ratios(outflows_det, nomis_out_delius_in_ratio)
  outflows_lic             <- mojstockr_mconv(inflows_lic, profiles_lic, c("senband"))
  pop_lic                  <- mojstockr_build_stock(inflows_lic, outflows_lic, c("senband"))
  
  inflows_recall           <- apply_ratios(pop_lic, recall_rate_exclPSS, postimpact_ratios = lever_recall_rate, impact_date = lever_recall_rate_impact_date)
  outflows_recall          <- mojstockr_mconv(inflows_recall, profiles_recall, c("senband"))
  pop_recall               <- mojstockr_build_stock(inflows_recall, outflows_recall, c("senband"))
  
  return(pop_recall)
}


#' @export
combine_casetypes <- function(pop_remand_delta, pop_det, pop_indet, pop_recall, run) {

  pop_det <- dplyr::mutate(pop_det, casetype = "determinate", .before = 1)
  
  pop_nondet <- rbind(pop_remand_delta, pop_indet) %>%
                  dplyr::mutate(senband = NA, .after = 1)

  pop_recall <- dplyr::mutate(pop_recall, casetype = "recall", .before = 1)
  
  pop <- rbind(pop_det, pop_nondet, pop_recall) %>%
           #dplyr::arrange(casetype, senband) %>%     # Commented for speed. No need to sort here.
           dplyr::mutate(run = !!run, .before = 1)
  
}


# For development purposes. Plots output for inspection.
dev_plot_population <- function(pop_combined, casetype, casetype_label) {

  pop_baseline <- dplyr::filter(pop_combined, run == "baseline", casetype == !!casetype, sex == "All") %>%
                    dplyr::select(-c("run", "casetype", "sex")) %>%
                    tidyr::pivot_wider(names_from = date, values_from = population, values_fill = 0)
  pop_scenario <- dplyr::filter(pop_combined, run == "scenario", casetype == !!casetype, sex == "All") %>%
                    dplyr::select(-c("run", "casetype", "sex")) %>%
                    tidyr::pivot_wider(names_from = date, values_from = population, values_fill = 0)
  
  N <- nrow(pop_baseline)
  if (N > 1)
    labels <- c(paste0("Baseline, ", pop_baseline$senband), paste0("Scenario, ", pop_scenario$senband))
  else
    labels <- c("Baseline", "Scenario")

  x <- as.Date(names(pop_baseline[, -1]))
  y <- rbind(pop_baseline[, -1], pop_scenario[, -1])
  matplot(x,
          t(y),
          type = 'l',
          xlab = "Date",
          main = paste0("Scenario population impact, ", casetype_label),
          ylab = "Population",
          lty = c(rep(1, N), rep(2, N)),
          col = c(1, 2, 3, 4)
  )
  legend("topright",
         inset = 0.02,
         legend = labels,
         lty = c(rep(1, N), rep(2, N)),
         col = c(1, 2, 3, 4)
  )

}
