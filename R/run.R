

#' Run \code{prisonsreadyreckoner} in its entirety.
#' 
#' Given a set of parameter values conforming with the standards of the
#' installed package version, \code{run_prisonsreadyreckoner()} generates a
#' forecast of prison population, split by case type, sentence band and sex
#' under 'baseline' and 'scenario' conditions. This function estimates the
#' impact of deviations of model assumptions from those used in a central
#' forecast, as embodied in some of the many input parameters. The difference
#' between scenario and baseline volumes represents the additional prison
#' population associated with the parameters and may be added to the relevant
#' central forecast to demonstrate impact.
#' 
#' @section Overview: The function of \code{run_prisonsreadyreckoner()} is
#'   complex and a full treatment is provided in our vignettes. Briefly, the
#'   parameters specify files containing police charge input scenarios. These
#'   are passed through Crown Court and sentencing modules in the scenario
#'   condition to generate a prison inflow time series. The inflow time series
#'   is converted into a prison population on the assumption that volumes in
#'   different sentence length bands have particular distributions of time in
#'   custody. Recall is generated from prison outflows using similar assumptions
#'   as used in the main prisons modelling. Monthly recall volumes are in
#'   proportion to the current licensee population and a distribution of time on
#'   recall is assumed. In our case, while time on licence is taken from the
#'   main modelling, we assume a fixed time on licence, equal to the average
#'   assumed in the main modelling.
#'   
#'   The user may explore the impact of key drivers of prison population by
#'   setting special parameters we call 'levers' and their impact dates. These
#'   levers currently relate to
#'   \enumerate{
#'     \item the particular police charge scenario selected;
#'     \item the number of sitting days added;
#'     \item the number of additional prisoners added to the population (as
#'     an alternative proxy for levers above);
#'     \item the degree to which time spent in custody might be shorter or
#'     longer than currently; and
#'     \item the rate of recall per licensee.
#'   }
#'   
#'   All of these change abruptly at the impact date by an amount specified and
#'   some are split by sentence length band.
#' 
#' @section Sentence length bands: Sentence length bands are as follows:
#'   \describe{
#'     \item{Band 1}{Up to 6 months (less than 182 days).}
#'     \item{Band 2}{6 to 12 months (182 to 364 days, inclusive).}
#'     \item{Band 3}{12 months to 4 years (365 to 1,460 days, inclusive).}
#'     \item{Band 4}{4 years or more (greater than or equal to 1,461 days). To
#'     be combined with Extended Determinate Sentence prisoners (EDS).}
#'   }
#' 
#' @section Assumptions and limitations: In order to operate quickly (so that it
#'   may support a Shiny app), we have made various simplifying assumptions. The
#'   principal simplifications currently are:
#'   \describe{
#'     \item{New prisoners only}{We only model impacts on new prisoners
#'     (entering after the forecast start date).}
#'     \item{Unlimited Crown backlog}{The app assumes that the Crown Court
#'     backlog will continue throughout the forecast period. If the user adds
#'     too many sitting days, this assumption may become invalid.}
#'     \item{Limited police charge scenarios}{The app offers a limited set of
#'     upstream scenarios and lacks the dynamic case type prioritisation of the
#'     formal prison projection.}
#'     \item{Naive remand and HDC}{The app does not model time the interaction
#'     of time on remand, HDC or recall on the time served as a sentence
#'     prisoner or on licence, as appropriate.}
#'   }
#'   
#'   THe first of these assumptions means that the ready reckoner cannot show
#'   the immediate impact of a change in recall behaviour because; it can only
#'   show an impact after a delay reflecting the time new determinate sentence
#'   prisoners serve their sentenced time in custody.
#' 
#' Please note, the contents of baseline and scenario mean different things for
#' different population cohorts and the baseline is provided only for
#' information. Only the difference, \code{scenario} minus \code{baseline} is a
#' meaningful measure, representing the impact of the parameters chosen.
#' 
#' @param params A list of parameters specifying all files and variable values
#'   required to generate output of a \code{prisonsreadyreckoner} scenario run.
#'   See the list of parameter descriptions provided in the
#'   parameters vignette for further details.
#' @param baseline_only This is a TRUE/FALSE switch, so that you can return either
#'   a table of results containing the baseline and the scenario (FALSE, default
#'   setting), or a table with just the baseline results (TRUE). This optional
#'   parameter is to make coding the prisons-ready-reckoner-app more robust.
#' @return A tibble with fields, \code{run}, \code{date}, \code{casetype},
#'   \code{senband}, \code{sex} and \code{population}, representing a forecast
#'   of prison population by case type (remand, determinate, indeterminate and
#'   recall) and, where relevant, sentence band (bands 1 to 4) and sex. Two runs
#'   are provided, the \code{baseline} and the \code{scenario}.
#' @examples
#' \dontrun{
#' run_prisonsreadyreckoner(params)
#' }
#' @export
run_prisonsreadyreckoner <- function(params, baseline_only = FALSE) {

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
  
  # Calculate properties of recall outflows.
  recall_time     <- multiply_two_named_vectors(average_time_on_recall, recall_profile_adjustments, arguments_to_keep = c("senband1", "senband2", "senband3", "senband4"))
  

  # Calculate baseline with only original inflows.
  pop_baseline <- run_baseline(params, inflows_det_loaded, profiles_det_loaded,
                               nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, recall_time)
  
  
  t0 <- Sys.time()
  
  # Check if baseline_only is TRUE, and if so then don't bind with the scenario outputs
  if(baseline_only == TRUE){
    pop_selected <- pop_baseline
  }else{
    # Run scenario and find overall population changes.
    pop_scenario <- run_scenario(params, cc_receipts_delta_loaded_list, cc_output_loaded, cc_capacity_loaded, mc_disposals_delta_loaded_list, profiles_remand_in, profiles_remand_out, sentencing_rates_loaded, inflows_det_loaded, profiles_det_loaded, nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, recall_time)
  
    # Then combine with the baseline
    pop_selected <- rbind(pop_baseline, pop_scenario)
  }
  
  # Pivot data for ease of splitting by gender.
  pop_combined <- pop_selected %>%
    tidyr::pivot_longer(-c("run", "casetype", "senband"), names_to = "date", values_to = "population") %>%
    dplyr::mutate(date = as.Date(date))
  
  # Add splits by gender
  pop_combined <- split_populations_by_gender(pop_combined, gender_splits)
  
  #print(paste0("pop_scenario and gender split took ", Sys.time() - t0, " seconds"))
  
  # # Plotting routines to be used in development to assess model output.
  # dev_plot_population(pop_combined, "remand", "Remand delta")
  # dev_plot_population(pop_combined, "determinate", "Determinate")
  # dev_plot_population(pop_combined, "indeterminate", "Indeterminate")
  # dev_plot_population(pop_combined, "recall", "Recall")
  
  return(dplyr::arrange(pop_combined, run, date, casetype, senband, sex))
}


#' A baseline run without any levers.
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#' 
#' @param params The parameters passed to \code{run_prisonsreadyreckoner()}.
#' @param inflows_det_loaded Baseline prison inflows (as used in the main
#'   prisons model).
#' @param profiles_det_loaded Determinate time-served profiles. 
#' @param nomis_out_delius_in_ratio Ratios, by sentence length band, for
#'   converting prison outflows to typical probation inflows.
#' @param profiles_lic Licensee time-served profiles.
#' @param recall_rate_exclPSS Recall rate for those not on post-sentence
#'   supervision (PSS).
#' @param recall_time Time to be spent on recall by recallees, split by
#'   sentence length band.
#' @return A tibble providing, for every date and case type (remand,
#'   determinate, indeterminate and recall), a baseline
#'   population based on baseline prison inflows. The exception is recall,
#'   which is zero.
#' @export
run_baseline <- function(params, inflows_det_loaded, profiles_det_loaded,
                         nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, recall_time) {
  
  # Remand assumed to be zero.
  pop_remand_delta <- tibble::tibble(casetype = rep("remand", params$projection_length_months), date = seq(params$forecast_start_date, params$forecast_end_date, "months"), pop_remand_delta = 0) %>%
    tidyr::pivot_wider(names_from = date,
                       values_from = pop_remand_delta,
                       names_sort = TRUE)
  
  
  prison_det_outputs <- run_prison_determinate_module(inflows_det_loaded, params$forecast_start_date, profiles_det_loaded)
    outflows_det     <- prison_det_outputs$outflows_det
    pop_det          <- prison_det_outputs$pop_det
  
  
  month_names        <- names(dplyr::select(inflows_det_loaded, -tidyselect::any_of(c("senband"))))
  pop_indet          <- run_prison_indeterminate_module(month_names, params$inflows_indet_mean)
  
  # Make unlevered recall profiles
  profiles_recall <- make_lag_filters(recall_time)
  profiles_recall <- add_phases(profiles_recall)
  
  pop_recall         <- run_prison_recall_module(outflows_det, nomis_out_delius_in_ratio, profiles_lic,
                                                 recall_rate_exclPSS, recall_rate_exclPSS, params$forecast_start_date, profiles_recall, params$forecast_start_date)
  
  pop_baseline <- combine_casetypes(pop_remand_delta, pop_det, pop_indet, pop_recall, "baseline")
}


# A scenario run to be compared with the baseline.
run_scenario <- function(params, cc_receipts_delta_loaded_list, cc_output_loaded, cc_capacity_loaded, mc_disposals_delta_loaded_list,
                         profiles_remand_in, profiles_remand_out,
                         sentencing_rates_loaded,
                         inflows_det_loaded, profiles_det_loaded,
                         nomis_out_delius_in_ratio, profiles_lic, recall_rate_exclPSS, recall_time) {
  
  # LEVER: Add police charges.
  cc_receipts_delta  <- cc_receipts_delta_loaded_list[[params$lever_police_charges_scenario]]
  mc_disposals_delta <- mc_disposals_delta_loaded_list[[params$lever_police_charges_scenario]]

  # LEVER: Add (or subtract) sitting days.
  cc_capacity_levered  <- add_cc_sitting_days(cc_capacity_loaded, params$lever_extra_cc_sitting_days, params$lever_extra_cc_sitting_days_impact_date)
  
  # run_courts_module() issues a warning if extra ring-fenced disposals outstrip
  # remaining capacity. Do what you want when a warning occurs. Here we are just
  # echoing it.
  # withCallingHandlers(warning = function(msg) {warning("Outputs will not be meaningful.")},
  #                     {courts_outputs <- run_courts_module(cc_output_loaded, cc_capacity_levered, cc_receipts_delta, mc_disposals_delta, profiles_remand_in, profiles_remand_out, sentencing_rates_loaded, inflows_det_loaded)})
  courts_outputs     <- run_courts_module(cc_output_loaded, cc_capacity_levered, cc_receipts_delta, mc_disposals_delta, profiles_remand_in, profiles_remand_out, params$published_remand_pop, sentencing_rates_loaded, inflows_det_loaded)
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

  month_names        <- names(dplyr::select(inflows_det_loaded, -tidyselect::any_of(c("senband"))))
  pop_indet          <- run_prison_indeterminate_module(month_names, params$inflows_indet_mean)
  
  
  
  # LEVER: Set recall rates
  recall_rate_levered             <- params$lever_recall_rate
  recall_rate_impact_date_levered <- params$lever_recall_rate_impact_date
  
  # LEVER: Stretch recall profiles
  profiles_recall_levered <- stretch_recall_time_lever(recall_time, params$lever_profiles_recall_stretch_factors, params$lever_profiles_recall_stretch_factor_min, params$lever_profiles_recall_stretch_factor_max)
  profiles_recall_stretch_impact_date_levered <- params$lever_profiles_recall_stretch_impact_date
  
  pop_recall         <- run_prison_recall_module(outflows_det, nomis_out_delius_in_ratio, profiles_lic,
                                                 recall_rate_exclPSS, recall_rate_levered, recall_rate_impact_date_levered, profiles_recall_levered, profiles_recall_stretch_impact_date_levered)
  
  
  pop_scenario <- combine_casetypes(pop_remand_delta, pop_det, pop_indet, pop_recall, "scenario")
}



#' Run Crown Court and magistrates' court modelling to generate change in prison
#' inflows owing to changes in the number of court disposals.
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#'
#' @param cc_output A table of Crown Court disposal statistics provided by the
#'   Courts Team.
#' @param cc_capacity A table of Crown Court sitting days and hours per sitting
#'   day provided by the Courts Team.
#' @param cc_receipts_delta A table of deviations in Crown Court receipt volumes
#'   under a single police charge scenario, lagged suitably to account for lag
#'   to disposal.
#' @param mc_disposals_delta A table of deviations in magistrates' court
#'   disposal volumes under a single police charge scenario.
#' @param profiles_remand_in The remand population impact profile per individual
#'   received in the magistrates' court.
#' @param profiles_remand_out The remand population impact profile per individual
#'   disposal in the Crown Court.
#' @param published_remand_pop The published remand population for use in error
#'   checking.
#' @param sentencing_rates A table of linear regression co-efficients for
#'   converting court disposal types to prison inflows by sentence band.
#' @param inflows_det The forecast prison inflows taken from the main prisons
#'   model.
#' @return A list including remand population impacts (\code{pop_remand_delta})
#'   and forecast prison inflows (\code{inflows_det_adj}), including the
#'   original volumes supplied by the Prisons Team
#' @export
run_courts_module <- function(cc_output, cc_capacity, cc_receipts_delta, mc_disposals_delta,
                              profiles_remand_in, profiles_remand_out, published_remand_pop,
                              sentencing_rates, inflows_det) {
  
  # NA signals the default condition, in which cases there are no extra receipts
  # to add and no extra ringfenced disposals.
  if (is.data.frame(cc_receipts_delta)) {

    # Add additional Crown Court receipts (and disposals for ring-fenced cases).
    cc_output <- add_cc_receipts_delta(cc_output, cc_receipts_delta)
    
    # Add extra ring-fenced hours to the capacity table.
    cc_capacity <- calculate_hours_ringfenced_delta(cc_output, cc_capacity)
    check_cc_capacity(cc_capacity)
  }

  # Join the capacity table with the crown output table and calculate disposals
  # delta for non-ring-fenced cases, assuming current non-ring-fenced disposal
  # case mix.
  cc_disposals_delta <- calculate_cc_disposals_delta(cc_output, cc_capacity)
  check_cc_disposals_delta(cc_output, cc_disposals_delta)
  
  # Calculate remand population from court disposals.
  pop_remand_delta  <- calculate_pop_remand_delta(mc_disposals_delta, cc_disposals_delta, profiles_remand_in, profiles_remand_out)
  check_pop_remand(pop_remand_delta, published_remand_pop)
  
  # Calculate determinate inflows from court disposals.
  inflows_det_delta <- calculate_inflows_det_delta(cc_disposals_delta, mc_disposals_delta, sentencing_rates)
  
  # Add delta from court disposals to background inflows.
  inflows_det_adj <- add_inflows_det_delta_courts(inflows_det, inflows_det_delta)
  
  return(list(pop_remand_delta = pop_remand_delta,
              inflows_det_adj = inflows_det_adj))
}


#' Create a determinate sentence population from prison inflows and outflow
#' profiles.
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#' 
#' @param inflows_det A table of determinate sentence inflow volumes.
#' @param lever_profiles_det_stretch_impact_date Impact dates for applying
#'   alternative determinate sentence time-served profiles.
#' @param profiles_det_levered Determinate sentence time-served profiles before
#'   and after the impact date.
#' @return A list including the outflows by determinate sentence length band and
#'   their populations.
#' @export
run_prison_determinate_module <- function(inflows_det, lever_profiles_det_stretch_impact_date, profiles_det_levered) {
  
  inflows_det              <- add_phases(inflows_det, impact_date = lever_profiles_det_stretch_impact_date)
  outflows_det             <- mojstockr_mconv(inflows_det, profiles_det_levered, c("senband", "phase"))
  inflows_det              <- combine_phases(inflows_det)
  outflows_det             <- combine_phases(outflows_det)
  pop_det                  <- mojstockr_build_stock(inflows_det, outflows_det, c("senband"))
  
  return(list(outflows_det = outflows_det, pop_det = pop_det))
}


#' Create an indeterminate sentence population from prison inflows.
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#' 
#' @param month_names The months defining the forecast period
#' @param inflows_indet_mean The mean indeterminate inflow rate (to be taken
#'   from publihed statistics).
#' @return A table representing the indeterminate population forecast.
#' @export
run_prison_indeterminate_module <- function(month_names, inflows_indet_mean) {
  
  inflows_indet            <- calculate_inflows_indet(inflows_indet_mean, month_names)
  outflows_indet           <- calculate_outflows_indet(month_names)
  pop_indet                <- mojstockr_build_stock(inflows_indet, outflows_indet, c("casetype"))
  
  return(pop_indet)
}


#' Create a recall prison population from estimates of the licence population.
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#' 
#' @param outflows_det The table of outflows by determinate sentence length
#'   band.
#' @param nomis_out_delius_in_ratio Ratios, by sentence length band, for
#'   converting prison outflows to typical probation inflows.
#' @param profiles_lic Licensee time-served profiles.
#' @param recall_rate_exclPSS Recall rate for those not on post-sentence
#'   supervision (PSS).
#' @param lever_recall_rate The recall rates by sentence length band.
#' @param lever_recall_rate_impact_date The impact date for changing recall
#'   rates.
#' @param profiles_recall_levered The time on recall profiles by sentence length
#'   band,
#' @param lever_profiles_recall_stretch_impact_date The impact date for applying
#'   the new recall profils
#' @return A table representing the recall population forecast.
#' @export
run_prison_recall_module <- function(outflows_det, nomis_out_delius_in_ratio, profiles_lic,
                                     recall_rate_exclPSS, lever_recall_rate, lever_recall_rate_impact_date, profiles_recall_levered, lever_profiles_recall_stretch_impact_date) {

  inflows_lic              <- apply_ratios(outflows_det, nomis_out_delius_in_ratio)
  outflows_lic             <- mojstockr_mconv(inflows_lic, profiles_lic, c("senband"))
  pop_lic                  <- mojstockr_build_stock(inflows_lic, outflows_lic, c("senband"))
  
  inflows_recall           <- apply_ratios(pop_lic, recall_rate_exclPSS, postimpact_ratios = lever_recall_rate, impact_date = lever_recall_rate_impact_date)
  inflows_recall           <- add_phases(inflows_recall, impact_date = lever_profiles_recall_stretch_impact_date)
  outflows_recall          <- mojstockr_mconv(inflows_recall, profiles_recall_levered, c("senband", "phase"))
  inflows_recall           <- combine_phases(inflows_recall)
  outflows_recall          <- combine_phases(outflows_recall)
  pop_recall               <- mojstockr_build_stock(inflows_recall, outflows_recall, c("senband"))
  
  return(pop_recall)
}


#' Combine all prison population cohorts into one table.
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#' 
#' @param pop_remand_delta Remand population impact.
#' @param pop_det Forecast determinate sentence population.
#' @param pop_indet Forecast indeterminate sentence population.
#' @param pop_recall Forecast recall population.
#' @param run A string for the run ('baseline' or 'scenario') column.
#' @return A table of population forecasts for each cohort labelled by run and
#'   split by case type and sentence band, where appropriate.
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
                    dplyr::select(-tidyselect::any_of(c("run", "casetype", "sex"))) %>%
                    tidyr::pivot_wider(names_from = date, values_from = population, values_fill = 0)
  pop_scenario <- dplyr::filter(pop_combined, run == "scenario", casetype == !!casetype, sex == "All") %>%
                    dplyr::select(-tidyselect::any_of(c("run", "casetype", "sex"))) %>%
                    tidyr::pivot_wider(names_from = date, values_from = population, values_fill = 0)
  
  N <- nrow(pop_baseline)
  if (N > 1) {
    labels <- c(paste0("Baseline, ", pop_baseline$senband), paste0("Scenario, ", pop_scenario$senband))
  } else {
    labels <- c("Baseline", "Scenario")
  }
  
  x <- as.Date(names(pop_baseline[, -1]))
  y <- rbind(pop_baseline[, -1], pop_scenario[, -1])
  graphics::matplot(x,
          t(y),
          type = 'l',
          xlab = "Date",
          main = paste0("Scenario population impact, ", casetype_label),
          ylab = "Population",
          lty = c(rep(1, N), rep(2, N)),
          col = c(1, 2, 3, 4)
  )
  graphics::legend("topright",
         inset = 0.02,
         legend = labels,
         lty = c(rep(1, N), rep(2, N)),
         col = c(1, 2, 3, 4)
  )
  
}