# Functions to load underlying datasets when prisonflowcast is first run


#' Load data from S3
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#' 
#' @param params The parameters passed to \code{run_prisonsreadyreckoner()}.
#' @return A list containing the contents of all files provided in params.
#' @export
load_datasets <- function(params) {

  # Read parameter tables for police charge and Crown modules
  ringfenced_lookup    <- load_ringfenced_lookup(params$ringfenced_lookup_file)
  
  # Load data for police charges module
  cc_receipts_delta_loaded_list <- load_police_charges_cc_data(params$police_charges_cc_files, params$police_charges_central_scenario, params$police_charges_cc_route_file, ringfenced_lookup, params$start_date$police_charges_cc, params$forecast_start_date, params$forecast_end_date)
  mc_disposals_delta_loaded_list <- load_police_charges_mc_data(params$police_charges_mc_files, params$police_charges_central_scenario, params$mc_remand_lookup, params$start_date$police_charges_mc, params$forecast_start_date, params$forecast_end_date)


  # Load data for Crown Court module
  cc_data     <- load_crown_data(params$cc_output_file, params$cc_capacity_file, ringfenced_lookup, params$start_date$cc_files, params$forecast_start_date, params$forecast_end_date)
  
  # Load data for sentencing module
  sentencing_rates <- load_sentencing_rates(params$sentencing_rates_file)
  
  # Load data for prisons module
  prison_data  <- load_prison_data(params$prison_inflows_file, params$profiles_file, params$licence_profiles_file, params$recall_file, params$gender_splits_file, params$start_date$inflows_det, params$start_date$recall_rate, params$forecast_start_date, params$forecast_end_date, params$projection_length_months, params$lever_profiles_det_stretch_factor_min)
  
  
  return(
    list(
      cc_receipts_delta_loaded_list   = cc_receipts_delta_loaded_list,
      mc_disposals_delta_loaded_list  = mc_disposals_delta_loaded_list,
      
      cc_output                       = cc_data$cc_output,
      cc_capacity                     = cc_data$cc_capacity,
      
      sentencing_rates                = sentencing_rates,
      
      inflows_det                     = prison_data$inflows_det,
      profiles_det                    = prison_data$profiles_det,
      nomis_out_delius_in_ratio       = prison_data$nomis_out_delius_in_ratio,
      profiles_lic                    = prison_data$profiles_lic,
      average_time_on_licence_excl_ps = prison_data$average_time_on_licence_excl_ps,
      recall_rate_exclPSS             = prison_data$recall_rate_exclPSS,
      average_time_on_recall          = prison_data$average_time_on_recall,
      recall_profile_adjustments      = prison_data$recall_profile_adjustments,

      gender_splits                   = prison_data$gender_splits
    )
  )
}


load_crown_data <- function(cc_output_file, cc_capacity_file, ringfenced_lookup, start_date, forecast_start_date, forecast_end_date) {
    
  # Read baseline inputs, which are disposals for ring-fenced cases and time
  # series for sitting days and hours available per sitting day.
  cc_output   <- load_crown_output(cc_output_file, start_date, forecast_start_date, forecast_end_date)
  cc_capacity <- load_cc_capacity(cc_capacity_file, start_date, forecast_start_date, forecast_end_date)
  check_cc_inputs(cc_output, cc_capacity)

  # Add ring-fenced status. Calculate backlog case rate and hours per disposal.
  cc_output <- augment_crown_output(cc_output, ringfenced_lookup)
  
  # Make capacity monitor
  cc_capacity  <- augment_cc_capacity(cc_capacity, cc_output)
  
  return(list(cc_output = cc_output, cc_capacity = cc_capacity))
}


# DEVELOPMENT NOTE: Is there any need for this function? Consider placing
# contents in load_datasets().
load_sentencing_rates <- function(sentencing_rates_file)
  sentencing_rates <- import_s3_file(sentencing_rates_file)


# DEVELOPMENT NOTE: Is there any need for this function? Consider placing
# contents in load_datasets().
load_prison_data <- function(prison_inflows_file, profiles_file, licence_profiles_file, recall_file, gender_splits_file, start_date_inflows_det, start_date_recall_rate, forecast_start_date, forecast_end_date, projection_length_months, lever_profiles_det_stretch_factor_min) {
    
  inflows_det <- load_inflows_det(prison_inflows_file, start_date_inflows_det, forecast_start_date, forecast_end_date)
  
  profiles_det <- load_profiles_det(profiles_file, projection_length_months, lever_profiles_det_stretch_factor_min)
  
  # Load licence and recall from the 'recall file'.
  nomis_out_delius_in_ratio  <- load_nomis_out_delius_in_ratio(recall_file)
  profiles_lic               <- load_profiles_lic(recall_file, projection_length_months)
  recall_rate_exclPSS        <- load_recall_rate_exclPSS(recall_file, start_date_recall_rate, forecast_start_date, forecast_end_date)
  average_time_on_recall     <- load_average_time_on_recall(recall_file)
  recall_profile_adjustments <- load_recall_profile_adjustments(recall_file)
  
  # Load data on split between male and female prisoners
  gender_splits <- load_gender_splits(gender_splits_file)
  
  return(
    list(
      inflows_det                     = inflows_det,
      profiles_det                    = profiles_det,
      
      nomis_out_delius_in_ratio       = nomis_out_delius_in_ratio,
      profiles_lic                    = profiles_lic,
      recall_rate_exclPSS             = recall_rate_exclPSS,
      average_time_on_recall          = average_time_on_recall,
      recall_profile_adjustments      = recall_profile_adjustments,
      
      gender_splits                   = gender_splits
    )
  )
  
}


# Trim dates to requested window.
# Used by load functions to filter time series data as soon as they are loaded.
trim_dates <- function(tbl, start_date, forecast_start_date, forecast_end_date) {
  
  if (min(tbl$date) != start_date)
    stop("First date in input file was ", min(tbl$date), ". ", start_date, " expected.")
  
  tbl <- dplyr::mutate(tbl, date = lubridate::floor_date(date, "month")) %>% 
    dplyr::filter(date >= forecast_start_date, date <= forecast_end_date)
  
  if (min(tbl$date) != forecast_start_date || max(tbl$date) != forecast_end_date)
    stop("Input file did not cover the full date range requested. Dates must exist for the interval, ", forecast_start_date, ", to ", forecast_end_date, ".")

  return(tbl)
}
