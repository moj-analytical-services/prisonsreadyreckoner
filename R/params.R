# Script for setting up parameters in correct format


#' Format parameters.
#' 
#' @export
format_params <- function(params) {
  
  # Add minimum and maximum stretch factors. For use, for example, by a Shiny
  # app.
  params$lever_profiles_det_stretch_factor_min   <- 0.5
  params$lever_profiles_det_stretch_factor_max   <- 2

  # Convert expected file start date strings to dates.
  params$start_date$police_charges_cc <- as.Date(params$start_date$police_charges_cc)
  params$start_date$police_charges_mc <- as.Date(params$start_date$police_charges_mc)
  params$start_date$inflows_det       <- as.Date(params$start_date$inflows_det)
  params$start_date$recall_rate       <- as.Date(params$start_date$recall_rate)
  params$start_date$cc_files          <- as.Date(params$start_date$cc_files)
  
  # Add forecast start and end dates based on file start dates and number of
  # months requested.
  params$forecast_start_date     <- lubridate::floor_date(max(params$start_date$police_charges_cc, params$start_date$police_charges_mc, params$start_date$inflows_det, params$start_date$recall_rate, params$start_date$cc_files), "month")
  params$forecast_end_date       <- lubridate::add_with_rollback(params$forecast_start_date, months(params$projection_length_months - 1), roll_to_first = TRUE) 
  
  # Convert lever date strings to dates.
  params$lever_extra_cc_sitting_days_impact_date <- as.Date(params$lever_extra_cc_sitting_days_impact_date)
  params$lever_extra_inflows_det_impact_date     <- as.Date(params$lever_extra_inflows_det_impact_date)
  params$lever_profiles_det_stretch_impact_date  <- as.Date(params$lever_profiles_det_stretch_impact_date)
  params$lever_recall_rate_impact_date           <- as.Date(params$lever_recall_rate_impact_date)
  
  return(params)
}


#' Add default parameters
#'
#' Default parameters may be used in a Shiny environment.
#'
#' @export
set_defaults <- function(params, recall_rate_exclPSS) {
  
  default_impact_date <- lubridate::add_with_rollback(params$forecast_start_date, months(3), roll_to_first = TRUE) 
  
  defaults <- list()
  
  # Number of extra police charges
  defaults$lever_police_charges_scenario     <- "central"
  
  defaults$extra_cc_sitting_days             <- 0             # [month^-1]
  defaults$extra_cc_sitting_days_impact_date <- default_impact_date

  defaults$extra_inflows_det                 <- c(senband1 = 0, senband2 = 0, senband3 = 0, senband4 = 0)
  defaults$extra_inflows_det_impact_date     <- default_impact_date
    
  defaults$profiles_det_stretch_factors      <- c(senband1 = 1, senband2 = 1, senband3 = 1, senband4 = 1)
  defaults$profiles_det_stretch_impact_date  <- default_impact_date

  defaults$recall_rate                       <- recall_rate_exclPSS
  defaults$recall_rate_impact_date           <- default_impact_date

  return(defaults)
}
