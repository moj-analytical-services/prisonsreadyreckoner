# Script for setting up parameters in correct format


#' Format parameters.
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#' 
#' @param params The parameters passed to \code{run_prisonsreadyreckoner()}.
#' @return The parameters with additional variables not seen by the user and
#'   others formatted for onward use.
#' @export
format_params <- function(params) {
  
  # Add the name of the default police charge scenario. Note, this is a reserved
  # value and should not be used for any scenarios provided for either
  # params$police_charges_mc_files or params$police_charges_cc_files.
  params$police_charges_central_scenario <- "central"
  
  # Add minimum and maximum stretch factors. For use, for example, by a Shiny
  # app.
  params$lever_profiles_det_stretch_factor_min   <- 0.5
  params$lever_profiles_det_stretch_factor_max   <- 2
  params$lever_profiles_recall_stretch_factor_min   <- 0.5
  params$lever_profiles_recall_stretch_factor_max   <- 2

  # Convert expected file start date strings to dates.
  params$start_date$police_charges_cc <- as.Date(params$start_date$police_charges_cc)
  params$start_date$police_charges_mc <- as.Date(params$start_date$police_charges_mc)
  params$start_date$inflows_det       <- as.Date(params$start_date$inflows_det)
  params$start_date$recall_rate       <- as.Date(params$start_date$recall_rate)   # May be NA if not provided as a time series.
  params$start_date$cc_files          <- as.Date(params$start_date$cc_files)
  
  # Add forecast start and end dates based on file start dates and number of
  # months requested.
  start_dates <- as.Date(c(params$start_date$police_charges_cc, params$start_date$police_charges_mc, params$start_date$inflows_det, params$start_date$recall_rate, params$start_date$cc_files))
  params$forecast_start_date     <- lubridate::floor_date(max(start_dates[!is.na(start_dates)]), "month")
  params$forecast_end_date       <- lubridate::add_with_rollback(params$forecast_start_date, months(params$projection_length_months - 1), roll_to_first = TRUE) 
  
  # Convert lever date strings to dates and floor to the beginning of the month
  # for good measure. Our model only operates on a monthly basis.
  params$lever_extra_cc_sitting_days_impact_date    <- lubridate::floor_date(as.Date(params$lever_extra_cc_sitting_days_impact_date), "month")
  params$lever_extra_inflows_det_impact_date        <- lubridate::floor_date(as.Date(params$lever_extra_inflows_det_impact_date), "month")
  params$lever_profiles_det_stretch_impact_date     <- lubridate::floor_date(as.Date(params$lever_profiles_det_stretch_impact_date), "month")
  params$lever_recall_rate_impact_date              <- lubridate::floor_date(as.Date(params$lever_recall_rate_impact_date), "month")
  params$lever_profiles_recall_stretch_impact_date  <- lubridate::floor_date(as.Date(params$lever_profiles_recall_stretch_impact_date), "month")

  return(params)
}


#' Add default parameters
#' 
#' Exported only for use by the Shiny app. Not to be used by a user of the
#' package.
#'
#' Default parameters may be used in a Shiny environment.
#'
#' @param params The parameters passed to \code{run_prisonsreadyreckoner()}.
#' @param recall_rate_exclPSS Recall rate for those not on post-sentence
#'   supervision (PSS).
#' @return A list of default lever values to be used by the Shiny app.
#' @export
set_defaults <- function(params, recall_rate_exclPSS) {
  
  default_impact_date <- lubridate::add_with_rollback(params$forecast_start_date, months(3), roll_to_first = TRUE) 
  
  defaults <- list()
  
  # Number of extra police charges
  defaults$lever_police_charges_scenario        <- "central"
  
  defaults$extra_cc_sitting_days                <- 0 / 12             # [month^-1]
  defaults$extra_cc_sitting_days_impact_date    <- default_impact_date

  defaults$extra_inflows_det                    <- c(senband1 = 0, senband2 = 0, senband3 = 0, senband4 = 0)
  defaults$extra_inflows_det_impact_date        <- default_impact_date
    
  defaults$profiles_det_stretch_factors         <- c(senband1 = 1, senband2 = 1, senband3 = 1, senband4 = 1)
  defaults$profiles_det_stretch_impact_date     <- default_impact_date

  defaults$recall_rate                          <- recall_rate_exclPSS
  defaults$recall_rate_impact_date              <- default_impact_date
  
  defaults$profiles_recall_stretch_factors      <- c(senband1 = 1, senband2 = 1, senband3 = 1, senband4 = 1)
  defaults$profiles_recall_stretch_impact_date  <- default_impact_date

  return(defaults)
}
