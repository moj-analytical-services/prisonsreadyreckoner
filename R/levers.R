
#' Increase Crown Court capacity
#' 
#' @export
add_cc_sitting_days <- function(cc_capacity, lever_extra_cc_sitting_days, capacity_impact_date) {
  
  if (capacity_impact_date < min(cc_capacity$date) || capacity_impact_date > max(cc_capacity$date))
    stop("Capacity impact date, ", capacity_impact_date, ", is outside the forecast horizon.")
  
  cc_capacity$sitting_days_delta <- (cc_capacity$date >= capacity_impact_date) * lever_extra_cc_sitting_days
  cc_capacity$capacity_delta     <- cc_capacity$sitting_days_delta * cc_capacity$hours_per_day
  
  return(cc_capacity)
}


#' Add a constant number of monthly determinate sentence prison receptions
#'
#' \code{add_inflows_det_delta_lever} allows users to specify an additional
#' monthly number of determinate sentence offenders who enter prison after being
#' sentenced at court. (Note that this lever does not affect the number of
#' offenders entering prison after being recalled from licence.) A separate
#' additional number is set for each Sentence Band, and can be positive or
#' negative. The lever affects all months after a user-specified impact date.
#' 
#' @export
add_inflows_det_delta_lever <- function(inflows_det, lever_extra_inflows_det, lever_extra_inflows_det_impact_date, non_data_cols = "senband") {
  
  inflows_det <- inflows_det %>%
    dplyr::mutate(dplyr::across(c(all_of(names(dplyr::select(., -c("senband"))))), ~tidyr::replace_na(.,0)))
  
  coldates_post <- c(FALSE, as.Date(names(inflows_det[-c(1)])) >= as.Date(lever_extra_inflows_det_impact_date))
  
  for (senband in names(lever_extra_inflows_det)) {
    
    inflows_det[inflows_det$senband == senband, coldates_post] <- inflows_det[inflows_det$senband == senband, coldates_post] + lever_extra_inflows_det[[senband]]
    
  }
  
  return(inflows_det)
  
}


#' Stretch profiles by a user-defined factor
#' 
#' @export
stretch_profiles <- function(profiles, lever_profiles_det_stretch_factors, min_stretch_factor, max_stretch_factor, projection_length_months, non_data_cols = c("senband", "phase")) {
  
  
  for (non_data_col in non_data_cols) {
    if (!(non_data_col %in% colnames(profiles)))
      stop("The tibble \"profiles\" passed to stretch_profiles() contains no column named ", non_data_col, ". Please remove this column name from the character array \"non_data_cols\". (Note that \"non_data_cols\" is an optional argument to the function stretch_profiles(). By default, \"non_data_cols\" is set to c(\"senband\", \"phase\").) If you have not explicitly set the argument \"non_data_cols\", please either manually set the argument \"non_data_cols\" as appropriate to your input tibble or ensure that the input tibble contains columns named \"senband\" and \"phase\".")
  }

  if (!("phase" %in% colnames(profiles))) {
    stop("The input tibble \"profiles\" does not contain a column named \"phase\" - please add this column. (The input tibble \"profiles\" must contain a column named \"phase\". Entries in this column must be either the string \"pre_impact\" or the string \"post_impact\", and there must be at least one instance of each of these strings.)")
  }
  
  # Check that all columns are numeric
  if (!is.numeric(dplyr::select(profiles, -all_of(non_data_cols)) %>% as.matrix))
    stop("The input tibble \"profiles\" contains some non-numeric values outside of the columns \"non_data_cols\". Please check that your tibble contains no non-numeric values apart from in the columns specified in the character vector \"non_data_cols\".")
  
  
  i <- 0
  C <- min(ncol(profiles), length(non_data_cols) + projection_length_months)

  for (stretch_factor_name in names(lever_profiles_det_stretch_factors)) {
    
    senband = stretch_factor_name
    stretch_factor = lever_profiles_det_stretch_factors[[stretch_factor_name]]
    
    profile <- profiles %>%
      dplyr::filter(senband == !!senband)
    
    # Do not stretch profile if stretch_factor = 1; simply return  a trimmed
    # profile
    if (stretch_factor == 1) {
      
      profile_resampled <- profile[, 1:C]
      
    } else {
      
      if (stretch_factor < min_stretch_factor)
        stop("Parameter \"stretch_factor\" is smaller than parameter \"min_stretch_factor\". Please set \"stretch_factor\" to be greater than or equal to \"min_stretch_factor\".")
      
      if (stretch_factor > max_stretch_factor)
        stop("Parameter \"stretch_factor\" is greater than parameter \"max_stretch_factor\". Please set \"stretch_factor\" to be smaller than or equal to \"max_stretch_factor\".")
      
      if (is.null(stretch_factor))
        stop("Parameter \"stretch_factor\" cannot be null. Please set \"stretch_factor\" to be a postive number.")
      else if (!is.null(stretch_factor)) {
        if (stretch_factor < 0)
          stop("Parameter \"stretch_factor\" < 0. It is not possible to stretch the probability distribution by a negative factor. Please set \"stretch_factor\" to be greater than 0.")
        if (stretch_factor == 0)
          stop("Parameter \"stretch_factor\" cannot equal 0. Please set \"stretch_factor\" to be greater than 0.")
      }
      
      if (! all(profile$phase %in% c("pre_impact", "post_impact")))
        stop(paste0("The tibble \"profiles\" contains rows for which the column \"senband\" equals ", senband, " and the column \"phase\" contains strings other than \"pre_impact\" or \"post_impact\". Please ensure that the column \"phase\" contains only the strings \"pre_impact\" and \"post_impact\"."))
      
      if (! any(profile$phase %in% "pre_impact"))
        stop(paste0("The tibble \"profiles\" contains rows for which the column \"senband\" equals ", senband, " and the column \"phase\" contains no instances of the string \"pre_impact\". Entries in the column \"phase\" must be either the string \"pre_impact\" or the string \"post_impact\", and there must be at least one instance of each of these strings. Please check your input tibble."))
  
      if (! any(profile$phase %in% "post_impact"))
        stop(paste0("The tibble \"profiles\" contains rows for which the column \"senband\" equals ", senband, " and the column \"phase\" contains no instances of the string \"post_impact\". Entries in the column \"phase\" must be either the string \"pre_impact\" or the string \"post_impact\", and there must be at least one instance of each of these strings. Please check your input tibble."))
 
      
      # Split profiles into pre- and post-impact rows, and separate into data and
      # non-data columns
      profile_pre_impact <- profile %>%
        dplyr::filter(phase == "pre_impact")
      
      profile_post_impact_non_data_cols <- profile %>%
        dplyr::filter(phase == "post_impact") %>%
        dplyr::select(all_of(non_data_cols))
      profile_post_impact <- profile %>%
        dplyr::filter(phase == "post_impact") %>%
        dplyr::select(-all_of(non_data_cols))
      
      # Resample post-impact profile for this senband using the bare minimum
      # of the profile.
      C_sample <- min(ncol(profile_post_impact), floor((projection_length_months - 1) / stretch_factor + 1))
      input_lag_numbers <- seq(0, C_sample - 1)
      
      profile_post_impact_resampled <- mojstockr_resample_profile(input_lag_numbers, profile_post_impact[, 1:C_sample], stretch_factor = stretch_factor) %>%
        dplyr::mutate(lag_names = paste0("lag", x_resampled), .keep = "unused") %>%
        tidyr::pivot_wider(names_from = lag_names, values_from = probabilities_resampled) %>%
        dplyr::bind_cols(profile_post_impact_non_data_cols, .)
      
      # Bind pre- and post-impact profiles together and pad with zeroes as
      # necessary
      profile_resampled <- dplyr::bind_rows(profile_pre_impact[, 1:C], profile_post_impact_resampled) %>%
        replace(is.na(.), 0)
    }
    
    
    # Combine profiles for different senbands and pad with zeroes as necessary
    if (i == 0) {
      profiles_resampled <- profile_resampled
    } else {
      profiles_resampled <- dplyr::bind_rows(profiles_resampled, profile_resampled) %>%
        replace(is.na(.), 0)
    }
    
    i <- i + 1
    
  }
  
  # Sort order of rows in profiles_resampled to match order of rows in profiles
  profiles_resampled <- profiles_resampled %>%
    dplyr::arrange(phase, senband)
  
  return(profiles_resampled)
}



#' Stretch time on recall by a user-defined factor
#' 
#'#' @export
stretch_recall_time_lever <- function(recall_time, lever_profiles_recall_stretch_factors) {
  
  recall_time_levered      <-  multiply_two_named_vectors(recall_time, lever_profiles_recall_stretch_factors, arguments_to_keep = c("senband1", "senband2", "senband3", "senband4"))
  
  profiles_recall_levered  <- make_lag_filters(recall_time_levered)
  
  profiles_recall_levered_post <- dplyr::mutate(profiles_recall_levered, phase = "post_impact", .after=1)
  profiles_recall_levered_pre  <- dplyr::filter(profiles_recall, phase == "pre_impact")
  
  profiles_recall_levered <- dplyr::bind_rows(profiles_recall_levered_post, profiles_recall_levered_pre) %>%
    dplyr::mutate(dplyr::across(c(all_of(names(dplyr::select(., -c("senband", "phase"))))), ~tidyr::replace_na(.,0))) 
  
}