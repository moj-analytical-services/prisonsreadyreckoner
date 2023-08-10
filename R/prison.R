# This script provides functions for projecting the prison population



################################################################################
# Remand prisoners
################################################################################


#' Build population impact filters for use in remand modelling. Inflow impacts.
#' 
#' Function to generate a linear response function (filter) representing the  
#' unit population impact of a receipt in the magistrates' court on remand
#' population. It is assumed that only a fraction of relevant receipts will
#' be associated with remand and that some proportion of these will leave remand
#' after the custody time limit (CTL).
#' 
#' @param remand_rate The fraction of receipts, to which this filter will be
#'   applied, being remanded in custody.
#' @param no_bail_rate The fraction of those remanded in custody who will be
#'   kept in prison after their CTL has exceeded.
#' @param ctl The custody time limit in months.
#' @return A tibble representing, for lags starting from zero, the unit remand
#'   population impact of a magistrates' court receipt.
#' @export
make_remand_filter_in <- function(remand_rate, no_bail_rate, ctl, projection_length_months) {
  
  ctl <- min(ctl, projection_length_months)
  
  filter_remand <- tibble::tibble(casetype = "remand",
                                  lag = seq(0, projection_length_months-1), 
                                  impact = remand_rate * c(rep(1, ctl), rep(no_bail_rate, projection_length_months - ctl)))
  
  filter_remand <- tidyr::pivot_wider(filter_remand, names_from = "lag", names_prefix = "lag", values_from = "impact")
  
}


#' Build population impact filters for use in remand modelling. Outflow impacts.
#' 
#' Function to generate a linear response function (filter) representing the
#' unit population impact of a disposal in the Crown court on remand population.
#' It is assumed that only a fraction of relevant disposals will be associated
#' with remand and that some proportion of these will have left remand after the
#' custody time limit (CTL) anyway. Additionally assuming that the disposal can
#' occur any time within the individual's CTL, the filter is most negative at a
#' lag of zero and declines linearly throughout a period equivalent to the CTL.
#' A steady state is reached, representing the probability that the individual's
#' circumstances do not warrant release on bail.
#' 
#' @param remand_rate The fraction of disposals, to which this filter will be
#'   applied, having been remanded in custody at disposal.
#' @param no_bail_rate The fraction of those remanded in custody who will be
#'   kept in prison after their CTL has exceeded.
#' @param ctl The custody time limit in months.
#' @return A tibble representing, for lags starting from zero, the unit remand
#'   population impact of a Crown Court disposal.
#' @export
make_remand_filter_out <- function(remand_rate, no_bail_rate, ctl, projection_length_months) {
  
  bail_rate <- 1 - no_bail_rate
  #min_step <- max(1, ctl - projection_length_months + 1)
  min_step <- max(1, (ctl - 1) - projection_length_months + 1)
  
  # Calculations chosen to match Jordan's.
  filter_remand <- tibble::tibble(casetype = "remand",
                                  lag = seq(0, projection_length_months-1), 
                                  #impact = -remand_rate * c(no_bail_rate + bail_rate * seq(ctl, min_step, -1) / ctl, rep(no_bail_rate, max(0, projection_length_months - ctl))))
                                  impact = -remand_rate * c(no_bail_rate + bail_rate * seq((ctl - 1), min_step, -1) / ctl, rep(no_bail_rate, max(0, projection_length_months - (ctl - 1)))))
  
  filter_remand <- tidyr::pivot_wider(filter_remand, names_from = "lag", names_prefix = "lag", values_from = "impact")
  
}


calculate_pop_remand_delta <- function(mc_disposals, cc_disposals, profiles_remand_in, profiles_remand_out) {
  
  # Find change in receipts relevant for remand.
  # As magistrates' court disposals are generated from receipts without
  # adjustment, it is acceptable to use disposals as a proxy for receipts.
  receipts_remand_delta <- dplyr::filter(mc_disposals, .data$remanded == TRUE) %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(n_disposals_delta = sum(.data$n_disposals_delta), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "date",
                       values_from = "n_disposals_delta",
                       values_fill = 0,
                       names_sort = TRUE) %>%
    dplyr::mutate(casetype = "remand", .before = 1)
  
  # Convert change in receipts to population impact.
  pop_remand_delta_in <- mojstockr_mconv(receipts_remand_delta, profiles_remand_in, c("casetype"))
  
  
  # Find Crown Court disposals relevant for remand.
  disposals_remand_delta <- dplyr::group_by(cc_disposals, .data$date) %>%
    dplyr::summarise(n_disposals_delta = sum(.data$n_disposals_delta), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "date",
                       values_from = "n_disposals_delta",
                       values_fill = 0,
                       names_sort = TRUE) %>%
    dplyr::mutate(casetype = "remand", .before = 1)
  
  # Convert Crown Court disposals to population impact.
  pop_remand_delta_out <- mojstockr_mconv(disposals_remand_delta, profiles_remand_out, c("casetype"))
  
  
  pop_remand_delta <- rbind(pop_remand_delta_in, pop_remand_delta_out) %>%
    dplyr::group_by(.data$casetype) %>%
    dplyr::summarise((dplyr::across(tidyselect::where(is.numeric), sum)), .groups = 'drop')
  
}


check_pop_remand <- function(pop_remand_delta, published_remand_pop) {
  
  if (any(dplyr::select_if(pop_remand_delta, is.numeric) < -published_remand_pop))
    warning("Assumption violation: ",
            "The remand population fell below zero. ",
            "Please provide a scenario with more court receipts or fewer sitting days.")
  
}



################################################################################
# Determinate prisoners
################################################################################


# Load determinate sentence inflows from the central projection
load_inflows_det <- function(prison_inflows_file, start_date, forecast_start_date, forecast_end_date) {
  
  inflows_det <- import_s3_file(prison_inflows_file, sheet = "determinate_flows") %>%
    dplyr::rename(date = Date, senband = Band) %>%
    dplyr::filter(DataType == "Forecast")
  
  inflows_det <- trim_dates(inflows_det, start_date, forecast_start_date, forecast_end_date) %>%
    dplyr::select(date, senband, inflows)
  
  inflows_det <- dplyr::mutate(inflows_det, senband = stringr::str_replace_all(senband, c(Band = "senband", EDS = "senband4"))) %>%
    dplyr::group_by(date, senband) %>%
    dplyr::summarise(inflows = sum(inflows), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = date, values_from = inflows, values_fill = 0)
  
}


# Load determinate sentence profiles.
load_profiles_det <- function(profiles_file, projection_length_months, lever_profiles_det_stretch_factor_min, non_data_cols = c("senband", "phase")) {
  
  profiles <- import_s3_file(profiles_file)
  
  # We use all.equal() rather than a simple x!=y test because some profiles may
  # not add up to 1 exactly owing to rounding error.
  if (!isTRUE(all.equal(sum(profiles[, !names(profiles) %in% non_data_cols]), nrow(profiles))))
    warning("At least one input profile, does not sum to one. Are you sure it is a probability distribution?")
  
  # Calculate maximum length of profile that can be expected based on minimum
  # stretch factor
  max_profile_length <-
    floor(projection_length_months / lever_profiles_det_stretch_factor_min + 1)
  
  # Trim profiles to length dictated by
  # params$lever_profiles_det_stretch_factor_min to improve computational
  # efficiency. Not strictly necessary as stretch_profiles trims further. Left 
  # in, however, to make the table being passed around as small as possible.
  profiles <- profiles[, 1:min(ncol(profiles), length(non_data_cols) + max_profile_length)]
  
  return(profiles)
}


# Add phase
add_phases <- function(time_series, impact_date = NULL) {
  
  time_series_post           <- time_series %>% dplyr::mutate(phase = "post_impact", .after = 1)
  time_series_pre            <- time_series_post %>% dplyr::mutate(phase = "pre_impact")
  time_series                <- rbind(time_series_post, time_series_pre)
  
  if (!is.null(impact_date)) {
    coldates_post          <- c(FALSE, FALSE, as.Date(names(time_series[-c(1,2)])) >= as.Date(impact_date))
    coldates_pre           <- c(FALSE, FALSE, as.Date(names(time_series[-c(1,2)])) < as.Date(impact_date))
    time_series[coldates_post] <- time_series[coldates_post] * c(1,1,1,1,0,0,0,0)
    time_series[coldates_pre]  <- time_series[coldates_pre] * c(0,0,0,0,1,1,1,1)
  }
  
  return(time_series)
}


#' Add delta inflows to base inflows
add_inflows_det_delta_courts <- function(inflows, delta) {
  
  inflows_delta <- dplyr::bind_rows(inflows, delta) %>%
    dplyr::mutate(dplyr::across(c(all_of(names(dplyr::select(., -c("senband"))))), ~tidyr::replace_na(.,0))) %>%
    dplyr::group_by(senband) %>%
    dplyr::summarise((dplyr::across(tidyselect::where(is.numeric), sum))) %>%
    dplyr::arrange(senband)
  
  return(inflows_delta)
}



################################################################################
# Indeterminate prisoners
################################################################################

# Set up indeterminate sentence inflows
# Assume that same number of indeterminate offenders enters the population
# every month. This number is the average value for the last twelve months,
# calculated from quarterly statistics.
calculate_inflows_indet <- function(flow, month_names)
  calculate_flows_indet(flow, month_names)


# Set up indeterminate sentence outflows
# Assume no departures in the forecast horizon.
calculate_outflows_indet <- function(month_names)
  calculate_flows_indet(0, month_names)


calculate_flows_indet <- function(flow, month_names) {
  
  flows <- tibble::tibble(x = month_names, y = rep(flow, length(month_names))) %>%
    tidyr::pivot_wider(names_from = x, values_from = y) %>%
    dplyr::mutate(casetype = "indeterminate", .before = everything())
}



################################################################################
# Licence and recall
################################################################################

# Load determinate sentence profiles.
load_profiles_lic <- function(profiles_file, projection_length_months) {
  
  profiles <- suppressMessages(import_s3_file(profiles_file)) %>%
    dplyr::rename(senband = SenBand)
  
  # We use all.equal() rather than a simple x!=y test because some profiles may
  # not add up to 1 exactly owing to rounding error.
  non_data_cols <- c('senband')
  if (!isTRUE(all.equal(sum(profiles$p), length(unique(profiles$senband)))))
    warning("At least one input profile, does not sum to one. Are you sure it is a probability distribution?")
  
  profiles <- dplyr::filter(profiles, time_months < projection_length_months) %>%
    dplyr::select(time_months, senband, p)
  
  profiles <-
    dplyr::arrange(profiles, senband) %>%
    tidyr::pivot_wider(
      names_from = time_months,
      values_from = p,
      values_fill = 0,
      names_prefix = "lag",
      names_sort = TRUE
    ) %>%
    dplyr::mutate(senband = paste0("senband", senband))
  
  
  return(profiles)
}


# Load licence and recall parameters from Jordan Carroll
load_recall_params <- function(recall_file, start_date, forecast_start_date, forecast_end_date) {
  
  nomis_out_delius_in_ratio <- import_s3_file(recall_file, sheet = "NOMIS_out_delius_in_ratio") %>%
    dplyr::rename(senband = SenBand) %>%
    dplyr::mutate(senband = paste0("senband", senband)) %>%
    dplyr::arrange(senband) %>%
    tibble::deframe()
  
  
  # Recall rate. Currently, a time series is provided. We will assume the steady
  # state values.
  recall_rate_exclPSS <- import_s3_file(recall_file, sheet = "recall_rate_exclPSS") %>%
    dplyr::rename(senband = SenBand) %>%
    dplyr::mutate(date = as.Date(date))
  
  recall_rate_exclPSS <- trim_dates(recall_rate_exclPSS, start_date, forecast_start_date, forecast_end_date) %>%
    dplyr::mutate(senband = paste0("senband", senband))
  
  recall_rate_exclPSS <- dplyr::filter(recall_rate_exclPSS, date == forecast_end_date) %>%
    dplyr::select(senband, recall_rate) %>%
    dplyr::arrange(senband) %>%
    tibble::deframe() %>%
    signif(3)
  
  
  # Convert from days to months and remove 0 and NA lines
  average_time_on_recall <- import_s3_file(recall_file, sheet = "average_time_on_recall") %>%
    dplyr::rename(senband = SenBand) %>%
    dplyr::filter(senband > 0) %>%
    dplyr::mutate(average = average / 30.4375,
                  senband = paste0("senband", senband)) %>%
    dplyr::arrange(senband) %>%
    tibble::deframe()
  
  recall_profile_adjustments <- import_s3_file(recall_file, sheet = "recall_profile_adjustments") %>%
    dplyr::rename(senband = SenBand) %>%
    dplyr::mutate(senband = paste0("senband", senband)) %>%
    dplyr::arrange(senband) %>%
    tibble::deframe()
  
  return(
    list(
      nomis_out_delius_in_ratio       = nomis_out_delius_in_ratio,
      #licence_profile_adjustments_exc = licence_profile_adjustments_exc,
      recall_rate_exclPSS             = recall_rate_exclPSS,
      average_time_on_recall          = average_time_on_recall,
      recall_profile_adjustments      = recall_profile_adjustments
    )
  )
  
}


#' Build lag filters for use in licence recall modelling
#' 
#' Function to generate a linear response function (filter) representing a 
#' fixed delay, including fractional time step delays.
#' 
#' @param lags Lags to be modelled
#' @return A data frame of lag filters, one row per sentence band
#' 
#' @export
make_lag_filters <- function(lags) {
  
  R <- length(lags)
  C <- ceiling(max(lags)) + 1  # + 1 to include the zero lag time step.
  
  # Matrix has C + 1 columns to include column "senband"
  lag_filters <- matrix(0, R, C + 1, dimnames = list(NULL, c("senband" , paste0("lag", seq(0, C-1)))))
  
  i <- 0
  for (senband in names(lags)) {
    i <- i + 1
    lag_filters[i, 2:ncol(lag_filters)] <- mojstockr_make_lag_filter(lags[[senband]], C-1)
    lag_filters[i, 1] <- senband
  }
  
  # Convert to tibble for consistency with other functions in this collection.
  lag_filters <- lag_filters %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(-senband, as.numeric))
}


# Multiply volumes, organised by date, in tibble by ratios in named character
# array, with ratios split by senband. If you wish, pass two character arrays of
# ratios, and apply different ratios before and after a specified impact date.
apply_ratios <- function(input_values, preimpact_ratios, column_to_match = "senband", postimpact_ratios = NULL, impact_date = NULL) {
  
  multiply_by_ratios <- function(input, ratios) {
    
    # In this command, convert output to dataframe and then back to tibble to
    # remove inner column names that have been added to mutate
    output <- input %>%
      #dplyr::mutate(across(-column_to_match, ~ .x * ratios)) %>%
      dplyr::mutate(dplyr::across(-tidyselect::all_of(column_to_match), ~ .x * ratios)) %>%
      as.data.frame() %>%
      tibble::as_tibble()
    
  }
  
  if (!(column_to_match %in% colnames(input_values))) {
    stop("The tibble \"input_values\" passed to apply_ratios() contains no column named \"", column_to_match, "\". (Note that \"column_to_match\" is an optional argument to the function apply_ratios(). By default, \"column_to_match\" is set to \"senband\".) Please either explicitly set the argument \"column_to_match\" as appropriate to your input tibble or ensure that the input tibble contains a column named \"senband\".")
  }
  
  if (nrow(input_values) != length(preimpact_ratios)) {
    stop("The length of the character vector \"preimpact_ratios\" does not match the number of rows in the tibble \"input_values\". Please ensure that the length of the character vector \"preimpact_ratios\" equals the number of rows in the tibble \"input_values\".")
  }
  
  if (!identical(names(preimpact_ratios), input_values[[column_to_match]])) {
    stop("The names within the character vector \"preimpact_ratios\" do not match the names within the column \"", column_to_match, "\" in the tibble \"input_values\". Please ensure that the names within the character vector \"preimpact_ratios\" match the names within the column \"", column_to_match, "\" in the tibble \"input_values\".")
  }
  
  if (is.null(postimpact_ratios) && is.null(impact_date)) {
    
    output_values <- multiply_by_ratios(input_values, preimpact_ratios)
    
  } else if (!is.null(postimpact_ratios) && !is.null(impact_date)) {
    
    if (nrow(input_values) != length(postimpact_ratios)) {
      stop("The length of the character vector \"postimpact_ratios\" does not match the number of rows in the tibble \"input_values\". Please ensure that the length of the character vector \"postimpact_ratios\" equals the number of rows in the tibble \"input_values\".")
    }
    
    if (!identical(names(postimpact_ratios), input_values[[column_to_match]])) {
      stop("The names within the character vector \"postimpact_ratios\" do not match the names within the column \"", column_to_match, "\" in the tibble \"input_values\". Please ensure that the names within the character vector \"postimpact_ratios\" match the names within the column \"", column_to_match, "\" in the tibble \"input_values\".")
    }
    
    output_values_pre  <- multiply_by_ratios(input_values, preimpact_ratios)
    output_values_post <- multiply_by_ratios(input_values, postimpact_ratios)
    
    input_values_values <- input_values %>%
      #dplyr::select(-c(column_to_match))
      dplyr::select(-tidyselect::all_of(column_to_match))
    
    coldates_pre           <- c(FALSE, as.Date(names(input_values_values)) < as.Date(impact_date))
    coldates_post          <- c(FALSE, as.Date(names(input_values_values)) >= as.Date(impact_date))
    
    output_values <- input_values
    output_values[coldates_pre] <- output_values_pre[coldates_pre]
    output_values[coldates_post] <- output_values_post[coldates_post]
    
  } else if (!is.null(postimpact_ratios) & is.null(impact_date)) {
    stop("Only optional argument postimpact_ratios, and not optional argument impact_date, is non-null in function apply_ratios(). If you wish to specify a change in ratios after an impact date, please ensure that both postimpact_ratios and impact_date are set to be non-null.")
  } else if (is.null(postimpact_ratios) & !is.null(impact_date)) {
    stop("Only optional argument impact_date, and not optional argument postimpact_ratios, is non-null in function apply_ratios(). If you wish to specify a change in ratios after an impact date, please ensure that both postimpact_ratios and impact_date are set to be non-null.")
  }
  
  return(output_values)
  
}



################################################################################
# Gender splits
################################################################################


load_gender_splits <- function(gender_splits_file) {
  
  gender_splits <- import_s3_file(gender_splits_file) %>%
    dplyr::select(-c("n_sex"))
  
}


#' @export
split_populations_by_gender <- function(pop, gender_splits) {
  
  pop <- pop %>%
    dplyr::left_join(gender_splits, by = c("casetype", "senband"), na_matches = "na", multiple = "all", unmatched = "error") %>%
    dplyr::mutate(population = population * prop_sex) %>%
    #dplyr::arrange(run, date, casetype, senband, sex) %>%    # Commented for speed. No need to sort here.
    dplyr::select(run, date, casetype, senband, sex, population)
  
}