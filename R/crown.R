# A script for holding Crown Court module functions.


load_ringfenced_lookup <- function(ringfenced_lookup_file) {
  
  col_types <-
    readr::cols(
      receipt_type_desc = readr::col_character(),
      route = readr::col_character(),
      ringfenced = readr::col_logical(),
    )
  
  ringfenced_lookup <- import_s3_file(ringfenced_lookup_file, col_types = col_types)
  
  # One could potentially add some validation but all files should have been
  # validated at creation with the relevant update_*() function.
}


load_crown_output <- function(cc_output_file, start_date, forecast_start_date, forecast_end_date) {
  
  col_types <-
    readr::cols(
      Time = readr::col_integer(),
      date = readr::col_date(format = '%Y-%m-%d'),
      FY = readr::col_character(),
      receipt_type_desc = readr::col_character(),
      region = readr::col_character(),
      actual_route = readr::col_character(),
      n_backlog = readr::col_integer(),
      n_receipts = readr::col_integer(),
      n_disposals = readr::col_integer(),
      dur_backlog = readr::col_double(),
      dur_receipts = readr::col_double(),
      dur_disposals = readr::col_double(),
      scenario = readr::col_character(),
      dev = readr::col_character(),
      scenario_dev = readr::col_character()
    )
  
  cc_output <- import_s3_file(cc_output_file, col_types = col_types) %>%
                 dplyr::rename(route = actual_route)
  
  cc_output <- trim_dates(cc_output, start_date, forecast_start_date, forecast_end_date) %>%
                 dplyr::select(date, receipt_type_desc, route,
                               n_backlog,                                 # Read n_backlog so we may sense-check backlog depletion.
                               n_disposals, dur_disposals)

  return(cc_output)
}


load_cc_capacity <- function(cc_capacity_file, start_date, forecast_start_date, forecast_end_date) {
  
  col_types <-
    readr::cols(
      TimePeriod = readr::col_integer(),
      monthly_ht = readr::col_double(),
      monthly_sd = readr::col_double(),
      htpsd_adj = readr::col_double(),
      date_value = readr::col_date(format = '%d/%m/%Y'),
      raw_sd = readr::col_double() #, raw_seasonal_sd = readr::col_double() # Field to be added at next iteration. See '20230505 - To Chun-yee Cheng - RE_ cjst adjusted hearing time per sitting day - save to s3 as standard.msg'
    )
  
  cc_capacity <- import_s3_file(cc_capacity_file, col_types = col_types) %>%
                   dplyr::rename(date = date_value, sitting_days = monthly_sd, hours_per_day = htpsd_adj)
  
  cc_capacity <- trim_dates(cc_capacity, start_date, forecast_start_date, forecast_end_date) %>%
                   dplyr::select(date, sitting_days, hours_per_day) %>%
                   dplyr::arrange(date)   # Do not remove this arrange as it is relied upon by calculate_hours_ringfenced_delta().
  
  return(cc_capacity)
}


check_cc_inputs <- function(cc_output, cc_capacity) {
  
  # Test 1. Do both files have the same dates?
  tryCatch({
      dplyr::left_join(unique(cc_output['date']), cc_capacity, by = c("date"), unmatched = "error")
    },
    error = function(msg) {
      stop("Crown Court input files have unmatched rows: ", msg)
    }
  )
  
  # Test 2. Do both files have almost the same implied court time?
  disposal_hours <- dplyr::group_by(cc_output, date) %>%
                      dplyr::summarise(hours_disposals = sum(dur_disposals / 60), .groups = "drop")
  
  cc_capacity <- dplyr::left_join(cc_capacity, disposal_hours, by = "date") %>%
                   dplyr::mutate(court_time = sitting_days * hours_per_day, error = court_time - hours_disposals)
  
  max_error <- max(cc_capacity$error)
  if (max_error > 50)
      stop("Crown Court input files have capacity discrepancies. A maximum error was found of ",  max_error, ".")
  
  invisible(TRUE)
}


# Add extra columns that will be needed in later calculations. Where values are
# not known (at the point this function is called), set to appropriate dummy
# values.
#
# Intended to be called immediately after loading the basic table from file. By
# adding these columns on loading, the table structure is more transparent and
# less CPU time will be spent manipulating the table, especially in contexts
# where model runs are invoked iteratively, such as in a Shiny app.
augment_crown_output <- function(cc_output, ringfenced_lookup) {
    
  # Add ring-fenced status.
  cc_output <- dplyr::left_join(cc_output, ringfenced_lookup, by = c("receipt_type_desc", "route"), unmatched = "error")

  # Calculate parameters that will be used in online calculations. All derived
  # time parameters will be in hours.
  cc_output <- dplyr::group_by(cc_output, date) %>%
                    dplyr::mutate(hours_non_ringfenced = sum((dur_disposals * !ringfenced) / 60)) %>%
                    dplyr::ungroup()
  
  cc_output <- dplyr::mutate(cc_output,
                                backlog_rate = (n_disposals * !ringfenced) / hours_non_ringfenced,
                                hours_per_disposal = dur_disposals / n_disposals / 60,
                                n_receipts_delta = 0,
                                n_disposals_ringfenced_delta = 0)
  
}


# Add extra columns that will be needed in later calculations. Where values are
# not known (at the point this function is called), set to appropriate dummy
# values.
#
# Intended to be called immediately after loading the basic table from file. By
# adding these columns on loading, the table structure is more transparent and
# less CPU time will be spent manipulating the table, especially in contexts
# where model runs are invoked iteratively, such as in a Shiny app.
augment_cc_capacity <- function(cc_capacity, cc_output) {
  
  base_hours <- dplyr::group_by(cc_output, date) %>%
                           dplyr::summarise(hours_ringfenced_base = sum(dur_disposals * ringfenced / 60),
                                            capacity_base = sum(dur_disposals / 60),
                                            .groups = "drop")
  
  cc_capacity <- dplyr::left_join(cc_capacity, base_hours, by = c("date"))
  
  cc_capacity <- dplyr::mutate(cc_capacity, sitting_days_delta = 0, hours_ringfenced_delta = 0, capacity_delta = 0)
  
}


# The input delta is assumed to have fields, date, receipt_type_desc, route and
# n_receipts_delta only.
add_cc_receipts_delta <- function(cc_output, receipts_delta) {
  
  cc_output <- dplyr::left_join(cc_output, receipts_delta, by = c("date", "receipt_type_desc", "route"), suffix = c("", ".y"), unmatched = "error") %>%
                 dplyr::mutate(n_receipts_delta = n_receipts_delta.y,
                               n_disposals_ringfenced_delta = n_receipts_delta.y * ringfenced) %>%
                 dplyr::select(-n_receipts_delta.y)
  
}


calculate_hours_ringfenced_delta <- function(cc_output, cc_capacity) {
  
  hours_ringfenced_delta <- dplyr::mutate(cc_output, delta_hours = n_disposals_ringfenced_delta * hours_per_disposal) %>%
                              dplyr::group_by(date) %>%
                              dplyr::summarise(delta_hours = sum(delta_hours), .groups = "drop")
  
  # For speed, we are assuming both cc_output and cc_capacity are
  # arranged by date. Demand is ordered by summarise(), above; capacity is
  # ordered on loading.
  cc_capacity$hours_ringfenced_delta <- hours_ringfenced_delta$delta_hours
  
  return(cc_capacity)
}


# Join cc_output and cc_capacity to calculate Crown Court disposals and
# calculate the number of additional disposals per route. Note that
# "receipt_type_desc" is ignored, as we know that all cases go via the Crown
# Court (e.g. receipt_type_desc = "ind" with route = "e_other" is counted as the
# same route as receipt_type_desc = "tew" with route = "e_other").
calculate_cc_disposals_delta <- function(cc_output, cc_capacity) {
  
  cc_disposals <- dplyr::left_join(cc_output, cc_capacity, by = c("date")) %>%
                    dplyr::mutate(n_disposals_delta = n_disposals_ringfenced_delta + (capacity_delta - hours_ringfenced_delta) * backlog_rate) %>%
                    dplyr::select(date, receipt_type_desc, route, 
                                  ringfenced,                          # Retain so we may check whether disposals deplete the backlog.
                                  n_receipts_delta,                    # Not currently used. Consider deleting.
                                  n_disposals_delta)
  
}


check_cc_capacity <- function(cc_capacity) {
  
  hours_ringfenced <- cc_capacity$hours_ringfenced_base + cc_capacity$hours_ringfenced_delta
  if (any(hours_ringfenced < 0))
    warning("Assumption violation: ",
            "The total duration of ring-fenced disposals implied by your court ",
            "receipts is less than zero for at least one month. ",
            "Please provide a scenario with a higher volume of court receipts.")
  
  residual_capacity <- (cc_capacity$capacity_base + cc_capacity$capacity_delta) - hours_ringfenced
  if (any(residual_capacity < 0))
    warning("Assumption violation: ",
            "There was not enough court capacity to serve the additional ring-fenced demand ",
            "implied by your court receipts. ",
            "Please provide a scenario with fewer court receipts or more sitting days.")
  
}


# A crude test of backlog depletion for speed. Assumes that one only needs to
# test the last date.
# Note, we are only interested in non-ring-fenced cases. By assumption, an
# increase in the ring-fenced receipts leads to an immediate increase in ring-
# fenced disposals with no impact on the backlog. A decrease in ring-fenced
# receipts would only start to impact the backlog once the drop is larger than
# the baseline ring-fenced disposals and we already have a check for that
# threshold.
check_cc_disposals_delta <- function(cc_output, cc_disposals_delta) {
  
  # Find the non-ringfenced backlog in the last month
  cc_backlog <- dplyr::filter(cc_output, date == max(cc_output$date), ringfenced == FALSE) %>%
    dplyr::group_by(.data$route) %>%
    dplyr::summarise(n_backlog = sum(.data$n_backlog, na.rm = TRUE))
  
  # Find the total additional disposals by the last month.
  cc_disposals_delta_total <- dplyr::filter(cc_disposals_delta, ringfenced == FALSE) %>%
    dplyr::group_by(.data$route) %>%
    dplyr::summarise(n_disposals_delta = sum(.data$n_disposals_delta, na.rm = TRUE))
  
  # Check that the total additional disposals did not exceed the backlog.
  cc_backlog <- dplyr::left_join(cc_backlog, cc_disposals_delta_total, by = "route")

  if (sum(cc_backlog$n_backlog) < sum(cc_backlog$n_disposals_delta)) {
    warning("Assumption violation: ",
            "The backlog of non-ring-fenced cases fell to zero by the end of the ",
            "simulation. ",
            "Please provide a scenario with more court receipts or fewer sitting days.")
  } else if (any(cc_backlog$n_backlog - cc_backlog$n_disposals_delta < 0)) {
    warning("Assumption violation: ",
            "The backlog of non-ring-fenced cases fell to zero by the end of the ",
            "simulation for at least one court route. ",
            "You may wish to provide a scenario with more court receipts or fewer sitting days.")
  }
    
}
  