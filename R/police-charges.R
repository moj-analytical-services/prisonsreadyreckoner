# Functions for loading additional Crown Court receipt and magistrates' court
# disposals, based on various police charge scenarios.

# load_police_charges_cc_data_list <- function(police_charges_cc_route_file, police_charges_cc_files, police_charges_central_scenario, ringfenced_lookup, start_date, forecast_start_date, forecast_end_date) {
#   
#   # Load file of ratios from Chun-Yee
#   police_charges_cc_route <- load_police_charges_cc_route(police_charges_cc_route_file)
#   
#   extra_police_charges_cc_list <- list()
#   
#   for (police_charges_cc_scenario in names(police_charges_cc_files)) {
#     
#     # Unlike the other scenarios, which are differences from the Central, the
#     # forecast in the Central file is the actual Central forecast. Set to NA to
#     # signal that it is to be ignored.
#     if (police_charges_cc_scenario == police_charges_central_scenario) {
#       
#       extra_police_charges_cc_list[[police_charges_cc_scenario]] <- NA
#       
#     } else {
#       
#       police_charges_cc_file <- police_charges_cc_files[[police_charges_cc_scenario]]
#       extra_police_charges_cc <- load_police_charges_cc_file(police_charges_cc_file, start_date, forecast_start_date, forecast_end_date)
#       extra_police_charges_cc <- calculate_police_charge_routes(police_charges_cc_route, extra_police_charges_cc)
#       extra_police_charges_cc_list[[police_charges_cc_scenario]] <- add_lag_by_cc_route(extra_police_charges_cc, ringfenced_lookup, forecast_start_date, forecast_end_date)
#       
#     }
#     
#   }
#   
#   return(extra_police_charges_cc_list)
# }


load_police_charges_cc_data <- function(police_charges_cc_files, police_charges_central_scenario, police_charges_cc_route_file, ringfenced_lookup, start_date, forecast_start_date, forecast_end_date) {
  
  # Load police charge routes
  police_charges_cc_route <- load_police_charges_cc_route(police_charges_cc_route_file)
  
  # Build up a combined tibble from each file
  extra_police_charges_cc <- tibble::tibble(scenario = character(), date = as.Date(numeric()), receipt_type = character(), n_receipts_delta = numeric())
  for (police_charges_cc_file in police_charges_cc_files) {
    
    extra_police_charges_cc <- import_s3_file(police_charges_cc_file) %>% 
      trim_dates(start_date, forecast_start_date, forecast_end_date) %>% 
      rbind(extra_police_charges_cc)
  }
  
  # Convert to court route and add a lag for receipt to disposal lag.
  extra_police_charges_cc <- calculate_police_charge_routes(extra_police_charges_cc, police_charges_cc_route)
  extra_police_charges_cc <- add_lag_by_cc_route(extra_police_charges_cc, ringfenced_lookup, forecast_start_date, forecast_end_date)
  
  # Some rudimentary checks
  # tally <- dplyr::group_by(extra_police_charges_cc, .data$scenario) %>%
  #            dplyr::count()
  # if (length(unique(tally$n)) != 1)
  #   stop("incompatible data n CC police charges file.")
  
  # Convert the tibble to a list of tibbles named after each scenario.
  extra_police_charges_cc_list <- list()
  police_charges_cc_scenarios <- unique(extra_police_charges_cc$scenario)
  for (police_charges_cc_scenario in police_charges_cc_scenarios) {
    
    extra_police_charges_cc_list[[police_charges_cc_scenario]] <- dplyr::filter(extra_police_charges_cc, scenario == police_charges_cc_scenario) %>%
      dplyr::select(tidyselect::all_of(c("date", "receipt_type_desc", "route", "n_receipts_delta")))
  }
  
  
  # For the central forecast set to NA to signal this adds no volume and is to
  # be ignored.
  extra_police_charges_cc_list[[police_charges_central_scenario]] <- NA
  
  return(extra_police_charges_cc_list)
}


load_police_charges_cc_route <- function(police_charges_cc_route_file) {
  
  police_charges_cc_route <- import_s3_file(police_charges_cc_route_file) %>%
    dplyr::select(-tidyselect::any_of(c("london_split", "total", "grouping", "ccs_snapshot_date"))) %>%
    dplyr::mutate(receipt_type = paste0(receipt_type_desc, "_", offence_group_rap))
  
}

# 
# load_police_charges_cc_file <- function(police_charges_cc_file, start_date, forecast_start_date, forecast_end_date) {
#   
#   # I am using col_select to avoid having to specify the type of a column with
#   # an empty header.
#   col_select <- c("month_year", "type", "n_receipts_delta")
#   col_types <-
#     readr::cols(
#       #`` = readr::col_integer(),
#       month_year = readr::col_date(format = '%Y-%m-%d'),
#       #scenario = readr::col_character(),
#       #forecast_id = readr::col_character(),
#       #forecast_round = readr::col_character(),
#       type = readr::col_character(),
#       n_receipts_delta = readr::col_double()
#     )
#   
#   # Suppress message about adding a new column name.
#   extra_police_charges_cc <- suppressMessages(import_s3_file(police_charges_cc_file, col_select = tidyselect::all_of(col_select), col_types = col_types))
#   
#   extra_police_charges_cc <- dplyr::rename(extra_police_charges_cc, date = month_year) %>%
#     trim_dates(start_date, forecast_start_date, forecast_end_date) %>%
#     #dplyr::select(-c("...1", "scenario", "forecast_id", "forecast_round")) %>%
#     dplyr::mutate(type = dplyr::if_else(type == "app_All", "app_app", type),
#                   type = dplyr::if_else(type == "sent_bb_All", "sent_sent", type),
#                   type = dplyr::if_else(type == "sent_cs_All", "sent_sent", type))
#   
#   # Change the sign because the files are supplied as central - scenario, not
#   # scenario - central.
#   extra_police_charges_cc['n_receipts_delta'] = - extra_police_charges_cc['n_receipts_delta']
#   
#   return(extra_police_charges_cc)
# }


calculate_police_charge_routes <- function(police_charges_cc, police_charges_cc_route) {
  
  police_charges_cc_split <- police_charges_cc %>%
    dplyr::left_join(police_charges_cc_route, by = "receipt_type") %>%
    dplyr::mutate(app = n_receipts_delta * app,
                  e_other = n_receipts_delta * e_other,
                  effective = n_receipts_delta * effective,
                  egp = n_receipts_delta * egp,
                  gp_cracked = n_receipts_delta * gp_cracked,
                  l_other = n_receipts_delta * l_other,
                  lgp = n_receipts_delta * lgp,
                  other_cracked = n_receipts_delta * other_cracked,
                  sent = n_receipts_delta * sent) %>%
    dplyr::select(-tidyselect::any_of(c("n_receipts_delta", "receipt_type", "offence_group_rap"))) %>%
    tidyr::pivot_longer(cols = c("app", "e_other", "effective", "egp", "gp_cracked", "l_other", "lgp", "other_cracked", "sent"), names_to = "route", values_to = "n_receipts_delta") %>%
    dplyr::group_by(scenario, date, receipt_type_desc, route) %>%
    dplyr::summarise(across(c(n_receipts_delta), sum), .groups = "drop")
  
}


# Add lags from the ringfenced lookup table. This is to align with the CJS
# Crown Court model, which adds a lag to receipts.
add_lag_by_cc_route <- function(extra_police_charges_cc, ringfenced_lookup, forecast_start_date, forecast_end_date) {
  
  add.months <- function(d, m)
    lubridate::add_with_rollback(d, months(m))
  
  
  # Drop unmatched keys, which should represent the unused app case types in the
  # routes table, provided the ringfenced lookup table has been constructed
  # correctly.
  extra_police_charges_cc <- dplyr::right_join(extra_police_charges_cc, ringfenced_lookup,
                                               by = c("receipt_type_desc", "route"), unmatched = "drop")
  
  # Add dates and reconstruct as dates as mapply drops the class to numeric.
  # Really?!
  extra_police_charges_cc <-
    dplyr::mutate(extra_police_charges_cc,
                  date = as.Date(mapply(add.months, .data$date, .data$lag_months), origin = "1970-01-01")) %>%
    dplyr::select(tidyselect::all_of(c("scenario", "date", "receipt_type_desc", "route", "n_receipts_delta"))) %>%
    dplyr::filter(.data$date <= forecast_end_date)
  
  # Add a dummy time series and pivot to add zeros to vacated dates.
  extra_police_charges_cc <- rbind(extra_police_charges_cc, data.frame(scenario = "dummy", date = seq(forecast_start_date, forecast_end_date, by = "month"), receipt_type_desc = "dummy", route = "dummy", n_receipts_delta = 0)) %>%
    tidyr::pivot_wider(names_from = "date", values_from = "n_receipts_delta", values_fill = 0) %>%
    dplyr::filter(.data$receipt_type_desc != "dummy") %>%
    tidyr::pivot_longer(!c("scenario", "receipt_type_desc", "route"), names_to = "date", values_to = "n_receipts_delta")
  
  # Tidy. Re-arranges columns into the original order and orders rows by date as
  # a date.
  extra_police_charges_cc <- dplyr::select(extra_police_charges_cc, tidyselect::all_of(c("scenario", "date", "receipt_type_desc", "route", "n_receipts_delta"))) %>%
    dplyr::mutate(date = as.Date(.data$date)) %>%
    dplyr::arrange(.data$scenario, .data$date, .data$receipt_type_desc, .data$route, .data$n_receipts_delta)
  
}



### NEW MAGISTRATES' COURT FUNCTIONS USING DATA FROM KATIE MAHON ###

load_police_charges_mc_data <- function(police_charges_mc_files, police_charges_central_scenario, mc_remand_lookup, start_date, forecast_start_date, forecast_end_date) {
  
  # Build up a combined tibble from each file
  extra_police_charges_mc <- tibble::tibble(scenario = character(), date = as.Date(numeric()), disposal_type = character(), n_disposals_delta = numeric())
  for (police_charges_mc_file in police_charges_mc_files) {
    
    extra_police_charges_mc <- import_s3_file(police_charges_mc_file) %>% 
      trim_dates(start_date, forecast_start_date, forecast_end_date) %>% 
      rbind(extra_police_charges_mc)
    
  }
  
  # # Some rudimentary checks
  # tally <- dplyr::group_by(extra_police_charges_mc, .data$scenario) %>%
  #            dplyr::count()
  # if (length(unique(tally$n)) != 1)
  #   stop("incompatible data n MC police charges file.")
  
  # Add flag for indicating which disposals are relevant for calculating remand.
  extra_police_charges_mc <- dplyr::left_join(extra_police_charges_mc, mc_remand_lookup, by = "disposal_type", unmatched = "error")
  
  # # Add central scenario and find change in disposals.
  # police_charges_mc_central_scenario <- dplyr::filter(extra_police_charges_mc, scenario == police_charges_central_scenario)
  # extra_police_charges_mc <- dplyr::left_join(extra_police_charges_mc, police_charges_mc_central_scenario, by = c("date", "disposal_type"), suffix = c("", ".y"), unmatched = "error") %>%
  #   dplyr::mutate(n_disposals_delta = n_disposals - n_disposals.y)
  
  
  # Convert the tibble to a list of tibbles named after each scenario.
  extra_police_charges_mc_list <- list()
  police_charges_mc_scenarios <- unique(extra_police_charges_mc$scenario)
  for (police_charges_mc_scenario in police_charges_mc_scenarios) {
    
    extra_police_charges_mc_list[[police_charges_mc_scenario]] <- dplyr::filter(extra_police_charges_mc, scenario == police_charges_mc_scenario) %>%
      dplyr::select(tidyselect::all_of(c("date", "disposal_type", "remanded", "n_disposals_delta")))
  }
  
  # For the central forecast set to NA to signal this adds no volume and is to
  # be ignored.
  extra_police_charges_mc_list[[police_charges_central_scenario]] <- NA
  
  return(extra_police_charges_mc_list)
}
