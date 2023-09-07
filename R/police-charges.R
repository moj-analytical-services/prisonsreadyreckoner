# Functions for loading additional Crown Court receipt and magistrates' court
# disposals, based on various police charge scenarios.

load_police_charges_cc_data_list <- function(police_charges_cc_route_file, police_charges_cc_files, ringfenced_lookup, start_date, forecast_start_date, forecast_end_date) {
  
  # Load file of ratios from Chun-Yee
  police_charges_cc_route <- load_police_charges_cc_route(police_charges_cc_route_file)
  
  extra_police_charges_cc_list <- list()
  
  for (police_charges_cc_scenario in names(police_charges_cc_files)) {
    
    # Unlike the other scenarios, which are differences from the Central, the
    # forecast in the Central file is the actual Central forecast. Set to NA to
    # signal that it is to be ignored.
    if (police_charges_cc_scenario == "central") {
      
      extra_police_charges_cc_list[[police_charges_cc_scenario]] <- NA
      
    } else {
      
      police_charges_cc_file <- police_charges_cc_files[[police_charges_cc_scenario]]
      extra_police_charges_cc <- load_police_charges_cc_file(police_charges_cc_file, start_date, forecast_start_date, forecast_end_date)
      extra_police_charges_cc <- calculate_police_charge_routes(police_charges_cc_route, extra_police_charges_cc)
      extra_police_charges_cc_list[[police_charges_cc_scenario]] <- add_lag_by_cc_route(extra_police_charges_cc, ringfenced_lookup, forecast_start_date, forecast_end_date)
      
    }
      
  }
  
  return(extra_police_charges_cc_list)
}


load_police_charges_cc_route <- function(police_charges_cc_route_file) {

  police_charges_cc_route <- import_s3_file(police_charges_cc_route_file) %>%
    dplyr::select(-tidyselect::any_of(c("london_split", "total", "grouping", "ccs_snapshot_date"))) %>%
    dplyr::mutate(type = paste0(receipt_type_desc, "_", offence_group_rap))
  
}


load_police_charges_cc_file <- function(police_charges_cc_file, start_date, forecast_start_date, forecast_end_date) {
  
  # I am using col_select to avoid having to specify the type of a column with
  # an empty header.
  col_select <- c("month_year", "type", "forecast_saved")
  col_types <-
    readr::cols(
      #`` = readr::col_integer(),
      month_year = readr::col_date(format = '%Y-%m-%d'),
      #scenario = readr::col_character(),
      #forecast_id = readr::col_character(),
      #forecast_round = readr::col_character(),
      type = readr::col_character(),
      forecast_saved = readr::col_double()
    )

  # Suppress message about adding a new column name.
  extra_police_charges_cc <- suppressMessages(import_s3_file(police_charges_cc_file, col_select = tidyselect::all_of(col_select), col_types = col_types))
  
  extra_police_charges_cc <- dplyr::rename(extra_police_charges_cc, date = month_year) %>%
    trim_dates(start_date, forecast_start_date, forecast_end_date) %>%
    #dplyr::select(-c("...1", "scenario", "forecast_id", "forecast_round")) %>%
    dplyr::mutate(type = dplyr::if_else(type == "app_All", "app_app", type),
                  type = dplyr::if_else(type == "sent_bb_All", "sent_sent", type),
                  type = dplyr::if_else(type == "sent_cs_All", "sent_sent", type))
  
  # Change the sign because the files are supplied as central - scenario, not
  # scenario - central.
  extra_police_charges_cc['forecast_saved'] = - extra_police_charges_cc['forecast_saved']

  return(extra_police_charges_cc)
}


calculate_police_charge_routes <- function(police_charges_cc_route, police_charges_cc) {
  
  police_charges_cc_split <- police_charges_cc %>%
    dplyr::left_join(police_charges_cc_route, by = "type") %>%
    dplyr::mutate(app = forecast_saved * app,
                  e_other = forecast_saved * e_other,
                  effective = forecast_saved * effective,
                  egp = forecast_saved * egp,
                  gp_cracked = forecast_saved * gp_cracked,
                  l_other = forecast_saved * l_other,
                  lgp = forecast_saved * lgp,
                  other_cracked = forecast_saved * other_cracked,
                  sent = forecast_saved * sent) %>%
    dplyr::select(-tidyselect::any_of(c("forecast_saved", "type", "offence_group_rap"))) %>%
    tidyr::pivot_longer(cols = c("app", "e_other", "effective", "egp", "gp_cracked", "l_other", "lgp", "other_cracked", "sent"), names_to = "route", values_to = "n_receipts_delta") %>%
    dplyr::group_by(date, receipt_type_desc, route) %>%
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
    dplyr::select(tidyselect::all_of(c("date", "receipt_type_desc", "route", "n_receipts_delta"))) %>%
    dplyr::filter(.data$date <= forecast_end_date)

  # Add a dummy time series and pivot to add zeros to vacated dates.
  extra_police_charges_cc <- rbind(extra_police_charges_cc, data.frame(date = seq(forecast_start_date, forecast_end_date, by = "month"), receipt_type_desc = "dummy", route = "dummy", n_receipts_delta = 0)) %>%
    tidyr::pivot_wider(names_from = "date", values_from = "n_receipts_delta", values_fill = 0) %>%
    dplyr::filter(.data$receipt_type_desc != "dummy") %>%
    tidyr::pivot_longer(!c("receipt_type_desc", "route"), names_to = "date", values_to = "n_receipts_delta")

  # Tidy. Re-arranges columns into the original order and orders rows by date as
  # a date.
  extra_police_charges_cc <- dplyr::select(extra_police_charges_cc, tidyselect::all_of(c("date", "receipt_type_desc", "route", "n_receipts_delta"))) %>%
    dplyr::mutate(date = as.Date(.data$date)) %>%
    dplyr::arrange(.data$date, .data$receipt_type_desc, .data$route, .data$n_receipts_delta)
  
}



### NEW MAGISTRATES' COURT FUNCTIONS USING DATA FROM KATIE MAHON ###

load_police_charges_mc_data <- function(police_charges_mc_files, police_charges_mc_central_scenario, mc_remand_lookup, start_date, forecast_start_date, forecast_end_date) {

  # Build up a combined tibble from each file
  extra_police_charges_mc <- tibble::tibble(scenario = character(), date = as.Date(numeric()), disposal_type = character(), n_disposals = numeric())
  for (police_charges_mc_file in police_charges_mc_files)
    extra_police_charges_mc <- import_s3_file(police_charges_mc_file) %>% rbind(extra_police_charges_mc)
  
  # # Some rudimentary checks
  # tally <- dplyr::group_by(extra_police_charges_mc, .data$scenario) %>%
  #            dplyr::count()
  # if (length(unique(tally$n)) != 1)
  #   stop("inpcompatible data n MC police charges file.")
  
  police_charges_mc_central_scenario <- extra_police_charges_mc %>%
      dplyr::filter(scenario == police_charges_mc_central_scenario) %>%
      trim_dates(start_date, forecast_start_date, forecast_end_date)
  
  
  # Add flag for indicating which disposals are relevant for calculating remand.
  extra_police_charges_mc <- dplyr::left_join(extra_police_charges_mc, mc_remand_lookup, by = "disposal_type", unmatched = "error")


  extra_police_charges_mc_list <- list()
  police_charges_mc_scenarios <- unique(extra_police_charges_mc$scenario)
  for (police_charges_mc_scenario in police_charges_mc_scenarios) {
    
    extra_police_charges_mc_scenario <- extra_police_charges_mc %>%
      dplyr::filter(scenario == police_charges_mc_scenario) %>%
      dplyr::right_join(police_charges_mc_central_scenario, by = c("date", "disposal_type")) %>%
      dplyr::select(-tidyselect::any_of(c("scenario.x", "scenario.y"))) %>%
      dplyr::mutate(n_disposals_delta = n_disposals.x - n_disposals.y) %>%
      dplyr::select(-tidyselect::any_of(c("n_disposals.x", "n_disposals.y")))
    
    extra_police_charges_mc_list[[police_charges_mc_scenario]] <- extra_police_charges_mc_scenario
    
  }
  
  return(extra_police_charges_mc_list)
}

# load_police_charges_mc_data <- function(police_charges_mc_file, police_charges_mc_scenarios, police_charges_mc_sheet, mc_remand_lookup, start_date, forecast_start_date, forecast_end_date) {
#   
#   extra_police_charges_mc <- suppressMessages(import_s3_file(police_charges_mc_file, sheet = police_charges_mc_sheet)) %>%
#     dplyr::filter(rdos_type == "DISPOSALS") %>%
#     dplyr::select(tidyselect::all_of(c("year_month", "type", "forecast_count", "scenario"))) %>%
#     dplyr::mutate(date = as.Date(year_month),
#                   disposal_type = type,
#                   n_disposals = forecast_count,
#                   .keep = "unused") %>%
#     dplyr::mutate(disposal_type = dplyr::if_else(disposal_type == "IO", "mc_ind", disposal_type),
#                   disposal_type = dplyr::if_else(disposal_type == "SM_other", "mc_sm", disposal_type),
#                   disposal_type = dplyr::if_else(disposal_type == "SNM_other", "mc_snm", disposal_type),
#                   disposal_type = dplyr::if_else(disposal_type == "TEW", "mc_tew", disposal_type))
# 
#   police_charges_mc_central_scenario <- extra_police_charges_mc %>%
#     dplyr::filter(scenario == police_charges_mc_scenarios[["central"]]) %>%
#     trim_dates(start_date, forecast_start_date, forecast_end_date)
#   
#   
#   # Add flag for indicating which disposals are relevant for calculating remand.
#   extra_police_charges_mc <- dplyr::left_join(extra_police_charges_mc, mc_remand_lookup, by = "disposal_type", unmatched = "error")
#     
#   
#   extra_police_charges_mc_list <- list()
#   
#   for (police_charges_mc_scenario in names(police_charges_mc_scenarios)) {
#     
#     police_charges_mc_scenario_columnvalue <- police_charges_mc_scenarios[[police_charges_mc_scenario]]
#     
#     extra_police_charges_mc_scenario <- extra_police_charges_mc %>%
#       dplyr::filter(scenario == police_charges_mc_scenario_columnvalue) %>%
#       dplyr::right_join(police_charges_mc_central_scenario, by = c("date", "disposal_type")) %>%
#       dplyr::select(-tidyselect::any_of(c("scenario.x", "scenario.y"))) %>%
#       dplyr::mutate(n_disposals_delta = n_disposals.x - n_disposals.y) %>%
#       dplyr::select(-tidyselect::any_of(c("n_disposals.x", "n_disposals.y")))
#       
#     extra_police_charges_mc_list[[police_charges_mc_scenario]] <- extra_police_charges_mc_scenario
#     
#   }
#   
#   return(extra_police_charges_mc_list)
#   
# }

