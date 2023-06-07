# Functions for loading additional Crown Court receipt and magistrates' court
# disposals, based on various police charge scenarios.

load_police_charges_cc_data_list <- function(police_charges_cc_route_file, police_charges_cc_files, start_date, forecast_start_date, forecast_end_date) {
  
  # Load file of ratios from Chun-Yee
  police_charges_cc_route <- load_police_charges_cc_route(police_charges_cc_route_file)
  
  extra_police_charges_cc_list <- list()
  
  for (police_charges_cc_scenario in names(police_charges_cc_files)) {
    
    police_charges_cc_file <- police_charges_cc_files[[police_charges_cc_scenario]]
    
    extra_police_charges_cc <- load_police_charges_cc_file(police_charges_cc_file, start_date, forecast_start_date, forecast_end_date, police_charges_cc_scenario)
    
    extra_police_charges_cc_list[[police_charges_cc_scenario]] <- calculate_police_charge_routes(police_charges_cc_route, extra_police_charges_cc)
  }
  
  return(extra_police_charges_cc_list)
}


load_police_charges_cc_route <- function(police_charges_cc_route_file) {
  
  police_charges_cc_route <- import_s3_file(police_charges_cc_route_file) %>%
    dplyr::select(-c("london_split", "total", "grouping", "ccs_snapshot_date")) %>%
    dplyr::mutate(type = paste0(receipt_type_desc, "_", offence_group_rap))
  
}


load_police_charges_cc_file <- function(police_charges_cc_file, start_date, forecast_start_date, forecast_end_date, police_charges_cc_scenario) {
  
  # Suppress message about adding a new column name.
  extra_police_charges_cc <- suppressMessages(import_s3_file(police_charges_cc_file))
  
  extra_police_charges_cc <- dplyr::rename(extra_police_charges_cc, date = month_year) %>%
    trim_dates(start_date, forecast_start_date, forecast_end_date) %>%
    dplyr::select(-c("...1", "scenario", "forecast_id", "forecast_round")) %>%
    dplyr::mutate(type = dplyr::if_else(type == "app_All", "app_app", type),
                  type = dplyr::if_else(type == "sent_bb_All", "sent_sent", type),
                  type = dplyr::if_else(type == "sent_cs_All", "sent_sent", type))
  
  # Unlike the other scenarios, which are differences from the Central, the
  # forecast in the Central file is the actual Central forecast. Set the
  # difference to zero. For other scenarios, change the sign because the files
  # are supplied as central - scenario, not scenario - central.
  if (police_charges_cc_scenario == "central")
    extra_police_charges_cc['forecast_saved'] = 0
  else
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
    dplyr::select(-c("forecast_saved", "type", "offence_group_rap")) %>%
    tidyr::pivot_longer(cols = c("app", "e_other", "effective", "egp", "gp_cracked", "l_other", "lgp", "other_cracked", "sent"), names_to = "route", values_to = "n_receipts_delta") %>%
    dplyr::group_by(date, receipt_type_desc, route) %>%
    dplyr::summarise(across(c(n_receipts_delta), sum), .groups = "drop")
  
}


### NEW MAGISTRATES' COURT FUNCTIONS USING DATA FROM KATIE MAHON ###

load_police_charges_mc_data <- function(police_charges_mc_file, police_charges_mc_scenarios, police_charges_mc_sheet, start_date, forecast_start_date, forecast_end_date) {
  
  extra_police_charges_mc <- suppressMessages(import_s3_file(police_charges_mc_file, sheet = police_charges_mc_sheet)) %>%
    dplyr::filter(rdos_type == "DISPOSALS") %>%
    dplyr::select(c("year_month", "type", "forecast_count", "scenario")) %>%
    dplyr::mutate(date = as.Date(year_month),
                  disposal_type = type,
                  n_disposals = forecast_count,
                  .keep = "unused") %>%
    dplyr::mutate(disposal_type = dplyr::if_else(disposal_type == "IO", "mc_ind", disposal_type),
                  disposal_type = dplyr::if_else(disposal_type == "SM_other", "mc_sm", disposal_type),
                  disposal_type = dplyr::if_else(disposal_type == "SNM_other", "mc_snm", disposal_type),
                  disposal_type = dplyr::if_else(disposal_type == "TEW", "mc_tew", disposal_type))


  police_charges_mc_central_scenario <- extra_police_charges_mc %>%
    dplyr::filter(scenario == police_charges_mc_scenarios[["central"]]) %>%
    trim_dates(start_date, forecast_start_date, forecast_end_date)
  
  
  extra_police_charges_mc_list <- list()
  
  for (police_charges_mc_scenario in names(police_charges_mc_scenarios)) {
    
    police_charges_mc_scenario_columnvalue <- police_charges_mc_scenarios[[police_charges_mc_scenario]]
    
    extra_police_charges_mc_scenario <- extra_police_charges_mc %>%
      dplyr::filter(scenario == police_charges_mc_scenario_columnvalue) %>%
      dplyr::right_join(police_charges_mc_central_scenario, by = c("date", "disposal_type")) %>%
      dplyr::select(-c("scenario.x", "scenario.y")) %>%
      dplyr::mutate(n_disposals_delta = n_disposals.x - n_disposals.y) %>%
      dplyr::select(-c("n_disposals.x", "n_disposals.y"))
      
    extra_police_charges_mc_list[[police_charges_mc_scenario]] <- extra_police_charges_mc_scenario
    
  }
  
  return(extra_police_charges_mc_list)
  
}

