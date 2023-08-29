# This script provides functions for converting court disposals to determinate
# sentence prisoner receptions.



#' Calculate receptions by sentence band by applying reception rates to a series
#' of disposal volumes by disposal type.
#'
#' @param cc_disposals A tibble representing a time series of disposal volumes
#'   by type.
#' @param sentencing_rates A tibble with columns, \code{disposal_type},
#'   \code{senband1}, \code{senband2}, \code{senband3} and \code{senband4},
#'   representing the coefficients for converting disposal types to receptions
#'   by sentence band.
calculate_inflows_det_delta <- function(cc_disposals, mc_disposals, sentencing_rates) {
  
  # Sum over receipt_type_desc as our linear model is based on route only.
  cc_disposals <- dplyr::mutate(cc_disposals, disposal_type = paste0("cc_", route)) %>%
                    dplyr::group_by(date, disposal_type) %>%
                    dplyr::summarise(n_disposals_delta = sum(n_disposals_delta, na.rm = TRUE), .groups = "drop")
  
  mc_disposals <- dplyr::select(mc_disposals, -tidyselect::any_of(c("remanded")))
  
  # Combine Crown Court disposals with magistrates' court disposals.
  disposals <- dplyr::bind_rows(cc_disposals, mc_disposals)
  
  inflows <- calculate_inflows_det_delta_SUB(disposals, sentencing_rates)

}


# A function split from the parent function to allow unit testing.
calculate_inflows_det_delta_SUB <- function(disposals, sentencing_rates) {

  inflows <-
    dplyr::inner_join(disposals, sentencing_rates, by = "disposal_type") %>%
    dplyr::mutate(
      senband1 = n_disposals_delta * senband1,
      senband2 = n_disposals_delta * senband2,
      senband3 = n_disposals_delta * senband3,
      senband4 = n_disposals_delta * senband4
    ) %>%
    dplyr::select(-n_disposals_delta) %>%
    tidyr::pivot_longer(tidyr::starts_with("senband"),
                        names_to = "senband",
                        values_to = "inflows_delta"
    ) %>%
    dplyr::group_by(date, senband) %>%
    dplyr::summarise(inflows_delta = sum(inflows_delta), .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = date,
                       values_from = inflows_delta,
                       values_fill = 0,
                       names_sort = TRUE)

}

