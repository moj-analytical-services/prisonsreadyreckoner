# General functions that could be placed in a separate stock-flow modelling
# package.


################################################################################
# Data preparation
################################################################################

#' Calculate conditional probabilities over multiple variables
#' 
#' Group dataframe rows that have non-unique combinations in specified columns
#' and, for each group, calculate the proportion of data belonging to specified
#' subgroups.
#'
#' \code{mojstockr_calculate_proportions} accepts an input dataframe and groups
#' row that have non-unique combinations of values in the columns specified by
#' the argument \code{predicate_columns}. Within each of the resulting groups,
#' rows are further grouped into subgroups that have non-unique combinations of
#' values in the columns specified by the argument \code{conditional_columns}.
#' The proportions of data within each subgroup are then calculated. (Note that
#' these proportions are calculated relative to the size of the group housing
#' the subgroup, \emph{not} relative to the size of all data within the whole
#' dataframe.) By default, proportions are based on the number of rows belonging
#' to each subgroup. Different rows can be given different weightings using
#' values in a column specified by the optional argument \emph{weights_column}.
#'
#' @param df String. Dataframe with multiple columns.
#' @param predicate_columns String. Columns on which to group data into groups.
#' @param conditional_columns String. Columns on which to group data into
#'   subgroups.
#' @param proportions_column_name \emph{Optional.} The name of the column
#'   containing the calculated proportions. Defaults to 'proportion'.
#' @param weights_column \emph{Optional.} String. Column containing weights to
#'   use in calculation of proportions. Higher values correspond to higher
#'   weights.
#' @return A tibble listing the groups and subgroups, and giving the proportion
#'   of data within each subgroup. Subgroups without any counts are omitted.
#' @examples
#' \dontrun{
#' df <- data.frame(a = c("France", "France", "France", "France", "Germany",
#'                        "Germany", "Germany", "Germany", "Poland", "Poland",
#'                        "Ukraine", "Ukraine", "Ukraine", "Ukraine", "Ukraine"),
#'                  b = c("Farm", "Farm", "Farm", "Village", "City", "City",
#'                        "City", "City", "Village", "Town", "Town", "Town",
#'                        "Town", "Town", "Town"),
#'                  c = c("male", "female", "unknown", "male", "female",
#'                        "unknown", "unknown", "unknown", "unknown", "unknown",
#'                         "male", "female", "female", "female", "female"),
#'                  d = c("A", "B", "C", "D","E", "F", "G", "H", "A", "B", "C",
#'                        "D", "E", "F", "G"),
#'                  volume = c(12, 20, 8, 15, 25, 20, 7, 2, 38, 3, 45, 2, 9, 13,
#'                             16)
#'        )
#' 
#' prob <- mojstockr_calculate_proportions(df, c('a', 'b'), c('c'), weights_column = "volume")
#' }
#' @export
mojstockr_calculate_proportions <- function(df, predicate_columns, conditional_columns, proportions_column_name = "proportion", weights_column = NULL) {
  
  if (is.null(weights_column)) {
    n_x <- dplyr::count(df, dplyr::across(tidyselect::all_of(predicate_columns)), name = "group_total")
    n_xy <- dplyr::count(df, dplyr::across(tidyselect::all_of(append(predicate_columns,
                                                                conditional_columns))), name = "subgroup_total")
  } else {
    n_x <- dplyr::count(df, dplyr::across(tidyselect::all_of(predicate_columns)), name = "group_total", wt = .data[[weights_column]])
    n_xy <- dplyr::count(df, dplyr::across(tidyselect::all_of(append(predicate_columns,
                                                                conditional_columns))), name = "subgroup_total", wt = .data[[weights_column]])
  }

  n_all <- merge(n_x, n_xy, by = predicate_columns)
  n_all[{{proportions_column_name}}] <- n_all["subgroup_total"] / n_all["group_total"]
  n_all <- dplyr::select(n_all, !c("group_total", "subgroup_total"))
  
  n_all <- tibble::as_tibble(n_all) # For consistency with other functions in this collection
  
  return(n_all)
}


#' Resample a discrete probability distribution that is defined at equidistant
#' time-steps (we refer to such a distribution as a profile)
#'
#' \code{mojstockr_resample_profiles} uses linear interpolation to increase or
#' decrease the number of points at which a profile is defined. The input
#' profile consists of probabilities, \code{probabilities}, that are defined at
#' the equally spaced values \code{x}. One (but not both) of the optional
#' arguments \code{x_resampled} and \code{stretch_factor} must be supplied in
#' order to define the points at which resampled probabilities are calculated.
#' The values within \code{x_resampled} need not be monotonically increasing or
#' equally spaced. If \code{stretch_factor} is supplied,
#' \code{mojstockr_resample_profiles} generates a vector \code{x_resampled}
#' consisting of \code{ceiling(length(x) \* stretch_factor)} equally spaced
#' values that lie between \code{min(x \* stretch_factor)} and \code{max(x \*
#' stretch_factor)}.
#' 
#' @param x Vector. Array containing points at which probabilities are defined.
#' @param probabilities Vector. Array containing probabilities corresponding to
#'   the points defined in \code{x}.
#' @param x_resampled \emph{Optional.} Vector. Array containing points at which
#'   resampled probabilities are defined.
#' @param stretch_factor \emph{Optional.} Float. The resampled probability
#'   distribution will be stretched along the x-axis by the factor
#'   \code{stretch_factor}.
#' @return A tibble containing the resampled probability distribution, with
#'   columns \code{x_resampled} and \code{probabilities_resampled}. The values
#'   of \code{x_resampled} are separated by the same interval that separates
#'   values of \code{x}.
#' @examples
#' \dontrun{
#' dummy_profile <- data.frame(a = c("t0", "t1", "t2", "t3", "t4", "t5", "t6",
#'                                   "t7", "t8", "t9", "t10", "t11"),
#'                             t = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
#'                             proportions = c(0.1, 0.2, 0.03, 0.07, 0.05, 0.05,
#'                                             0.01, 0.1, 0.1, 0.1, 0.1, 0.09)
#' )
#' 
#' returned_tibble <-
#'   mojstockr_resample_profiles(dummy_profile$t,
#'                               dummy_profile$proportions,
#'                               x_resampled = seq(from = 0, to = 24))
#' 
#' returned_tibble <-
#'   mojstockr_resample_profiles(dummy_profile$t, dummy_profile$proportions, 
#'                               stretch_factor = 2.5)
#' }
#' @export
mojstockr_resample_profile <- function(x, probabilities, x_resampled = NULL, stretch_factor = NULL) {

  if (is.null(x_resampled) && is.null(stretch_factor))
    stop("Neither parameter x_resampled nor parameter stretch_factor has been supplied. Please supply one of these factors.")
  else if (!is.null(x_resampled) && !is.null(stretch_factor))
    stop("Both parameter x_resampled and parameter stretch_factor have been supplied. Please supply only one of these factors.")
  
  if (!is.null(x_resampled) && length(x_resampled) == 1)
    stop(paste0("x_resampled is a vector of length 1 containing the value ", x_resampled, ". mojstockr_resample_profile() will return a single probability of 1 (i.e. a delta function), centred at ", x_resampled, ". Consider supplying a vector with more than one element."))

  if (!is.null(stretch_factor)) {
    if (stretch_factor < 0)
      stop("stretch_factor < 0. It is not possible to stretch the probability distribution by a negative factor. Please set stretch_factor to be greater than 0.")
    if (stretch_factor == 0)
      stop("stretch_factor cannot equal 0. Please set stretch_factor to be greater than 0.")

    # IMPORTANT: I DON'T BELIEVE THE NEXT LINE IS APPROPRIATAE WHEN MIN(X)!=0
    x_resampled <- seq(min(x), by = 1, length.out = ceiling(length(x) * stretch_factor))
    #x_resampled <- seq(from = min(x * stretch_factor), to = max(x * stretch_factor), length.out = ceiling(length(x) * stretch_factor))
  } else
    stretch_factor <- max(x_resampled) / max(x)

  # Recalibrate x to lie between 0 and 1
  temp_x <- x / max(x)
  temp_x_resampled <- x_resampled / max(x_resampled)
  # temp_x <- seq(from = 0, to = 1, length.out = length(x))
  # temp_x_resampled <- seq(from = 0, to = 1, length.out = length(x_resampled))
  
  # Resample the probabilities at the desired x
  sampling_function <- stats::approxfun(temp_x, probabilities)
  probabilities_resampled <- sampling_function(temp_x_resampled)
  
  # Renormalise probability function
  # probabilities_resampled <- probabilities_resampled / sum(probabilities_resampled)
  probabilities_resampled <- probabilities_resampled / stretch_factor
  
  if (0 < length(probabilities_resampled) && length(probabilities_resampled) <= 2)
    warning(paste0("The resampled probability function is only of length ", length(probabilities_resampled), ". Is this sufficient for your purposes?"))

  resampled_probabilities <- data.frame(x_resampled = x_resampled,
                               probabilities_resampled = probabilities_resampled)
  
  resampled_probabilities <- tibble::as_tibble(resampled_probabilities) # For consistency with other functions in this collection
  
  return(resampled_probabilities)
}



################################################################################
# Filtering
################################################################################

#' Build a lag filter for use in simplified stock-flow modelling
#' 
#' Function to generate a linear response function (filter) representing a 
#' fixed lag, including decimal time step lags.
#' 
#' For decimal lags, the mass of the response is shared between the
#' relevant consecutive time steps, in proportion to the fractional part of the
#' decimal. The first time step is assumed to represent a lag of zero.
#' 
#' For example, a lag of 1.2 would be shared between time step 2 and 3, with
#' respective masses, 0.8 and 0.2.
#' 
#' @param lag The lag to represent. A real, non-negative number.
#' @param N The maximum time step to include in the filter, counting from zero.
#'   If \code{N+1} is less than \code{as.integer(lag)+2} an error will occur.
#' @return A length \code{N+1} numeric vector, representing time steps 0 to N,
#'   populated with zeros except at the time step (or steps if lag is a decimal)
#'   representing the fixed delay from the origin.
#' @examples
#' \dontrun{
#' h <- make_lag_filter(1.2, 2)
#' }
#' @export
mojstockr_make_lag_filter <- function(lag, N = NULL) {
  
  if (is.null(N))
    N <- ceiling(lag)
  if (N < ceiling(lag))
    stop("Lag in lag filter is beyond the requested filter length.")
  
  h <- numeric(N + 1)
  
  n <- as.integer(lag)
  if (n == lag)
    h[n + 1] <- 1
  else {
    p <- lag - n
    h[n + 1] <- 1-p
    h[n + 2] <- p
  }
  
  return(h)
}


#' Convolution of multiple time series with a matched set of filters.
#'
#' Takes a set of time series, \code{x}, and linear response function (filter
#' kernel), \code{h}, and generates an output, \code{y}, as the convolution of
#' each time series with its filter.
#' 
#' Both \code{x} and \code{h} must be in a 'horizontal' format where, following
#' some non-data columns, consecutive time steps are represented as consecutive
#' columns. The non-data columns would typically identify the time series for a
#' given row. For example,
#' 
#' \code{h <- dataframe(sex = c("female", "male"), t0 = c(1, 1), t1 = c(0.6,
#'                0.5), t2 = c(0.3, 0.1))}
#'                
#' The first time step of the filter should be a the zero-lag response *h\[0\]*.
#' 
#' The identity of any non-data columns should be specified as a character
#' vector of column names, e.g. \code{c("age", "sex")}
#' 
#' h must have the same number of rows as x but is not expected to have the same
#' number of data columns.
#' 
#' Convolution is with the base function, \code{stats::convolve()}. This uses
#' the Fast Fourier Transform and will return real-valued output. You may wish
#' to round output, if working with integer-valued systems.
#'
#' @param x A set of time series in 'horizontal' format with identifying, 'non-
#'   data' columns on the left and data columns on the right. Data columns
#'   contain the time series, one time series per row.
#' @param h A set of linear response functions, one for each time series. Non-
#'   data columns are assumed to be the same as for \code{x}. Data columns are
#'   assumed to start at zero lag.
#' @param non_data_cols A character vector of non-data columns to be excluded
#'   from the convolution calculation.
#' @return A tibble in the same format and dimensions as \code{x}, providing the
#'   convolution of each row of \code{x} with the corresponding row of \code{h}.
#' @examples
#' x <- data.frame(sex = c("female", "male"), t0 = rpois(2, 10),
#'                t1 = rpois(2, 10), t2 = rpois(2, 10), t3 = rpois(2, 10),
#'                t4 = rpois(2, 10), t5 = rpois(2, 10))
#' h <- data.frame(sex = c("female", "male"), lag0 = c(1, 1),
#'                lag1 = c(0.6, 0.5), lag2 = c(0.3, 0.1))
#' y <- mojstockr_mconv(x, h, non_data_cols = c("sex"))
#' @export
mojstockr_mconv <- function(x, h, non_data_cols = NULL) {
  
  # FYI, works with null non_data_cols fine.
  non_data   <- dplyr::select(x, tidyselect::all_of(non_data_cols))
  non_data_h <- dplyr::select(h, tidyselect::all_of(non_data_cols))
  x <- dplyr::select(x, -tidyselect::all_of(non_data_cols)) %>% as.matrix()
  h <- dplyr::select(h, -tidyselect::all_of(non_data_cols)) %>% as.matrix()
  
  if (nrow(x) != nrow(h))
    stop("Inconsistent rows between time series and filter tables.")
  if (!all(non_data_h == non_data))
    stop("Time series table, x, and filter table, h, have inconsistent labels.")
  if (!is.numeric(x))
    stop("Some data columns in time series table, x, had non-numeric data.")
  if (!is.numeric(h))
    stop("Some data columns in filter table, h, had non-numeric data.")

  
  y <- x * 0
  C <- ncol(x)
  for (i in 1:nrow(x)) {
    
    y_i <- stats::convolve(x[i, ], rev(h[i, ]), type = "o")
    y[i, ] <- y_i[1:C]   # Remove the tail from the convolution output to trim to original data length.
    
  }

  y <- cbind(non_data, y)
  y <- tibble::as_tibble(y) # For consistency with other functions in this collection.

  return(y)
}


#' Build multiple stocks from corresponding inflows and outflows.
#' 
#' Function to construct a stock as the sum of all inflows minus the sum of all
#' outflows.
#' 
#' Inflows and outflows must be a data frame of the same general structure,
#' having a set of non-data columns and a set of data columns. Consecutive data
#' columns will represent flows at consecutive time steps.
#' 
#' The function works by binding the inflows to minus the outflows, summing
#' across the two and then integrating across time steps.
#' 
#' @param inflows A data frame of inflows with flows in consecutive time steps
#'   represented by consecutive 'data' columns.
#' @param outflows A data frame of outflows with flows in consecutive time steps
#'   represented by consecutive 'data' columns.
#' @param non_data_cols A character vector specifying the columns in inflows and
#'   outflows designated as non-data.
#' @return A tibble representing the integration of inflows and outflows for
#'   each factor in the two data frames where factors will be designated by
#'   \code{non_data_cols}
#' @examples
#' \dontrun{
#' inflows <- dataframe(sex = c("female", "male"), t0 = rpois(2, 10),
#'                t1 = rpois(2, 10), t2 = rpois(2, 10), t3 = rpois(2, 10),
#'                t4 = rpois(2, 10), t5 = rpois(2, 10))
#' outflows <- dataframe(sex = c("female", "male"), t0 = rep(2, 2),
#'                t1 = rep(2, 2), t2 = rep(2, 2), t3 = rep(2, 2),
#'                t4 = rep(2, 2), t5 = rep(2, 2))
#' stock <- build_stock(inflows, outflows, c("sex"))
#' }
#' @export
mojstockr_build_stock <- function(inflows, outflows, non_data_cols = NULL) {
  
  inflows <- dplyr::mutate(inflows, flow = "in")

  outflows <- dplyr::mutate(outflows, flow = "out") %>% 
    dplyr::mutate(dplyr::across(-c(tidyselect::all_of(non_data_cols), "flow"), ~ -.x))

  # Be careful. This also re-orders by character columns alphabetically and
  # converts to a tibble.
  stock <- rbind(inflows, outflows) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of((non_data_cols)))) %>%
    dplyr::summarise(dplyr::across(-c("flow"), sum), .groups = 'drop')

  data_cols <- !names(stock) %in% non_data_cols
  stock[, data_cols] <- t(apply(stock[, data_cols], 1, cumsum))

  return(stock)
}

