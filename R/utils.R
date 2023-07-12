# This script contains basic functions used at various locations, not tied to
# any particular section of the model.


################################################################################
# General
################################################################################

#' Deploy the magrittr pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Provide access to the .data object in calls to tidyverse functions
#' 
#' @name .data
#' @importFrom rlang .data
NULL


#' Temporarily turn off debug messages when botor interacts with S3 while the
#' calling function is active.
#'
#' @export
silence_botor <- function() {
  
  botor_threshold <- logger::log_threshold(namespace = 'botor')
  logger::log_threshold(logger::WARN, namespace = 'botor')
  withr::defer(logger::log_threshold(attr(botor_threshold, "level"), namespace = 'botor'), envir = parent.frame())
  
}



################################################################################
# Data manipulation
################################################################################

# Combine time series across phases
combine_phases <- function(flows) {
  
  # Be careful. This will re-order by factors alphabetically and return a
  # tibble.
  flows <-
    dplyr::group_by(flows, senband) %>% dplyr::summarise(dplyr::across(-c("phase"), sum), .groups = 'drop')
  
}



################################################################################
# File management
################################################################################

# A function to read files of various file types from s3.
import_s3_file <- function(path_s3, ...) {
  
  ext <- tolower(tools::file_ext(path_s3))
  tbl <- switch (
    ext,
    'xlsx' = botor::s3_read(path_s3, readxl::read_excel, ...),
    'csv'  = botor::s3_read(path_s3, readr::read_csv, show_col_types = FALSE, ...),
    stop("Extension, '",
         ext,
         "', not recognised.")
  )
  
  return(tibble::as_tibble(tbl))
}


write_table <- function(table, path_output, ...) {
  
  silence_botor()
  
  if (!is.data.frame(table))
    stop("Argument, table, must be a tibble or a data.frame.")
  
  tryCatch(
    botor::s3_write(table, readr::write_csv, path_output, ...),
    error = function(msg)
      stop(
        "I was unable to save the output table with assigned path, ",
        path_output,
        ". Check you have access to the full path.\nbotor says: ",
        msg
      )
  )
  
  return(TRUE)
}


#' Multiply the values of two vectors based on name
#' 
#' Match the arguments of two vectors together based on name, and then multiply
#' the argument values together. If names are present in \code{vector1} but not
#' \code{vector2}, the returned vector will contain \code{NA} values
#' corresponding to those names. If names are present in \code{vector2} but not
#' \code{vector1}, they will be dropped from \code{vector2}. The optional
#' argument \code{arguments to keep} can be used to specify which arguments to
#' keep in the returned vector.
#' 
#' @param vector1 Vector.
#' @param vector2 Vector.
#' @param arguments_to_keep \emph{Optional.} Character vector. Specifies which
#'   arguments to keep in the returned vector.
#' @return A vector containing the multiplication of values in \code{vector1}
#'   and \code{vector2}.
#' @examples
#' \dontrun{
#' recall_time <- multiply_two_named_vectors(average_time_on_recall,
#'                  recall_profile_adjustments,
#'                  arguments_to_keep = c("senband1", "senband2", "senband3", "senband4"))
#' }
#' @export
multiply_two_named_vectors <- function(vector1, vector2, arguments_to_keep = NULL) {
  
  if (!is.null(arguments_to_keep)) {
    mapply('*', vector1, vector2[names(vector1)])[arguments_to_keep]
  } else {
    mapply('*', vector1, vector2[names(vector1)])
  }
  
}


