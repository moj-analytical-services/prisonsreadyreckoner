% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prison.R
\name{make_lag_filters}
\alias{make_lag_filters}
\title{Build lag filters for use in licence recall modelling}
\usage{
make_lag_filters(lags)
}
\arguments{
\item{lags}{Lags to be modelled}
}
\value{
A data frame of lag filters, one row per sentence band
}
\description{
Function to generate a linear response function (filter) representing a
fixed delay, including fractional time step delays.
}
