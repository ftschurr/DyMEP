% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DyMEP_utils_prediction_helper_functions.R
\name{predhelputils.get_all_permutations}
\alias{predhelputils.get_all_permutations}
\title{A function to get all potential permutations}
\usage{
predhelputils.get_all_permutations(your_environmental_covariates)
}
\arguments{
\item{your_environmental_covariates}{list of the environmental covariates you
have at hand (tas, tasmax, tasmin, RH, VPD, SPI, global_radiation are
 available)}
}
\value{
provides all potential permutations given your input vector
}
\description{
A function to get all potential permutations
}
\examples{
predhelputils.get_all_permutations(your_environmental_covariates =
                                      c("tas","tasmin","VPD"))
}
\keyword{internal}
