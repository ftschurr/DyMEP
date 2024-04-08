% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DyMEP_pheno_phase_prediction.R
\name{pheno_phase_prediction}
\alias{pheno_phase_prediction}
\title{function to predict the a phenological phase in winter wheat}
\usage{
pheno_phase_prediction(
  phase_covariate_list,
  environmental_data,
  phase_starting_date,
  crop_abbrev,
  output_type = "dates"
)
}
\arguments{
\item{phase_covariate_list}{list like: list("sowing-emergence" =
c("tas","VPD","SPI"),
"emergence-jointing" = c("tas"))
indicating per phenological phase the covariates to use. List of
of phenological phases must be consecutive!}

\item{environmental_data}{data.frame with the necessary environmental data,
one column must be "DATE" (as.Date format), the others with the names of
the environmental covariates (e.g. tas, tasmin etc.)}

\item{phase_starting_date}{starting date of the first phase which will be
predicted (object of class "Date" (use as.Date()))}

\item{crop_abbrev}{abbreviation of the crop to be modeled (valid crop_abbrevs
can be found with available_crops_and_phases())}

\item{output_type}{either "dates" or "detailed_information"; defines what
output of the model they user wants to have as return,
default is set to "dates". If a user wants to get the response parameters,
curves, predictions and model thresholds, it should be chosen
"detailed_information"
output = "dates" will return a dataframe with the stages and according dates
output = "detailed_information" will return a list with the dates, but also
 the corresponding dose response parameters and predictions}
}
\value{
returns the end-date of each phase
}
\description{
predict one or all phenological phases
}
\examples{
pheno_phase_prediction(phase_covariate_list = list(
        "sowing-emergence" = c("tasmin","VPD","SPI"),
        "emergence-jointing"= c("tas","tasmin","VPD","SPI"),
        "jointing-heading" = c("global_radiation","tas","SPI")),
      environmental_data= data.frame(
          "DATE"=seq.Date(from = as.Date("2021-01-01"),
                          to = as.Date("2022-12-31"),by=1),
          "tas"=runif(730,min=-10,max=40),
          "tasmin"=runif(730,min=-10,max=40),
          "VPD" = runif(730,min=-10,max=40),
          "SPI"= runif(730,min=-1,max=4),
          "global_radiation"= runif(730,min=0,max=10000)),
      phase_starting_date =as.Date("2022-01-01"),
      crop_abbrev = "WR")
}
\keyword{phase}
\keyword{phenology}
\keyword{prediction}