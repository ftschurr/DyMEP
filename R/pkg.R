#' @docType package
#' @name DyMEP
#' @title Dynamic Multi Environment Phenology-Model
#' @description
#' Empirically models/predicts the phenology (macro-phases) of 10 crop
#'  plants (trained on a big dataset over 80 years derived from the German
#'   weather service (DWD)). Can be applied for remote sensing purposes,
#'  environmental inputs can be chosen from a range of pre-trained response
#'  curves and applied to the trained crops and phenological phases.
#'  No retraining is done within the use of this package.
#'@examples
#'available <- available_crops_and_phases()
#'#what is the best environmental covariates for one or multiple phases?
#' # check what covairates are implemented in the model
#' available_covariates <- available_environmental_covariates()
#'
#'best_DyMEP_model(env_covariates = c("tas","tasmin","VPD","SPI",
#' "global_radiation","tasmax","RH"),
#'                 pheno_phases = c("sowing-emergence","jointing-heading"),
#'                 crop_abbrev = "WW")
#'# create a list of wanted phases and corresponding environmental covariates
#' phase_covariate_list <- list("sowing-emergence" = c("tasmin","VPD","SPI"),
#'                          "emergence-jointing"= c("tas","tasmin","VPD","SPI"),
#'                      "jointing-heading" = c("global_radiation","tas","SPI"))
#'
#'# alternatively you can create this input list directly like this with the
#'# best available model:
#'phase_covariate_list <- best_DyMEP_model(env_covariates =
#'c("tas","tasmin","VPD","SPI","global_radiation","tasmax","RH"),
#'pheno_phases = c("sowing-emergence","emergence-jointing","jointing-heading"),
#'crop_abbrev = "WW",
#'output_list_for_prediction = TRUE)
#'
#'
#'# create dummy environmental data
#' environmental_data <- data.frame("DATE" = seq.Date(
#'               from = as.Date("2021-01-01"), to = as.Date("2023-12-31"),by=1),
#'                           "tas"=runif(1095,min=-10,max=40),
#'                            "RH"=runif(1095,min=0,max=100),
#'                            "tasmin"=runif(1095,min=-10,max=40),
#'                            "tasmax"=runif(1095,min=-5,max=40),
#'                            "VPD" = runif(1095,min=0,max=40),
#'                            "SPI"= runif(1095,min=-1,max=4),
#'                            "global_radiation"= runif(1095,min=0,max=10000))
#'
#'
#' pheno_phase_prediction(phase_covariate_list = phase_covariate_list,
#'                       environmental_data = environmental_data,
#'                       phase_starting_date =as.Date("2021-01-01"),
#'                       crop_abbrev = "WW")
#'
#'  # you can also get a more detailed output, containing detailed predictions
#'  # and the parameters of the used DRC curves:
#'  detailed_output <- pheno_phase_prediction(
#'             phase_covariate_list = phase_covariate_list,
#'             environmental_data = environmental_data,
#'             phase_starting_date =as.Date("2021-01-01"),
#'             crop_abbrev = "WW",
#'             output_type = "detailed_information")
#'
#'  #  this output can be visualised like:
#'  # get overview plot of the prediction
#'  DyMEP_prediction_visualizer(detailed_output)
#'  # check the DRC curves of the used model
#'  DyMEP_DRC_visualizer(detailed_output)
#'

NULL
