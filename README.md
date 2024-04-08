
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DyMEP

<!-- badges: start -->
<!-- badges: end -->

DyMEP a Dynamic Multi-Environment Phenology Model. This phenology model
allows to model crop phenology according to environmental covariates.
One can chose, or ask the model, for the best combination of
environmental covariates for a specific crop phenology macro-phase. The
model was trained using a large dataset of crop phenology derived from
the German weather service (DWD): <https://opendata.dwd.de>. more
details can be found here: DOIs

Available Crops are: -Winter wheat -Winter Rye -Winter Barley -Spring
Wheat -Spring Barley -Rapeseed -Oat -Maize -Green Peas -Green Beans

used covariates (daily resolution): - minimal temperature - maximal
temperature - mean temperature - SPI (30-days standardized precipitation
index) - relative humidity - global radiation - VPD

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(DyMEP)
## basic example code
library(DyMEP)
## basic example code
# check what exists
available_phases <- available_crops_and_phases()
# check what covairates are implemented in the model
available_covariates <- available_environmental_covariates()


#what is the best environmental covariates for one or multiple phases?

best_DyMEP_model(env_covariates = c("tas","tasmin","VPD","SPI",
                                    "global_radiation","tasmax","RH"),
                 pheno_phases = c("sowing-emergence","jointing-heading"),
                        crop_abbrev = "WB")


phase_covariate_list <- best_DyMEP_model(env_covariates = c("tas","tasmin",
                                "VPD","SPI","global_radiation","tasmax","RH"),
                 pheno_phases = c("sowing-emergence","emergence-jointing",
                                  "jointing-heading"),
                 crop_abbrev = "WB",
                 output_list_for_prediction = T)

#create a list of wanted phases and corresponding environmental covariates
# we chose the suggested models, but we add the phase emergence-booting as well.
# (Attention, the model will not run if you provide gaps in the phenology!
#it is no problem to run a subset of phases but they need to be consecutive!)
phase_covariate_list <- list("sowing-emergence" = c("tasmin","VPD","SPI"),
                        "emergence-jointing"= c("tas","tasmin","VPD","SPI"),
                     "jointing-heading" = c("global_radiation","tas","SPI"))

#create dummy environmental data
environmental_data <- data.frame("DATE"=seq.Date(from = as.Date("2021-01-01"),
                                              to = as.Date("2022-12-31"),by=1),
                             "tas"=runif(730,min=-10,max=40),
                             "RH"=runif(730,min=0,max=100),
                             "tasmin"=runif(730,min=-10,max=40),
                             "tasmax"=runif(730,min=-10,max=40),
                             "VPD" = runif(730,min=-10,max=40),
                             "SPI"= runif(730,min=-1,max=4),
                             "global_radiation"= runif(730,min=0,max=10000))
# predict phenology, we that the crop was sown at the "2022-01-01" 

pheno_phase_prediction(phase_covariate_list = phase_covariate_list,
                       environmental_data = environmental_data,
                       phase_starting_date =as.Date("2022-01-01"),
                       crop_abbrev = "WR")


detailed_output <- pheno_phase_prediction(phase_covariate_list =
                                            phase_covariate_list,
                       environmental_data = environmental_data,
                       phase_starting_date =as.Date("2022-01-01"),
                       crop_abbrev = "WR",
                       output_type = "detailed_information")


# get overview plot of the prediction
DyMEP_prediction_visualizer(detailed_output)
# check the DRC curves of the used model
DyMEP_DRC_visualizer(detailed_output)

# get summary
summary(detailed_output)



# output are the end dates of the stages (attention in the example
# we use random generated dummy data, the resulting dates are therefore 
# not extremely meaningful)
```
