# Author: Flavian Tschurr
# Project: KP030
# Date: 08.03.2022
# Purpose: DyMEP: set parameters
################################################################################





meta_data_path <- "meta/phenological_phases"

################################################################################
################################################################################
# set parameters and constants
################################################################################
################################################################################

pheno_phases <- c("sowing-emergence","emergence-booting",  "booting-heading",
                  "heading-senescence")


# environmental covariates
env_variable_DRCs_inputs <- list("tas" = c("WangEngels"),
                                 "tasmin" = c("WangEngels"),
                                 "tasmax" = c("WangEngels"),
                                 "RH" = c("reg_linear","non_linear",
                                          "asymptotic","WangEngels"),
                                 "global_radiation" =  c("WangEngels"),
                                 "SPI"=c("reg_linear","non_linear",
                                         "asymptotic","WangEngels"),
                                 "VPD" =  c("reg_linear","non_linear",
                                            "asymptotic","WangEngels") )


env_variables <- names(env_variable_DRCs_inputs)

granularity <- "daily"
crop_abbrev <- "WW"

# name of the model_run
model_run <- "run_WW1"

################################################################################
# set colors for plots


pheno_phase_colors_vect <- c("sowing-emergence" =  "#A66969",
                             "emergence-booting"= "#3D5BA3",
                             "booting-heading"  ="#49662A",
                             "heading-senescence" = "#DC482B")

DRC_colors_vect <- c("WangEngels" = "#003f5c",
                          "asymptotic" = "#7a5195",
                          "reg_linear" = "#ef5675",
                          "non_linear" = "#ffa600")

DRC_name_vect <- c("WangEngels" = "Wang Engels",
                     "asymptotic" = "asymptotic",
                     "reg_linear" = "linear",
                     "non_linear" = "non linear")

