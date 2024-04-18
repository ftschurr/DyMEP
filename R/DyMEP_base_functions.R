# Author: Flavian Tschurr
# Project: KP030
# Date: 06.04.2024
# Purpose: DyMEP: standard base functions
################################################################################




#' Summary method for DyMEP class
#'
#' This function summarizes DyMEP objects.
#'
#' @param object An object of class "DyMEP".
#' @param ... Additional arguments to be passed to summary functions.
#' @keywords internal
#' @return print directly into console
#' @export
summary <- function(object, ...) {
  UseMethod("summary", object)
}


#' @export
summary.DyMEP <- function(object, ...) {
  detailed_output <- object
  cat("Phenology for the stages: ",
      paste(names(detailed_output$detailled_output), collapse = ", "),
      " was predicted as follows:\n")
  print(detailed_output$dates_df)

  phase_duration <- data.frame(phase = NA, duration = NA)
  covaraites_per_phase <- data.frame(phase = NA, covariates = NA)
  counter <- 1
  for (phase in names(detailed_output$detailled_output)) {
    phase_duration[counter, ] <- c(phase, length(which(
      detailed_output$detailled_output[[
        phase]]$DRC_and_phase_prediction$stage_prediction == 0)))
    covaraites_per_phase[counter, ] <- c(phase, paste(names(
    detailed_output$detailled_output[[phase]]$DRC_parameters), collapse = ", "))
    counter <- counter + 1
  }
  cat("The phenology phases had the following duration:\n")
  print(phase_duration)
  cat("For the prediction, following environmental covariates have been used:")
  print(covaraites_per_phase)
}
