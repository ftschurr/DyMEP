# Author: Flavian Tschurr
# Project: KP030
# Date: 16.03.2023
# Purpose: DyMEP: apply to unknown data helper functions
################################################################################

#' A function to create a date vector
#'
#' @param start_date start of the phase
#' @param period_length length of the period
#' @keywords internal
#' @export
#' @examples
#' envpredutils.period_date_creator(as.Date("2024-01-15"),100)
envpredutils.period_date_creator <- function(start_date, period_length){
  return(seq(as.Date(start_date), (as.Date(start_date)+period_length),
             by = "days"))
}

#' A function to cut the environmental data into the correct length
#'
#' @param start_date start of the phase
#' @param env_variables name of the wanted variables
#' @param env_data actual environmental data
#' @param  max_period_length in order to reduce computing time, a maximal
#'  amount of days will be considered (default = 300); if less data available,
#'  less days will be considered
#' @keywords internal
#' @export
#' @examples
#' envpredutils.env_period_cutter(Sys.Date(),
#' c("tas"), list("tas"=data.frame("DATE"= seq(as.Date(Sys.Date()),
#'  (Sys.Date()+100),by="day"),
#'   "VALUE"= c(1:101))), 50)
envpredutils.env_period_cutter <- function(start_date, env_variables,
                                           env_data,
                                           max_period_length= 300){
  out_list <- list()
  out_list[["timestamps"]] <-envpredutils.period_date_creator(start_date,
                                                              max_period_length)
  for(env_vari in env_variables){
    timestamp_vect <- as.Date(env_data[[env_vari]]$DATE)

    if(length(env_data[[env_vari]]$VALUE[which(timestamp_vect >=
                                               as.Date(start_date))]) >
       max_period_length){

      out_list[[env_vari]] <- env_data[[env_vari]]$VALUE[which(
        timestamp_vect >= as.Date(start_date) &
          timestamp_vect <= (as.Date(start_date) + max_period_length))]

    }else{
      out_list[[env_vari]] <- env_data[[env_vari]]$VALUE[which(
        timestamp_vect >= as.Date(start_date))]

      out_list[["timestamps"]] <- envpredutils.period_date_creator(start_date,
                                            (length(out_list[[env_vari]])-1) )
    }

    if(length( out_list[["timestamps"]]) != length( out_list[[env_vari]] )) {
      rm(out_list)
      out_list <- NA
      next
    }
  }
  return(out_list)

}


#' A function to get the best model with the covariates at hand
#'
#' @param env_data_frame data.frame with the necessary environmental data,
#'  one column must be "DATE" (as.Date format), the others with the names
#'   of the environmental covariates (e.g. tas, tasmin etc.)
#'@keywords internal
#' @export
#' @examples envpredutils.data_frame_to_list(data.frame("tas"=c(1:100),
#'  "VPD"=c(1:100), "DATE"=c(1:100)))


envpredutils.data_frame_to_list <- function(env_data_frame){

  dates <- env_data_frame$DATE

  env_data_noDATE <- env_data_frame[,-which(names(env_data_frame)=="DATE")]

  env_data_list <- list()

  for(env in names(env_data_noDATE)){

    env_data_list[[env]] <- data.frame("DATE"= dates,
                                       "VALUE"= env_data_noDATE[[env]])


  }
  return(env_data_list)
}


#' A function to get the wanted model
#'
#' @param crop_abbrev abbreviation of the crop
#' @param pheno_phase phenological phase
#' @param env_variables vector with the wanted environmental variables
#' @keywords internal
#' @export
#' @examples
#' envpredutils.model_selecter("WW","sowing-emergence",c("tas"))
envpredutils.model_selecter <- function(crop_abbrev,
                                        pheno_phase,
                                        env_variables){

  all_models <- list.files(system.file("extdata","pmem",
                                       crop_abbrev, package = "DyMEP"))

  pheno_phase_short <- paste(unlist(lapply(strsplit(pheno_phase,"-"),
                                           substr,0,1)),collapse="-")


  model_name <- all_models[grep(pheno_phase_short,all_models)]
  model_name <- model_name[grep(length(env_variables),model_name)]

  for(env in env_variables){
    model_name <- model_name[grep(paste0("_",env,"_"),model_name)]
  }
  return(model_name)
}

#' function to calculate cumulative response
#'
#' @param env_data_vector environmental covariate to predict on
#' @param .response_function. dose response function
#' @param parameters parameters of the response function
#' @export
#' @keywords  internal

envpredutils.cumulative_dose_response_pred_helper <- function(env_data_vector,
                                                            .response_function.,
                                                              parameters){
  #'@description wrapper function to apply the response_prediction
  #'function in a "weighted" manner and do some gap filling of the env data

  if(length(which(is.na(env_data_vector)))>=1){
    return(rep(NA, length(env_data_vector)))
  }else{
    return(cumsum(as.numeric(unlist(lapply(env_data_vector,.response_function.
                                           ,parameters)))))
  }

}



#' create DF suitable to predict with GLM
#'
#' @param resposne_predictions prediction output
#' @param timestamp_vect vector with timestamps
#' @export
#' @keywords internal
#' @examples
#' envpredutils.GLM_prediction_df_creator(list("tas"=list("growth_cumulative"=
#'                                     c(1))),timestamp_vect = Sys.Date())
envpredutils.GLM_prediction_df_creator <- function(resposne_predictions,
                                                   timestamp_vect){

  one_df <- matrix(data=NA, ncol = (length(names(resposne_predictions))+1),
                   nrow = length(timestamp_vect))
  counter <- 1
  for(env_vari in names(resposne_predictions)){

    one_df[,counter] <- resposne_predictions[[env_vari]][["growth_cumulative"]]
    counter <- counter +1
  }
  one_df[,counter] <- as.character(timestamp_vect)

  colnames(one_df) <- c(names(resposne_predictions),"timestamp")

  out_df <-as.data.frame(one_df)

  for(r in which(names(out_df) %in% names(resposne_predictions))){
    out_df[,r] <- as.numeric(out_df[,r])
  }
  out_df$timestamp <- as.Date(out_df$timestamp)

  return(out_df)
}




###############################################################################


#' apply the prediction with glm model
#'
#' @param env_data_pheno_phase environmental data required to predict the phase
#' @param pheno_phase phenological phase
#' @param crop_abbrev abbreviation of the crop
#' @param model the selected model to predict the wanted phenological phase
#' @param output_type either "dates" or "detailed_information"; defines what
#' output of the model they user wants to have as return,
#' default is set to "dates". If a user wants to get the response parameters,
#' curves, predictions and model thresholds, it should be chosen
#' "detailed_information"
#' output = "dates" will return a dataframe with the stages and according dates
#' output = "detailed_information" will return a list with the dates, but also
#'  the corresponding dose response parameters and predictions
#' @importFrom stats predict.glm
#' @keywords phase prediction glm
#' @export

envpredutils.pheno_phase_prediction_glm_model <- function( env_data_pheno_phase,
                                                           pheno_phase,
                                                           crop_abbrev,
                                                           model,
                                                           output_type = "dates"){

  env_variables_model <- names(model$coefficients$env_parameters)[-1]
  predictions <- list()
  output_long_params <- list()
  for(env_vari_model in env_variables_model){
    ##########################################################################
    # load .response_functions.
    ###########################################################################
    granularity <-  "daily"

    file_name_best <- paste0(env_vari_model,"_",granularity,"_",pheno_phase,
                             "_best_response_curve_type.rds")
    best_curve <- readRDS(system.file("extdata","pbRC",crop_abbrev,
                                      file_name_best, package = "DyMEP"))


    .response_function. <- get(paste0(best_curve$response_curve_type,
                                      "_prediction"))
    parameters <- best_curve$parameters$curve

    output_long_params[[env_vari_model]] <- list("DRC_name" =
                                              best_curve$response_curve_type,
                                              "parameters" =
                                                best_curve$parameters$curve)

    ###########################################################################
    # subset data
    ##########################################################################

    if(length(which(is.na(env_data_pheno_phase[[env_vari_model]])) ==TRUE) > 1){
      return(NA)
    }else{
      predictions[[
        env_vari_model]][["growth_cumulative"
              ]]<- envpredutils.cumulative_dose_response_pred_helper(
                            env_data_pheno_phase[[env_vari_model]],
                            .response_function., parameters)
    }


  }

  combined_envs <- envpredutils.GLM_prediction_df_creator(
    resposne_predictions = predictions,
    timestamp_vect = env_data_pheno_phase[["timestamps"]])



  # predict, handing in the fitted glm  and using the predict function
  combined_envs$prediction <- predict.glm(model$fitted_glm, combined_envs)
  combined_envs$stage_prediction <- ifelse(
    combined_envs$prediction <=
      as.numeric(model$coefficients$threshold_phase),0,1)

  if(length(which(combined_envs$stage_prediction == 1))==0){
    stop(paste0("please provide a longer time series of environmental covariates,
         the model did not reach the end of the phenological phase with the
         given input! The current phase was: ",pheno_phase))
  }

    # find end date
    end_date <- as.character(combined_envs$timestamp[max(
      which(combined_envs$stage_prediction == 0))])

  if(output_type == "dates"){
    # return end date
    return(end_date)

  }else{
    whole_output <- list("end_date" = end_date,
                         "DRC_and_phase_prediction" = combined_envs,
                         "DRC_parameters" = output_long_params,
                         "phase_threshold" = model$coefficients$threshold_phase)

    return(whole_output)
  }

}
