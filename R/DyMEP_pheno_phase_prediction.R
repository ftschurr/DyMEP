# Author: Flavian Tschurr
# Project: KP030
# Date: 16.03.2023
# Purpose: DyMEP: predict phases
################################################################################

#' function to predict the a phenological phase in winter wheat
#'
#' predict one or all phenological phases
#'@param phase_covariate_list list like: list("sowing-emergence" =
#'c("tas","VPD","SPI"),
#' "emergence-jointing" = c("tas"))
#' indicating per phenological phase the covariates to use. List of
#' of phenological phases must be consecutive!
#'
#'@param phase_starting_date starting date of the first phase which will be
#'predicted (object of class "Date" (use as.Date()))
#'@param environmental_data data.frame with the necessary environmental data,
#' one column must be "DATE" (as.Date format), the others with the names of
#' the environmental covariates (e.g. tas, tasmin etc.)
#'@param crop_abbrev abbreviation of the crop to be modeled (valid crop_abbrevs
#' can be found with available_crops_and_phases())
#' @param output_type either "dates" or "detailed_information"; defines what
#' output of the model they user wants to have as return,
#' default is set to "dates". If a user wants to get the response parameters,
#' curves, predictions and model thresholds, it should be chosen
#' "detailed_information"
#' output = "dates" will return a dataframe with the stages and according dates
#' output = "detailed_information" will return a list with the dates, but also
#'  the corresponding dose response parameters and predictions
#'@return returns the end-date of each phase
#'@keywords phenology phase prediction
#'@importFrom stats na.omit
#' @export
#' @examples
#' pheno_phase_prediction(phase_covariate_list = list(
#'         "sowing-emergence" = c("tasmin","VPD","SPI"),
#'         "emergence-jointing"= c("tas","tasmin","VPD","SPI"),
#'         "jointing-heading" = c("global_radiation","tas","SPI")),
#'       environmental_data= data.frame(
#'           "DATE"=seq.Date(from = as.Date("2021-01-01"),
#'                           to = as.Date("2022-12-31"),by=1),
#'           "tas"=runif(730,min=-10,max=40),
#'           "tasmin"=runif(730,min=-10,max=40),
#'           "VPD" = runif(730,min=-10,max=40),
#'           "SPI"= runif(730,min=-1,max=4),
#'           "global_radiation"= runif(730,min=0,max=10000)),
#'       phase_starting_date =as.Date("2022-01-01"),
#'       crop_abbrev = "WR")
pheno_phase_prediction <-  function(phase_covariate_list,
                                    environmental_data,
                                    phase_starting_date,
                                    crop_abbrev,
                                    output_type = "dates"){

  # check if all the phases are written correctly
  phase_covariate_list <- predhelp.check_pheno_phase_order(phase_covariate_list,
                                                           crop_abbrev)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # check the user inputs
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  if(length(crop_abbrev)!=1){
    stop("please select one crop at a time.")
  }

  if(!(output_type %in% c("dates","detailed_information"))){

    stop("please select as output_type either 'dates' or 'detailed_information' and try again.")

  }
  if(!any(names(environmental_data) %in% c("DATE"))){
    stop("please name the date column in the environmental_data
         data.frame 'DATE'")
  }

  if(length(environmental_data$DATE) > length(na.omit(environmental_data)$DATE)){
    stop("your environmental_data dataframe contains NA values,
         please fill the gaps or clean the input data")
  }

  for(vari in as.character(unlist(phase_covariate_list))){
    if(!any(names(environmental_data) %in% c(vari))){
      stop(paste0("please add a column to the environmental_data dataframe with
                 ", vari," data, respectively use the
                 available_environmental_covariates() to backcheck
                 availability in this package for the chosen
                 environmental covariate"))

    }
  }

  if(is.na(as.Date(phase_starting_date))){
    stop("your phase_starting_date does not seem to be a date object,
         please provide a date (as.Date())")
  }

  if(length(environmental_data$DATE) < 50 ){
    warning(paste0("your environmental_data input contains just ",
              length(environmental_data$DATE),". Please make sure, this
              is enough!"))
  }
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # restructure environmental covariates
  env_data <- envpredutils.data_frame_to_list(environmental_data)



  start_date <- phase_starting_date
  stages_list <- list()
  output_detailed_list <- list()
  for(pheno_phase in names(phase_covariate_list)){

    # find correct model
    model_name <- envpredutils.model_selecter(crop_abbrev,
                                              pheno_phase,
                                            phase_covariate_list[[pheno_phase]])

    # load correct model
    model <- readRDS(system.file("extdata","pmem",crop_abbrev,
                                 model_name, package = "DyMEP"))

    # cut period
    env_period <- envpredutils.env_period_cutter(start_date = start_date,
                            env_variables = phase_covariate_list[[pheno_phase]],
                            env_data=env_data,
                            max_period_length= 250)




    # apply
    if(output_type == "dates"){

      end_phase <- envpredutils.pheno_phase_prediction_glm_model(
                                              env_data_pheno_phase = env_period,
                                              pheno_phase = pheno_phase,
                                              crop_abbrev = crop_abbrev,
                                              model = model,
                                              output_type = output_type)
    }else{

      output_detailed_list[[pheno_phase]] <- envpredutils.pheno_phase_prediction_glm_model(
                      env_data_pheno_phase = env_period,
                      pheno_phase = pheno_phase,
                      crop_abbrev = crop_abbrev,
                      model = model,
                      output_type = output_type)

      end_phase <- output_detailed_list[[pheno_phase]]$end_date

    }
    # safe dates
    stages_list[[pheno_phase]]<- data.frame("stage" = unlist(
      strsplit(pheno_phase,split="-"))[2],
               "date" = end_phase)

    start_date <- end_phase


  }
  output <- do.call("rbind",stages_list)
  output$date <- as.Date(output$date)

  if(output_type == "dates"){
    return(output)
  }else{
    whole_output <- list("dates_df" = output,
                         "detailled_output" = output_detailed_list)

    comment(whole_output) <- "created_by_DyMEP"

    class(whole_output) <- "DyMEP"

    return(whole_output)



  }

}
