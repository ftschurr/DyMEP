# Author: Flavian Tschurr
# Project: KP030
# Date: 02.10.2023
# Purpose: get best model for your data
################################################################################

#' A function to get the best model with the environmental covariates at hand
#'
#' @param env_covariates the environmental
#' covariates you have at hand; inputs as character strings naming the available
#'   environmental covariate
#' @param pheno_phases the phenological phases you are interested in. Input
#' as character string, available phenological phases do depend on the chosen
#' crop (use available_crops_and_phases())
#' @param crop_abbrev the abbreviation of the wanted crop
#' @param skillscore_to_select the skillscore according to which you want to
#'  select (Default = "RMSE); available options: "RMSE", "MAE", "cor", "AIC" and
#'  "SumLogLikelihood"
#' @param return_just_best decide whether you want to get a list of all
#' potential options back, or just the best (default = TRUE)
#' @param output_list_for_prediction boolean (TRUE/FALSE), if TRUE the output
#' of this function is a list which can directly be used for the
#' pheno_phase_prediction function as input (phase_covariate_list). Select
#' return_just_best also as TRUE if this output is wished
#' @keywords internal
#' @importFrom utils read.csv
#' @export
#' @examples
#' best_DyMEP_model(env_covariates =  c("tas","VPD"),
#'       pheno_phases = c("sowing-emergence"),
#'       crop_abbrev = "GB")

best_DyMEP_model  <- function(env_covariates,
                                    pheno_phases,
                                    crop_abbrev,
                                    skillscore_to_select = "RMSE",
                                    return_just_best=TRUE,
                              output_list_for_prediction= FALSE){
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # check the user inputs
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(length(crop_abbrev)!=1){
    stop("please select one crop at a time.")
  }



  # Load data from an .rds file in the package
  all_available_crops_and_pheno_phases <- read.csv(
    system.file("extdata","meta","crop_pheno_phase_description_DWD.csv",
                package = "DyMEP"))


  if(length(which(unique(all_available_crops_and_pheno_phases$abbrev)== crop_abbrev))<1){
    stop(paste0("your crop: ",crop_abbrev," is not available in this model
                 please select one of the following: ",
                paste(unique(all_available_crops_and_pheno_phases$abbrev),
                      collapse=" ")," ; for futher
                information check the available_crops_and_phases()
                function." ))
  }


  phases_crop <- subset(all_available_crops_and_pheno_phases, abbrev ==
                          crop_abbrev)

  if(length(!which(phases_crop$pheno_phase %in% pheno_phases)) != length(pheno_phases)){
    stop(paste0("One or multiple of your input pheno phase are not available.
                You entered: ", paste(pheno_phases,collapse=" "),
                " the available list is: " ,paste(
                  phases_crop$pheno_phase,collapse=" ")," ; for futher
                information check the available_crops_and_phases()
                function."))
  }


  if(!(skillscore_to_select %in% c("cor","RMSE","MAE","AIC","SumLogLikelihood"))){
    stop("you selected a not available skillscore, the default is 'RMSE',
         available are following other skill scores:
         cor,MAE,AIC,SumLogLikelihood. Please select one of them!")
  }
  if(typeof(return_just_best) != "logical"){
    stop("return_just_best must be boolean, please provide TRUE/FALSE as input")
  }

  all_available_envs <- read.csv(
    system.file("extdata","meta",
                "env_covariate_information.csv",
                package = "DyMEP"))


  if(length(!which(all_available_envs$env_covariate %in% env_covariates)) != length(env_covariates)){
    stop(paste0("One or multiple of your input environmental covaraites are not
                available. You entered: ", paste(env_covariates,collapse=" "),
                " the available list is: " ,paste(
                  all_available_envs$env_covariate,collapse=" "), " ; for futher
                information check the available_environmental_covariates()
                function."))
  }

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  # read data
  all_skillscores <- read.csv(system.file("extdata","validation",crop_abbrev,
                                          "all_models_selected_with_RMSE.csv",
                                          package = "DyMEP"))


  # get all potential combinations to select in the validation data.frame
your_environmental_covariates_permutations <-predhelputils.get_all_permutations(
    your_environmental_covariates = env_covariates)
  all_available_models <- list()
  best_available_model <- list()

  for(pheno_ph in pheno_phases){
    one_pheno_skillscores <- subset(all_skillscores, pheno_phase == pheno_ph)
    all_available_models[[pheno_ph]] <- one_pheno_skillscores[which(
      one_pheno_skillscores$env_variables %in%
        your_environmental_covariates_permutations),]

    if(skillscore_to_select == "cor"){
      best_available_model[[pheno_ph]] <- all_available_models[[pheno_ph]][
        which.max(all_available_models[[pheno_ph]][[skillscore_to_select]]),]

    }else{
      best_available_model[[pheno_ph]] <- all_available_models[[pheno_ph]][
        which.min(all_available_models[[pheno_ph]][[skillscore_to_select]]),]
    }
  }


  if(output_list_for_prediction==TRUE){
    if(return_just_best == FALSE){
      stop("if output_list_for_prediction = TRUE, please set as return_just_best
           = TRUE")
    }


    prediction_list <- list()
    for(pheno_ph in pheno_phases){
      env_covairiates_to_put <-  gsub("global_radiation","GR"
                                ,best_available_model[[pheno_ph]]$env_variables)
      env_covairiates_to_put <- unlist(strsplit(env_covairiates_to_put, "_"))

      env_covairiates_to_put <- gsub("GR", "global_radiation"
                                     ,env_covairiates_to_put)

      prediction_list[[pheno_ph]] <- env_covairiates_to_put
    }

    # check if all the phases are written correctly
    phase_covariate_list <- predhelp.check_pheno_phase_order(prediction_list,
                                                             crop_abbrev)

    return(prediction_list)
  }

  if(return_just_best ==TRUE){
    output_df <- do.call("rbind",best_available_model)
  }else{
    output_df <- do.call("rbind",all_available_models)

  }

  output_df$env_variables <- gsub("_",",",output_df$env_variables)
  output_df$env_variables <- gsub("global,radiation","global_radiation",output_df$env_variables)

  return(output_df)

}



#' available_crops_and_phases
#'
#'  check what crops and corresponding phenology phases are available at the
#'  moment. Chose a crop (crop_abbrev), phenological phase from the output of
#'  this function further usage
#'
#' @keywords check what crops and corresponding phenology phases are available
#' @export
#' @importFrom utils read.csv
#' @examples
#' available_crops_and_phases()

available_crops_and_phases <- function(){
  # Load data from an .rds file in the package
  all_available_crops_and_pheno_phases <- read.csv(
    system.file("extdata","meta",
                "crop_pheno_phase_description_package_display.csv",
                package = "DyMEP"))


  # print(all_available_crops_and_pheno_phases)

  return(all_available_crops_and_pheno_phases)

}



#' available_environmental_covariates
#' check what environmental covariates are implemented, use or alter prediction
#'   these abbreviations and the corresponding unit
#'
#' @keywords check what environmental covariates are implemented
#' @export
#' @importFrom utils read.csv
#' @examples
#' available_environmental_covariates()

available_environmental_covariates <- function(){
  # Load data from an .rds file in the package
  all_available_envs <- read.csv(
    system.file("extdata","meta",
                "env_covariate_information.csv",
                package = "DyMEP"))


  return(all_available_envs)

}

#' predhelp.check_pheno_phase_order
#'
#' @param phase_covariate_list list with all available phenology phases
#' @param crop_abbrev current crop_abbrev (name of the crop)
#' @keywords internal
#' @importFrom utils read.csv
#' @export
#' @examples
#' predhelp.check_pheno_phase_order(phase_covariate_list = list(
#'   "sowing-emergence" = c("tasmin","VPD","SPI")),
#'   crop_abbrev = "GB")

predhelp.check_pheno_phase_order <- function(phase_covariate_list,crop_abbrev){
  pheno_phases <- names(phase_covariate_list)

  # Load data from an .rds file in the package
  all_available_crops_and_pheno_phases <- read.csv(
    system.file("extdata","meta","crop_pheno_phase_description_DWD.csv",
                package = "DyMEP"))
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # check the user inputs
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  if(length(which(unique(all_available_crops_and_pheno_phases$abbrev)== crop_abbrev))<1){
    stop(paste0("your crop: ",crop_abbrev," is not available in this model
                 please select one of the following: ",
                paste(unique(all_available_crops_and_pheno_phases$abbrev),
                      collapse=" ") ))
  }

  phases_crop <- subset(all_available_crops_and_pheno_phases, abbrev ==
                          crop_abbrev)

  if(length(!which(phases_crop$pheno_phase %in% pheno_phases)) != length(pheno_phases)){
    stop(paste0("One or multiple of you input pheno phase are not available.
                You entered: ", paste(pheno_phases,collapse=" "),
                " the available list is: " ,paste(
                  phases_crop$pheno_phase,collapse=" ")))
  }

  entry_numbers <- which(phases_crop$pheno_phase %in% pheno_phases)

  # check if phases are consecutive
  if(any(diff(entry_numbers) >1)){
    stop(paste0("the input pheno phase are not in a consecutive order!
                please adapt this, otherwise the start of the
                next phenological phase is not estimated. you entered: ",
                paste(pheno_phases,collapse=" ")," the available list is: "
                ,paste(phases_crop$pheno_phase, collapse = " ")))
  }

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # check if they are in the correct order
  order_checker <- c()
  counter <- 1
  for(p in pheno_phases){
    order_checker[counter] <- which(phases_crop$pheno_phase == p)
    counter <- counter + 1
  }


  if(any(diff(order_checker) >1)){
    phase_covariate_list_corrects <- list()
    for(p in phases_crop$pheno_phase){
      if(p %in% pheno_phases){
        phase_covariate_list_corrects[[p]] <- phase_covariate_list[[p]]
      }
    }
    phase_covariate_list <- phase_covariate_list_corrects
  }


  return(phase_covariate_list)
  }

