# Author: Flavian Tschurr
# Project: KP030
# Date: 19.02.2024
# Purpose: DyMEP: apply to unknown data helper functions
################################################################################

#' DyMEP_prediction_visualizer
#'
#' Visualizes the predictions of the DyMEP model.
#'
#' @param detailed_output Output of the pheno_phase_prediction function with
#'                        output_type = "detailed_information".
#'
#' @return A plot with one panel per phenology phase, showing the environmental
#'         covariate responses, the GLM prediction, and the phase prediction
#'         (points).
#'
#' @keywords visualization
#' @field timestamp Description of timestamp column.
#'
#' @importFrom graphics abline
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics par
#' @export
#' @examples
#' phase_covariate_list <- best_DyMEP_model(env_covariates =
#'              c("tas","tasmin","VPD","SPI","global_radiation","tasmax","RH"),
#'            pheno_phases = c("sowing-emergence","emergence-jointing",
#'            "jointing-heading"),
#'                                         crop_abbrev = "WW",
#'                                         output_list_for_prediction = TRUE)
#'
#' # Create dummy environmental data
#' environmental_data <- data.frame("DATE" =
#'                                        seq.Date(from = as.Date("2021-01-01"),
#'                                          to = as.Date("2023-12-31"), by = 1),
#'                       "tas" = runif(1095, min = -10, max = 40),
#'                        "RH" = runif(1095, min = 0, max = 100),
#'                        "tasmin" = runif(1095, min = -10, max = 40),
#'                        "tasmax" = runif(1095, min = 0, max = 40),
#'                        "VPD" = runif(1095, min = 0, max = 40),
#'                        "SPI" = runif(1095, min = -1, max = 4),
#'                        "global_radiation" = runif(1095, min = 0, max = 10000))
#'
#' DyMEP_prediction_visualizer(detailed_output = pheno_phase_prediction(
#'                                  phase_covariate_list = phase_covariate_list,
#'                                  environmental_data = environmental_data,
#'                                  phase_starting_date = as.Date("2021-01-01"),
#'                                  crop_abbrev = "WW",
#'                                  output_type = "detailed_information"))
#'
DyMEP_prediction_visualizer <- function(detailed_output){

  comment_out <- try(attributes(detailed_output)$comment)
  if(typeof(comment_out)=="NULL"){
    stop("please provide the direct output of the pheno_phase_prediction
         function withoutput_type = 'detailed_information', thank you!")
  }
  if(comment_out != "created_by_DyMEP"){

    stop("please provide the direct output of the pheno_phase_prediction
         function withoutput_type = 'detailed_information', thank you!")
  }
  timestamp <- NA

  available_env_coviariates <- available_environmental_covariates()

  env_colors <- c("#0173b2", "#de8f05", "#029e73", "#d55e00", "#cc78bc", "#ca9161", "#fbafe4")
  names(env_colors) <- available_env_coviariates$env_covariate
  # visualize the predictions DRC curves
  # set par to match the number of phenology phases
  par(mfrow=c(length(names(detailed_output$detailled_output)),1))
  par(mar = c(5, 6, 4, 2) + 0.1)
  for (phase in names(detailed_output$detailled_output)) {
    data <- detailed_output$detailled_output[[phase]]$DRC_and_phase_prediction
    data <- subset(data, `timestamp` <=
                     as.Date(detailed_output$detailled_output[[phase]]$end_date)+10 )

    env_covariates <- names(data)[names(data) %in%
                                    available_env_coviariates$env_covariate]

    max_val <- max(unlist(data[sapply(data, is.numeric)]), na.rm = TRUE)
    min_val <- min(unlist(data[sapply(data, is.numeric)]), na.rm = TRUE)

    plot(data$timestamp, data$stage_prediction*max_val,
         ylim = c(floor(min_val),ceiling(max_val)),
         ylab = paste(phase,"response",sep="\n"),
         xlab="Time",
         col= "gray25",
         pch = 19)
    # add env variables
    for (env in env_covariates) {
      lines(data$timestamp, data[[env]], col =
              env_colors[which(names(env_colors)==env)])
    }
    # add glm
    lines(data$timestamp, data$prediction*max_val , lwd= 2, col="#004080")
    abline(v=as.Date(detailed_output$detailled_output[[phase]]$end_date))

    legend("topleft",
           legend = c(env_covariates, "glm prediction", "phase prediction"),
           col = c(env_colors[names(env_colors) %in% env_covariates],
                   "#004080", "gray25"),
           pch = c(rep(1, length(env_covariates)), 1, 16),
           lty = c(rep(1, length(env_covariates)), 1, 0),
           lwd = 2)



  }

}


#' DyMEP_DRC_visualizer
#'
#' Visualizes the Dose-Response Curves (DRC) for each phenological phase and
#' environmental covariate.
#'
#' @param detailed_output Output of the pheno_phase_prediction function with
#'                        output_type = "detailed_information".
#'
#' @return Returns plots showing the DRC curves for each phenological phase and
#'         environmental covariate. Each row represents a phenology phase.
#'
#' @keywords visualization
#'
#' @export
#'
#' @importFrom graphics legend
#' @importFrom graphics par
#' @examples
#' phase_covariate_list <- best_DyMEP_model(env_covariates =
#' c("tas","tasmin","VPD","SPI","global_radiation","tasmax","RH"),
#' pheno_phases = c("sowing-emergence","emergence-jointing","jointing-heading"),
#' crop_abbrev = "WW",
#' output_list_for_prediction = TRUE)
#' # create dummy environmental data
#' environmental_data<- data.frame("DATE"=seq.Date(from = as.Date("2021-01-01"),
#'                    to = as.Date("2023-12-31"),by=1),
#'                                 "tas"=runif(1095,min=-10,max=40),
#'                                 "RH"=runif(1095,min=0,max=100),
#'                                 "tasmin"=runif(1095,min=-10,max=40),
#'                                 "tasmax"=runif(1095,min=0,max=40),
#'                                 "VPD" = runif(1095,min=0,max=40),
#'                                 "SPI"= runif(1095,min=-1,max=4),
#'                              "global_radiation"= runif(1095,min=0,max=10000))
#'
#' DyMEP_DRC_visualizer(detailed_output = pheno_phase_prediction(
#' phase_covariate_list = phase_covariate_list,
#' environmental_data = environmental_data,
#' phase_starting_date =as.Date("2021-01-01"),
#'crop_abbrev = "WW",
#' output_type = "detailed_information")
#' )
DyMEP_DRC_visualizer <- function(detailed_output){

    comment_out <- try(attributes(detailed_output)$comment)

  if(typeof(comment_out)=="NULL"){
    stop("please provide the direct output of the pheno_phase_prediction
         function withoutput_type = 'detailed_information', thank you!")
  }
  if(comment_out != "created_by_DyMEP"){

    stop("please provide the direct output of the pheno_phase_prediction
         function withoutput_type = 'detailed_information', thank you!")
  }


  available_env_coviariates <- available_environmental_covariates()

  env_colors <- c("#0173b2", "#de8f05", "#029e73", "#d55e00", "#cc78bc", "#ca9161", "#fbafe4")
  names(env_colors) <- available_env_coviariates$env_covariate

  # create one line per phase and one col per covariate
  # length(names(detailed_output$detailled_output))
  covariate_counter <- NULL
  i=1
  for(phase in names(detailed_output$detailled_output)){
    covariate_counter[i] <- length(names(detailed_output$detailled_output[[phase]]$DRC_parameters))
    i= i+1
  }

  par(mfrow=c(length(names(detailed_output$detailled_output)),max(covariate_counter)))
  title_counter <- 1
  phase_counter <- 1
  for (phase in names(detailed_output$detailled_output)) {
    env_counter <- 1
    for(env_variable in names(detailed_output$detailled_output[[phase]]$DRC_parameters)){
      DRC_name <- detailed_output$detailled_output[[phase]]$DRC_parameters[[env_variable]]$DRC_name
      params <- detailed_output$detailled_output[[phase]]$DRC_parameters[[env_variable]]$parameters

      .response_function. <- get(paste0(DRC_name,"_prediction"))

      if(env_variable == "RH"){
        data = seq(from=0, to= 100, by=0.01)
      }

      if(env_variable %in% c("tas","tasmax","tasmin")){
        data = seq(from=-15, to= 40, by=0.01)
      }


      if(env_variable == "global_radiation"){
        data = seq(from=0, to= 3500, by=0.1)
      }

      if(env_variable == "SPI"){
        data = seq(from=-6, to= 6, by=0.001)
      }

      if(env_variable == "VPD"){
        data = seq(from=-3, to= 35, by=0.01)
      }


      response <- unlist(lapply(
        data,
        .response_function., params))

      if(title_counter == 1){
        plot(data, response,
             xlab= env_variable,
             ylab= paste(DRC_name,"response",sep=" "),
             type="l",
             col = env_colors[which(names(env_colors)==env_variable)],
             main = phase)
        title_counter <- 2

      }else{

        plot(data, response,
             xlab= env_variable,
             ylab= paste(DRC_name,"response",sep=" "),
             type="l",
             col = env_colors[which(names(env_colors)==env_variable)])
      }
      # fill with empty plots the line
      if(env_counter == length(names(detailed_output$detailled_output[[phase]]$DRC_parameters))){
        for( empty_plot in rep(1,diff(c(env_counter,max(covariate_counter))))){
          plot(1, type = "n", axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "")
        }
      }

      phase_counter <- phase_counter + 1
      env_counter <- env_counter +1
    }
    title_counter <- 1
  }

}

