# Author: Flavian Tschurr
# Project: KP030
# Date: 22.03.2022
# Purpose:DyMEP: response functions and fitting
################################################################################

################################################################################
# response function linear
################################################################################

#' DRC function: reg_linear_prediction
#'
#'@param env_variate value of a environmental covariate
#'@param params list of input parameter; intercept estimated value,
#'slope of the linear phase
#'@description linear model according to an env variable
#'@keywords DRC
#' @return value with reg_linear response
#'@export
#'@examples
#'reg_linear_prediction(1,list("intercept_value"=1, "slope_value"=5))
#'# visualization
#'reg_linear <- lapply(seq(0, 10, 0.1),
#'reg_linear_prediction,
#'list("intercept_value"=-1,"slope_value"=1))
#'plot(seq(0, 10, 0.1), reg_linear)
reg_linear_prediction <- function(env_variate,params){
  intercept_value <- as.numeric(params[which(names(params)=="intercept_value")])
  slope <- as.numeric(params[which(names(params)=="slope_value")])

  y <- intercept_value + env_variate*slope
  return(y)
}




################################################################################
# response function non_linear
################################################################################

#' DRC function: non_linear_prediction
#'
#'@param env_variate value of a environmental covariate
#'@param params list of input parameter; base_value: minimal value;
#' slope estimated value, slope of the linear phase
#'@description broken stick model according to an env variable
#'@keywords DRC
#' @return value with non_linear response
#'@export
#'@examples
#'non_linear_prediction(1,list("base_value"=5,"slope_value"=1))
#'
#'# visualization
#'non_linear <- lapply(seq(0, 10, 0.1),
#'non_linear_prediction,
#'list("base_value"=5,"slope_value"=1))
#'plot(seq(0, 10, 0.1), non_linear)

non_linear_prediction <- function(env_variate,params){
  base_value <- as.numeric(params[which(names(params)=="base_value")])
  slope <- as.numeric(params[which(names(params)=="slope_value")])

    y <- (env_variate-base_value)*slope
  y <- ifelse(env_variate>base_value,y,0)
  return(y)
}



################################################################################
# asymptotic function
################################################################################


#' DRC function: asymptotic_prediction
#'
#'@param x input variable
#'@param params list of input parameter;
#'Asym  a numeric parameter representing the horizontal asymptote
#' on the right side (very large values of input). ;
#' lrc 	a numeric parameter representing the natural
#'  logarithm of the rate constant;
#'  c0  	a numeric parameter representing the x for which the response is zero.
#'@keywords DRC
#' @return value with asymptotic response
#'@importFrom stats SSasympOff
#'@export
#'@examples
#'asymptotic_prediction(5,list("Asym_value"=0.5,
#'                     "lrc_value"=0.2,
#'                       "c0_value"=4))
#' # visualization
#'asymptote <- lapply(seq(0, 10, 0.1),
#' asymptotic_prediction,
#' list("Asym_value"=0.5, "lrc_value"=0.2, "c0_value"=4))
#' plot(seq(0, 10, 0.1), asymptote)

asymptotic_prediction <- function(x, params){
  Asym <- as.numeric(params[which(names(params)=="Asym_value")])
  lrc <- as.numeric(params[which(names(params)=="lrc_value")])
  c0 <- as.numeric(params[which(names(params)=="c0_value")])

  y <- SSasympOff(x , Asym, lrc, c0)
  y <- ifelse(y > 0, y,0) # no negative growth
  return(y)
}

################################################################################
# Wang Engels function
################################################################################

#' DRC function: WangEngels_prediction
#'
#'@param x effective env_variable value
#'@param params list of input parameter; xmin_value represents the minimal
#'  env_variable value above which growth response will happen ;
#'  xopt_value: optimal growth point, env_variable values here have the highest
#'   response;
#'  xmax_value represents the maximal env_variable value above which no growth
#'  response will happen according to the wang engel model.
#'@keywords DRC
#' @return value with WangEngels response
#'@export
#'@examples
#'WangEngels_prediction(10, params = list("xmin_value"=1,
#'                                        "xopt_value"=25,
#'                                        "xmax_value"=35,
#'                                         "r_value"=0.5))
#'  # visualization
#'WangEngels <- lapply(seq(0, 40, 0.1),
#' WangEngels_prediction,
#' list("xmin_value"=1,
#'      "xopt_value"=25,
#'       "xmax_value"=35,
#'        "r_value"=0.5))
#'        plot(seq(0, 40, 0.1), WangEngels)
WangEngels_prediction <- function(x, params){
  xmin <- as.numeric(params[which(names(params)=="xmin_value")])
  xopt <- as.numeric(params[which(names(params)=="xopt_value")])
  xmax <- as.numeric(params[which(names(params)=="xmax_value")])
  r <- as.numeric(params[which(names(params)=="r_value")])


  alpha <- log(2)/(log((xmax-xmin)/(xopt-xmin)))

  if(xmin < x & x < xmax){

    y <- r*(2*(x-xmin)^alpha*(xopt-xmin)^alpha-(x-xmin)^(2*alpha))/
      ((xopt-xmin)^(2*alpha))

  }else{
    y <- 0
  }

  return(y)

}


