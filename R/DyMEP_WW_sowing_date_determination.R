################################################################################
# Project: KP00xx
# Sub-Project: Cliamte Suitabuility
# Author: Flavian Tschurr
# Date: 12.01.2021
# Script purpose: Sowing-Date determination:
# Takes Holzkämper et al 2015 (10.1007/s10113-014-0627-7) as standard:
#       sowing date is between 7.10 and 7.11 with a 6 day period with tas
#below 12°C and pr day 5< 20;4<16;3<12,2<8;1<4
################################################################################

sowing_date_determination <- function(env_data,earliest_date="10-07",
                                      latest_date="11-07",force_sowing=TRUE){
  #'@description Determines the sowing date
  #'@param env_data List with environmental data (tas and pr at least)
  #'containing each a Date vector
  #'@param eraliest_date MM-DD of the of the beginnig of the sowing-window
  #'@param latest_date MM-DD latest day of the sowing window
  #'@param force_sowing (default =TRUE) if TRUE, the latest day of the sowing
  #'window will be chose as sowing day if conditions are bad during the whole
  #'window, else NA will be introduced
  #'@return returns a vector with dates (for each year the first possible
  #' sowing date)
  #' @importFrom stats filter
  # moving average function
  ma <- function(x, n = 5,sides_nr =1){filter(x, rep(1 / n, n),
                                              sides = sides_nr)}



  env_data[["tas"]]$year <- as.numeric(format(as.Date(env_data[["tas"]]$DATE,
                                                      format="%d/%m/%Y"),"%Y"))
  env_data[["tas"]]$month <- as.numeric(format(as.Date(env_data[["tas"]]$DATE,
                                                       format="%d/%m/%Y"),"%m"))
  env_data[["tas"]]$day <- as.numeric(format(as.Date(env_data[["tas"]]$DATE,
                                                     format="%d/%m/%Y"),"%d"))


  env_data[["pr"]]$year <- as.numeric(format(as.Date(env_data[["pr"]]$DATE,
                                                     format="%d/%m/%Y"),"%Y"))
  env_data[["pr"]]$month <- as.numeric(format(as.Date(env_data[["pr"]]$DATE,
                                                      format="%d/%m/%Y"),"%m"))
  env_data[["pr"]]$day <- as.numeric(format(as.Date(env_data[["pr"]]$DATE,
                                                    format="%d/%m/%Y"),"%d"))


  all_data <- data.frame(year = env_data[["tas"]]$year, month =
                           env_data[["tas"]]$month,
                         day= env_data[["tas"]]$day ,env_data[["tas"]]$VALUE)
  all_data[,5] <- env_data[["pr"]]$VALUE
  names(all_data) <- c("year","month","day","tas","pr")

  all_data$tas_cond <- ifelse(ma(all_data$tas,n=6)>12, TRUE,FALSE)

  for(i in 1:length(all_data$year)){
    # browser()
    if(i < 6){
      all_data$pr_cond[i] <- FALSE
    }
    else{
      if(all_data$pr[i-5]<20 && all_data$pr[(i-4)]<16 && all_data$pr[i-3]<12 &&
         all_data$pr[i-2]<8 && all_data$pr[i-1]<4){
        all_data$pr_cond[i] <- TRUE
      } else {
        all_data$pr_cond[i] <- FALSE
      }
    }
  }

  # determine actual sowing day within the sowing window
  sowing_date_years <- NULL
  yr_counter <- 1
  all_data$date <- as.Date(paste(all_data$year,all_data$month,
                                 all_data$day,sep="-"))
  all_data$sowing_cond <- ifelse(all_data$tas_cond==TRUE
                                 &all_data$pr_cond==TRUE, TRUE,FALSE)
  for(yr in unique( env_data[[1]]$year)){

    oneYear <- all_data[which(all_data$year == yr),]
    pot_peri <- subset(oneYear, date  <= as.Date(paste(yr,latest_date,sep="-")))
    pot_peri <- subset(pot_peri, date  >= as.Date(paste(yr,
                                                        earliest_date,sep="-")))
    if(length(pot_peri$date[which(pot_peri$sowing_cond==TRUE)])!=0){
      sowing_date <- pot_peri$date[which(pot_peri$sowing_cond==TRUE)][1]

    }else{
      if(force_sowing==TRUE){
        sowing_date <- as.Date(paste(yr,latest_date,sep="-"))
      }else{
        sowing_date<- NA
      }
    }
    sowing_date_years[yr_counter]<- (as.character(sowing_date))
    yr_counter <- yr_counter+1

  } # end year condition
  return(sowing_date_years)

}

