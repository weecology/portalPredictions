library(lubridate)
library(dplyr)
library(magrittr)
library(htmltab)

#' Return normalized path for all operating systems
#' @param ReferencePath a path to join with current working directory
#' @param BasePath Current working directory else path given
#'
#' @return
#' @export
#' @examples
#' FullPath('PortalData/Rodents/Portal_rodent.csv')
#' FullPath('PortalData/Rodents/Portal_rodent.csv', '~')
FullPath <- function( ReferencePath, BasePath=getwd()){
  BasePath = normalizePath(BasePath)
  Path = normalizePath(file.path(BasePath, ReferencePath), mustWork = FALSE)
  return (Path)
}

#' Get data for newmoonnumbers and related trapping period codes
#' @return newmoons table
#' @examples
#' get_moon_data()
#' 
get_moon_data <- function(){
  #Get the newmoon number of the  most recent sample
  moons <- read.csv(FullPath('PortalData/Rodents/moon_dates.csv', '~'), header=T)
  
  #Add in year and month to join with the rest of the data
  moons$year=year(moons$newmoondate)
  moons$month=month(moons$newmoondate)
  return(moons)
}

####################################################################################
#' Get rodent data, tailored for forecasting (all plots and controls only)
#' @param moons current newmoonnumber table
#' @param forecast_date date the forecast is run
#' 
#' @return a list of two dataframes, all plots and control plots
#' @examples
#' get_rodent_data(moons, forecast_date)
#'

get_rodent_data <- function(moons, forecast_date){
  #Period 203/newmoonnumber 217 will be when the training data timeseries
  #begins. Corresponding to Jan 1995
  historic_start_period=203
  historic_start_newmoon=217
  
  #get Portal abundance data for the entire site and for control plots only.
  
  #Control plots
  controls = portalr::abundance(level="Treatment",type="Rodents",length="Longterm", incomplete = FALSE)
  #Drop PI
  controls = controls[ , -which(colnames(controls) == "PI")]
  #The total rodent count in each treatment
  controls$total = rowSums(controls[,-(1:2)])
  #Drop non-control treatments and add in newmoonnumber
  controls = controls %>%
    filter(treatment == 'control') %>%
    select(-treatment) %>%
    inner_join(moons,by=c("period"="period")) %>%
    subset(newmoonnumber >= historic_start_newmoon) %>%
    select(-newmoondate,-censusdate)
  
  #All plots
  all = portalr::abundance(level="Site",type="Rodents",length="all", incomplete = FALSE)
  #Drop PI
  all = all[ , -which(colnames(all) == "PI")]
  #The total rodent count across the entire site
  all$total = rowSums(all[,-(1)])
  all = all %>% inner_join(moons,by=c("period"="period")) %>%
    subset(period >= historic_start_period) %>%
    select(-newmoondate,-censusdate)
    
  rodent_data = list()
  rodent_data$controls = controls
  rodent_data$all = all
  return(rodent_data)
}
