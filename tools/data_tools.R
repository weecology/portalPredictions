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

#' Get next 12 new moon dates and assign newmoon numbers for forecasting
#' @param moons current newmoonnumber table
#' 
#' @return expected moons table for 12 future new moons
#' @examples
#' get_future_moons(moons)
#' 
get_future_moons <- function(moons, num_future_moons=12){
  # Get dates of 12 future new moons from navy website
  # Returns data.frame of newmoons in the future in the same format as the output of get_moon_data() function
  most_recent_year = tail(moons$year,1)
  most_recent_month = tail(moons$month,1)+1
  if (most_recent_month == 13) {
    most_recent_month = 1
    most_recent_year = most_recent_year +1
  }
  newmoondates = htmltab(doc=paste("http://aa.usno.navy.mil/cgi-bin/aa_phases.pl?year=",most_recent_year,"&month=",most_recent_month,"&day=1&nump=50&format=t", sep=""),which=1)
  newmoondates = gsub('.{6}$', '', newmoondates$"Date and Time (Universal Time)"[newmoondates$"Moon Phase" == "New Moon"])
  newmoondates = as.Date(ymd(newmoondates, format='%Y %m %d'))
  newmoondates = newmoondates[1:num_future_moons]
  if(length(newmoondates)!=num_future_moons){stop('Not enough moons obtained. Expected ',num_future_moons,' got ',length(newmoondates))}
  #Set up dataframe for new moon dates to be added
  newmoons = data.frame(newmoonnumber = max(moons$newmoonnumber)+1:length(newmoondates),
                        newmoondate = as.Date(newmoondates),
                        period = NA,
                        censusdate = as.Date(NA),
                        year = year(newmoondates),
                        month = month(newmoondates))
  return(newmoons)
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
