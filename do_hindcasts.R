library(tidyverse)
library(lubridate)
source('forecast_tools.R')



#' Reset all the observed data (which will be used to training) to look
#' like the sampling period in period_info just happened. 
#' @param period_info data.frame One row data.frame output from new_moons
#' @return None
backdate_observed_data=function(period_info){
  rodents_filename  = '~/PortalData/Rodents/Portal_rodent.csv'
  new_moon_filename = '~/PortalData/Rodents/moon_dates.csv'
  weather_filename  = '~/PortalData/Weather/Portal_weather.csv'
  
  period_info$CensusDate = as.Date(period_info$CensusDate)
  period_info$month      = lubridate::month(period_info$CensusDate)
  period_info$year       = lubridate::year(period_info$CensusDate)
  
  rodents = read_csv(rodents_filename, col_types = cols())
  rodents = rodents %>%
    filter(period<=period_info$Period)
  write_csv(rodents, rodents_filename)
  
  new_moons = read_csv(new_moon_filename, col_types = cols())
  new_moons = new_moons %>%
    filter(Period<=period_info$Period)
  write_csv(new_moons, new_moon_filename)
  
  weather = read_csv(weather_filename, col_types = cols())
  weather = weather %>%
    filter((Year<period_info$year | ((Year==period_info$year) & (Month<=period_info$month))))
  write_csv(weather, weather_filename)
}


#Hindcast will set the initial time period based on these periods. For each one
#a hindcast will be made which pretends that sampleing period had just happened. 
initial_time_periods=421:430

#Get the latest to obtatain the latest new moon info
download_observations()


new_moons=read.csv('~/PortalData/Rodents/moon_dates.csv')

#Period number, year, and  month of sampling will be used to backdate the observation data
initial_times_info = new_moons %>%
  filter(Period %in% initial_time_periods)

for(i in seq_along(initial_times_info)){
  backdate_observed_data(initial_times_info[i,])
  download_observations()
}

#for initial_time in the past
# download data & filter all to be before or after that date
# IDEA: Go by period number. Get period number, get sample date from new_moon_csv, filter everything to before that date.
# 
# run forecast script
