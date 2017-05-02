library(dplyr)
library(lubridate)
library(readr)
library(PortalDataSummaries)

#' Reset all the observed data (which will be used to training) to look
#' like the sampling period in period_info just happened. 
#' @param period_info data.frame One row data.frame output from new_moons
#' @return None
backdate_observed_data=function(period_info, base_path='~'){
  rodents_filename  = PortalDataSummaries::FullPath('PortalData/Rodents/Portal_rodent.csv', 
                                                    base_path)
  new_moon_filename = PortalDataSummaries::FullPath('PortalData/Rodents/moon_dates.csv', 
                                                    base_path)
  weather_filename  = PortalDataSummaries::FullPath('PortalData/Weather/Portal_weather.csv', 
                                                    base_path)
  
  period_info$CensusDate = as.Date(period_info$CensusDate)
  period_info$month      = lubridate::month(period_info$CensusDate)
  period_info$year       = lubridate::year(period_info$CensusDate)
  
  rodents = read.csv(rodents_filename, na.strings=c(""), colClasses=c('tag'='character'), stringsAsFactors = FALSE)
  rodents = rodents %>%
    filter(period<=period_info$Period)
  write_csv(rodents, rodents_filename, na = '')
  
  new_moons = read_csv(new_moon_filename, col_types = cols())
  new_moons = new_moons %>%
    filter(NewMoonNumber<=period_info$NewMoonNumber)
  write_csv(new_moons, new_moon_filename, na = '')
  
  weather = read_csv(weather_filename, col_types = cols())
  weather = weather %>%
    filter((Year<period_info$year | ((Year==period_info$year) & (Month<=period_info$month))))
  write_csv(weather, weather_filename, na = '')
}


#Hindcast will set the initial time period based on these periods. For each one
#a hindcast will be made which pretends that sampleing period had just happened. 
#381 to 458 is Jan,2010 - Jan,2017. 
initial_time_periods=381:458

#Get the latest to obtain the latest new moon info
PortalDataSummaries::download_observations()

base_path = '~'
new_moon_file = PortalDataSummaries::FullPath('PortalData/Rodents/moon_dates.csv', 
                                                  base_path)

new_moons=read.csv(new_moon_file)

#Period number, year, and  month of sampling will be used to backdate the observation data
initial_times_info = new_moons %>%
  filter(Period %in% initial_time_periods)

#Run the portal forecasts script in an external command so it has it's own namespace. 
for(i in 1:nrow(initial_times_info)){
  backdate_observed_data(initial_times_info[i,])
  system('Rscript PortalForecasts.R hindcast')
  PortalDataSummaries::download_observations()
}
