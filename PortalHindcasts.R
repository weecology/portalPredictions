library(dplyr)
library(lubridate)
library(readr)
library(portalr)
source('forecast_tools.R')
source('models/model_functions.R')


filename_suffix = 'hindcasts'

#The date this forecast model is run. Always todays date.
forecast_date = Sys.Date()


#Hindcast will set the time based on these NewMoonNumbers. For each one
#a hindcast will be made which pretends that sampleing period had just happened. 
#403 to 490 is Jan,2010 - Jan,2017. 
initial_time_newmoons=403:490


for(this_newmoon in initial_time_newmoons){

  moons = get_moon_data()

  #Don't do hindcasting from newmoons that were not
  #actually sampled
  this_newmoon_sampling_date = moons %>%
    filter(newmoonnumber == this_newmoon) %>%
    pull(CensusDate)
  if(is.na(this_newmoon_sampling_date)){
    next
  }

  moons = get_moon_data() %>%
    filter(newmoonnumber<=this_newmoon)
  
  #Beginning and end of the forecast timeperiod
  most_recent_newmoon = moons$newmoonnumber[which.max(moons$period)]
  most_recent_newmoon_date = as.Date(moons$newmoondate[which.max(moons$period)])
  first_forecast_newmoon=most_recent_newmoon+1
  last_forecast_newmoon=first_forecast_newmoon + 11
  forecast_newmoons = first_forecast_newmoon:last_forecast_newmoon
  forecast_months=month(most_recent_newmoon_date %m+% months(1:12))
  forecast_years=year(most_recent_newmoon_date %m+% months(1:12))
  
  rodent_data = get_rodent_data(moons, forecast_date, filename_suffix)
  rodent_data$all = rodent_data$all %>%
    filter(newmoonnumber <= this_newmoon)
  rodent_data$controls = rodent_data$controls %>%
    filter(newmoonnumber <= this_newmoon)
  
  weather_data = get_weather_data(moons, rodent_data$all, first_forecast_newmoon, last_forecast_newmoon) %>%
    filter(newmoonnumber <= this_newmoon)
  
  #Get only relevent columns now that this is isn't needed to subset weather.
  rodent_data$all = rodent_data$all %>%
    select(-newmoondate,-censusdate,-period,-year,-month)
  
  #tscount::tsglm() will not model a timeseries of all 0's. So for those species, which are
  #ones that just haven't been observed in a while, make a forecast of all 0's.
  zero_abund_forecast = list(pred=rep(0,12), interval=matrix(rep(0,24), ncol=2))
  colnames(zero_abund_forecast$interval) = c('lower','upper')
  
  allforecasts=try(forecastall(rodent_data$all,"All",weather_data,weathermeans, forecast_date, forecast_newmoons, forecast_months, forecast_years))
  controlsforecasts=try(forecastall(rodent_data$controls,"Controls",weather_data,weathermeans, forecast_date, forecast_newmoons, forecast_months, forecast_years))

}
