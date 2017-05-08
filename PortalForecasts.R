library(lubridate)
library(dplyr)
library(magrittr)
library(rmarkdown)
source('forecast_tools.R')
source('model_functions.R')

######Run Models########################################################
  #By default name files YYYY-MM-DDXXXforecasts.csv. If hindcasting is being done
  #(signified by hindcast CLI argument) then name them  YYYY-MM-DDXXXhindcast.csv
args=commandArgs(trailingOnly = TRUE)
if(is.na(args[1])){
  filename_suffix = 'forecasts'
} else if(args[1]=='hindcast') {
  filename_suffix = 'hindcasts'
} else {
  stop(paste('Argument uknown: ', args[1]))
}

#The date this forecast model is run. Always todays date.
forecast_date = Sys.Date()

moons = get_moon_data()

#Beginning and end of the forecast timeperiod
most_recent_newmoon = moons$NewMoonNumber[which.max(moons$Period)]
first_forecast_newmoon=most_recent_newmoon+1
last_forecast_newmoon=first_forecast_newmoon + 11
forecast_newmoons = first_forecast_newmoon:last_forecast_newmoon
forecast_months=month(forecast_date %m+% months(0:11))
forecast_years=year(forecast_date %m+% months(0:11))

rodent_data = get_rodent_data(moons, forecast_date, filename_suffix)
weather_data = get_weather_data(moons, rodent_data$all, first_forecast_newmoon, last_forecast_newmoon)

#Get only relevent columns now that this is isn't needed to subset weather.
rodent_data$all = rodent_data$all %>%
  select(-NewMoonDate,-CensusDate,-Period,-Year,-Month)

#tscount::tsglm() will not model a timeseries of all 0's. So for those species, which are
#ones that just haven't been observed in a while, make a forecast of all 0's.
zero_abund_forecast = list(pred=rep(0,12), interval=matrix(rep(0,24), ncol=2))
colnames(zero_abund_forecast$interval) = c('lower','upper')

allforecasts=forecastall(rodent_data$all,"All",weather_data,weathermeans, forecast_date, forecast_newmoons, forecast_months, forecast_years)
controlsforecasts=forecastall(rodent_data$controls,"Controls",weather_data,weathermeans, forecast_date, forecast_newmoons, forecast_months, forecast_years)

######Update Website####################################################
rmarkdown::render('portal-forecast.Rmd', output_format = "html_document",
                  output_file = 'index.html', output_dir = 'docs')
