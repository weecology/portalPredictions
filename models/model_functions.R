library(lubridate)
library(dplyr)
library(magrittr)
library(rmarkdown)
source('forecast_tools.R')

get_moon_data <- function(){
  #Get the newmoon number of the  most recent sample
  moons <- read.csv(FullPath('PortalData/Rodents/moon_dates.csv', '~'), header=T)
  
  #Add in year and month to join with the rest of the data
  moons$Year=year(moons$NewMoonDate)
  moons$Month=month(moons$NewMoonDate)
  return(moons)
}

get_rodent_data <- function(moons, forecast_date, filename_suffix){
  #Period 203/NewMoonNumber 217 will be when the training data timeseries
  #begins. Corresponding to Jan 1995
  historic_start_period=203
  historic_start_newmoon=217
  
  ####################################################################################
  #get Portal abundance data for the entire site and for control plots only.
  controls=PortalDataSummaries::abundance(level="Treatment",type="Rodents",length="Longterm", incomplete = FALSE)
  
  #Control plots
  #The total rodent count in each treatment
  controls$total = rowSums(controls[,-(1:2)])
  #Drop non-control treatments and add in NewMoonNumber
  controls = controls %>%
    filter(treatment == 'control') %>%
    select(-treatment) %>%
    inner_join(moons,by=c("period"="Period")) %>%
    subset(NewMoonNumber >= historic_start_newmoon) %>%
    select(-NewMoonDate,-CensusDate,-period,-Year,-Month)
  
  #All plots
  all=PortalDataSummaries::abundance(level="Site",type="Rodents",length="all", incomplete = FALSE)
  #The total rodent count across the entire site
  all$total = rowSums(all[,-(1)])
  all=inner_join(moons,all,by=c("Period"="period"))
  
  all=subset(all,Period >= historic_start_period)
  rodent_data = list()
  rodent_data$controls = controls
  rodent_data$all = all
  return(rodent_data)
}

###################################################################################
#get weather data
get_weather_data <- function(moons, all, first_forecast_newmoon, last_forecast_newmoon){
  weather_data=PortalDataSummaries::weather("Monthly") %>%
    ungroup() %>%
    left_join(moons, by=c('Year','Month'))
  
  #Offset the NewMoonNumber to create a 6 month lag between
  #rodent observations and weather
  weather_data$NewMoonNumber_with_lag = weather_data$NewMoonNumber + 6
  
  #Assign weather using lag to rodent observations.
  #This will match weather row numbers to corrosponding rows in all and controls
  weather_data = weather_data %>%
    select(-NewMoonDate, -CensusDate, -Period, -Year, -Month) %>%
    right_join(all, by=c('NewMoonNumber_with_lag'='NewMoonNumber')) %>%
    select(Year,Month,MinTemp,MaxTemp,MeanTemp,Precipitation,NDVI,NewMoonNumber, NewMoonNumber_with_lag)
  
  ##Get 6 month weather forecast by combining stations data and monthly means of past 3 years
  #Used to make predictions for the months 7-12 of a 12 month forecast using a 6 month lag.
  
  #A data.frame of the months that will be in this weather forecast.
  weather_forecast_months = moons %>%
    filter(NewMoonNumber >= first_forecast_newmoon-5, NewMoonNumber <= last_forecast_newmoon-5)
  
  #  x=subset(weather,NewMoonNumber>=first_forecast_newmoon-5) %>%
  #  subset(NewMoonNumber<=last_forecast_newmoon-5)
  
  weathermeans=weather_data[dim(weather_data)[1]-36:dim(weather_data)[1],] %>%
    group_by(Month) %>%
    summarize(MinTemp=mean(MinTemp,na.rm=T),MaxTemp=mean(MaxTemp,na.rm=T),MeanTemp=mean(MeanTemp,na.rm=T),
              Precipitation=mean(Precipitation,na.rm=T),NDVI=mean(NDVI,na.rm=T)) %>%
    slice(match(weather_forecast_months$Month, Month))
  
  #Insert longterm means where there is missing data in the historic weather
  weather_data=weather_data %>%
    mutate(NDVI = ifelse(is.na(NDVI), mean(NDVI, na.rm = T), NDVI)) %>%
    mutate(MinTemp = ifelse(is.na(MinTemp), mean(MinTemp, na.rm = T), MinTemp)) %>%
    mutate(MaxTemp = ifelse(is.na(MaxTemp), mean(MaxTemp, na.rm = T), MaxTemp)) %>%
    mutate(MeanTemp = ifelse(is.na(MeanTemp), mean(MeanTemp, na.rm = T), MeanTemp)) %>%
    mutate(Precipitation = ifelse(is.na(Precipitation), mean(Precipitation, na.rm = T), Precipitation))
}


#####Forecasting wrapper function for all models########################

forecastall <- function(abundances, level, weather_data, weatherforecast,
                        forecast_date, forecast_newmoons,
                        forecast_months, forecast_years,
                        CI_level = 0.9, num_forecast_months = 12) {
  
  #forecasts is where we will append all forecasts for this level
  #all_model_aic is where we will append all model aics for this level
  forecasts = data.frame(date=as.Date(character()), forecastmonth=numeric(), forecastyear=numeric(),
                         NewMoonNumber=numeric(), currency=character(), model=character(), level=character(),
                         species=character(), estimate=numeric(), LowerPI=numeric(), UpperPI=numeric())
  all_model_aic = data.frame(date=as.Date(character()),currency=character(),model=character(),level=character(),
                             species=character(), aic=numeric())
  
  #######Community level predictions#################
  
  
  ###naive models####
  
  #Model 1 is the default Forecast package with BoxCox.lambda(0),allow.multiplicative.trend=T
  
  source('models/naive01.R')
  model01=naive01(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months,CI_level)
  
  #Append results to forecasts and AIC tables
  forecasts = forecasts  %>%
    bind_rows(data.frame(model01[1]))
  
  all_model_aic = all_model_aic %>%
    bind_rows(data.frame(date=forecast_date, currency='abundance', model='Forecast', level=level, species='total', aic=as.numeric(unlist(model01[2]))))
  
  
  #Model 2 is the default Forecast package auto.arima (lambda=0)
  source('models/naive02.R')
  model02=naive02(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months,CI_level)
  
  #Append results to forecasts and AIC tables
  forecasts = forecasts  %>%
    bind_rows(data.frame(model02[1]))
  
  all_model_aic = all_model_aic %>%
    bind_rows(data.frame(date=forecast_date, model='AutoArima', currency='abundance', level=level, species='total', aic=unlist(model02[2])))
  
  
  
  ####### Species level predictions #################
  #total is also included in these, for a community-level prediction
  
  ##Negative Binomial Time Series Model
  source('models/neg_binom_ts.R')
  nbts=neg_binom_ts(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months,CI_level)
  
  #Append results to forecasts and AIC tables
  forecasts = forecasts  %>%
    bind_rows(data.frame(nbts[1]))
  
  all_model_aic = all_model_aic %>%
    bind_rows(data.frame(nbts[2]))
  
  ##Poisson environmental
  #Species level time series model with the best environmental covariates chosen by AIC
  source('models/pois_env_ts.R')
  pets=pois_env_ts(abundances,weather_data,weathermeans,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months,CI_level)
  
  #Append results to forecasts and AIC tables
  forecasts = forecasts  %>%
    bind_rows(data.frame(pets[1]))
  
  all_model_aic = all_model_aic %>%
    bind_rows(data.frame(pets[2]))
  
  
  #########Write forecasts to file and aics to separate files###############
  #Appending a csv without re-writing the header. Needed for hindcasting
  #when many hindcasts are put into the same dated files. 
  append_csv=function(df, filename){
    write.table(df, filename, sep = ',', row.names = FALSE, col.names = !file.exists(filename), append = file.exists(filename))
  }
  
  forecast_filename = file.path('predictions', paste(as.character(forecast_date), level, filename_suffix, ".csv", sep=""))
  model_aic_filename = file.path('predictions', paste(as.character(forecast_date), level, filename_suffix, "_model_aic.csv", sep=""))
  append_csv(forecasts, forecast_filename)
  append_csv(all_model_aic, model_aic_filename)
  
  ########Add ensembles to files############################################
  ensemble=make_ensemble(forecasts) %>% 
    subset(select=colnames(forecasts))
  append_csv(ensemble, forecast_filename)
  
  return(list(forecasts,all_model_aic))
}
