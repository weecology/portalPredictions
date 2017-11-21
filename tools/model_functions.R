library(lubridate)
library(dplyr)
library(magrittr)
library(rmarkdown)
library(htmltab)

#########Write forecasts to file and aics to separate files###############
#Appending a csv without re-writing the header. Needed for hindcasting
#when many hindcasts are put into the same dated files. 
append_csv=function(df, filename){
  write.table(df, filename, sep = ',', row.names = FALSE, col.names = !file.exists(filename), append = file.exists(filename))
}

get_moon_data <- function(){
  #Get the newmoon number of the  most recent sample
  moons <- read.csv(FullPath('PortalData/Rodents/moon_dates.csv', '~'), header=T)
  
  #Add in year and month to join with the rest of the data
  moons$year=year(moons$newmoondate)
  moons$month=month(moons$newmoondate)
  return(moons)
}

get_future_moons <- function(moons){
  # Get dates of future new moons from navy website
  # Returns data.frame of newmoons in the future in the same format as the output of get_moon_data() function
  most_recent_year = tail(moons$year,1)
  most_recent_month = tail(moons$month,1)+1
  newmoondates = htmltab::htmltab(doc=paste("http://aa.usno.navy.mil/cgi-bin/aa_phases.pl?year=",most_recent_year,"&month=",most_recent_month,"&day=1&nump=50&format=t", sep=""),which=1)
  newmoondates = gsub('.{6}$', '', newmoondates$"Date and Time (Universal Time)"[newmoondates$"Moon Phase" == "New Moon"])
  newmoondates = as.Date(ymd(newmoondates, format='%Y %m %d'))
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
get_rodent_data <- function(moons, forecast_date){
  #Period 203/newmoonnumber 217 will be when the training data timeseries
  #begins. Corresponding to Jan 1995
  historic_start_period=203
  historic_start_newmoon=217
  
  #get Portal abundance data for the entire site and for control plots only.
  controls=portalr::abundance(level="Treatment",type="Rodents",length="Longterm", incomplete = FALSE)
  
  #Control plots
  #The total rodent count in each treatment
  controls$total = rowSums(controls[,-(1:2)])
  #Drop non-control treatments and add in newmoonnumber
  controls = controls %>%
    filter(treatment == 'control') %>%
    select(-treatment) %>%
    inner_join(moons,by=c("period"="period")) %>%
    subset(newmoonnumber >= historic_start_newmoon) %>%
    select(-newmoondate,-censusdate,-period,-year,-month)
  
  #All plots
  all=portalr::abundance(level="Site",type="Rodents",length="all", incomplete = FALSE)
  #The total rodent count across the entire site
  all$total = rowSums(all[,-(1)])
  all=inner_join(moons,all,by=c("period"="period"))
  
  all=subset(all,period >= historic_start_period)
  rodent_data = list()
  rodent_data$controls = controls
  rodent_data$all = all
  return(rodent_data)
}

###################################################################################
#get weather data
get_weather_data <- function(moons, all, first_forecast_newmoon, last_forecast_newmoon){
  weather_data=portalr::weather("Monthly") %>%
    ungroup() %>%
    left_join(moons, by=c('year','month'))
  
  #Offset the newmoonnumber to create a 6 month lag between
  #rodent observations and weather
  weather_data$NewMoonNumber_with_lag = weather_data$newmoonnumber + 6
  
  #Assign weather using lag to rodent observations.
  #This will match weather row numbers to corrosponding rows in all and controls
  weather_data = weather_data %>%
    select(-newmoondate, -censusdate, -period, -year, -month) %>%
    right_join(all, by=c('NewMoonNumber_with_lag'='newmoonnumber')) %>%
    select(year,month,mintemp,maxtemp,meantemp,precipitation,NDVI,newmoonnumber, NewMoonNumber_with_lag)
  
  #Insert longterm means where there is missing data in the historic weather
  weather_data=weather_data %>%
    mutate(NDVI = ifelse(is.na(NDVI), mean(NDVI, na.rm = T), NDVI)) %>%
    mutate(mintemp = ifelse(is.na(mintemp), mean(mintemp, na.rm = T), mintemp)) %>%
    mutate(maxtemp = ifelse(is.na(maxtemp), mean(maxtemp, na.rm = T), maxtemp)) %>%
    mutate(meantemp = ifelse(is.na(meantemp), mean(meantemp, na.rm = T), meantemp)) %>%
    mutate(precipitation = ifelse(is.na(precipitation), mean(precipitation, na.rm = T), precipitation))
}


##########################Forecast processing############################
#Combine all new forecasts of the same level, add columns, add ensembles

forecastall <- function(level, filename_suffix = 'forecasts') {
  

#######Collect all model results################# 
  #forecasts is where we will append all forecasts for this level
  #all_model_aic is where we will append all model aics for this level
  forecasts = data.frame(date=as.Date(character()), forecastmonth=numeric(), forecastyear=numeric(),
                         newmoonnumber=numeric(), currency=character(), model=character(), level=character(),
                         species=character(), estimate=numeric(), LowerPI=numeric(), UpperPI=numeric())
  all_model_aic = data.frame(date=as.Date(character()),currency=character(),model=character(),level=character(),
                             species=character(), aic=numeric())
  
  
  #List files 
  
  
  #Append results to forecasts and AIC tables
  forecasts = 
  
  all_model_aic = 
  
  #########Include columns describing the data used in the forecast###############
  forecasts$fit_start_newmoon = min(abundances$newmoonnumber)
  forecasts$fit_end_newmoon   = max(abundances$newmoonnumber)
  forecasts$initial_newmoon   = max(abundances$newmoonnumber)
  all_model_aic$fit_start_newmoon = min(abundances$newmoonnumber)
  all_model_aic$fit_end_newmoon   = max(abundances$newmoonnumber)
  all_model_aic$initial_newmoon = max(abundances$newmoonnumber)
  

  
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
