library(tscount)
library(forecast)
library(lubridate)
library(dplyr)
library(magrittr)
library(testit)
source('forecast_tools.R')

#Period 203/NewMoonNumber 217 will be when the training data timeseries
#begins. Corresponding to Jan 1995
historic_start_period=203
historic_start_newmoon=217

#Get the newmoon number of the  most recent sample
moons <- read.csv(FullPath('PortalData/Rodents/moon_dates.csv', '~'), header=T)
most_recent_newmoon = moons$NewMoonNumber[which.max(moons$Period)]

#Add in year and month to join with the rest of the data
moons$Year=year(moons$NewMoonDate)
moons$Month=month(moons$NewMoonDate)

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

#Beginning and end of the forecast timeperiod
first_forecast_newmoon=most_recent_newmoon+1
last_forecast_newmoon=first_forecast_newmoon + 11
forecast_newmoons = first_forecast_newmoon:last_forecast_newmoon
forecast_months=month(forecast_date %m+% months(0:11))
forecast_years=year(forecast_date %m+% months(0:11))

####################################################################################
#get Portal abundance data for the entire site and for control plots only.
moons$Year=year(moons$NewMoonDate); moons$Month=month(moons$NewMoonDate)
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

###################################################################################
#get weather data
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

#Get only relevent columns now that this is isn't needed to subset weather.
all=all %>%
  select(-NewMoonDate,-CensusDate,-Period,-Year,-Month)

#tscount::tsglm() will not model a timeseries of all 0's. So for those species, which are
#ones that just haven't been observed in a while, make a forecast of all 0's.
zero_abund_forecast = list(pred=rep(0,12), interval=matrix(rep(0,24), ncol=2))
colnames(zero_abund_forecast$interval) = c('lower','upper')

#####Forecasting wrapper function for all models########################

forecastall <- function(abundances, level, weather_data, weatherforecast, CI_level = 0.9, num_forecast_months = 12) {
  all_model_aic = data.frame()

  ##Community level predictions

  #naive models
  model01=forecast(abundances$total,h=num_forecast_months,level=CI_level,BoxCox.lambda(0),allow.multiplicative.trend=T)

  forecasts01=data.frame(date=forecast_date, forecastmonth=forecast_months,forecastyear=forecast_years, NewMoonNumber=forecast_newmoons,
                         currency="abundance",model="Forecast", level=level, species="total", estimate=model01$mean,
                         LowerPI=model01$lower[,which(model01$level==CI_level*100)], UpperPI=model01$upper[,which(model01$level==CI_level*100)])
  forecasts01[sapply(forecasts01, is.ts)] <- lapply(forecasts01[sapply(forecasts01, is.ts)],unclass)
  all_model_aic = all_model_aic %>%
    bind_rows(data.frame(date=forecast_date, model='Forecast', currency='abundance', level=level, species='total', aic=model01$model$aic))

  model02=forecast(auto.arima(abundances$total,lambda = 0),h=num_forecast_months,level=CI_level,fan=T)

  forecasts02=data.frame(date=forecast_date, forecastmonth=forecast_months, forecastyear=forecast_years, NewMoonNumber=forecast_newmoons,
                         currency="abundance", model="AutoArima", level=level, species="total", estimate=model02$mean,
                         LowerPI=model02$lower[,which(model02$level==CI_level*100)], UpperPI=model02$upper[,which(model02$level==CI_level*100)])
  forecasts02[sapply(forecasts02, is.ts)] <- lapply(forecasts02[sapply(forecasts02, is.ts)],unclass)
  all_model_aic = all_model_aic %>%
    bind_rows(data.frame(date=forecast_date, model='AutoArima', currency='abundance', level=level, species='total', aic=model02$model$aic))

  #Start builing results table
  forecasts=rbind(forecasts01,forecasts02)

  ##Time Series Model and Species level predictions
  #Note: PI is missing. It has an error in the Poison Env model which produces NA values.
  species=c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PL','PM','PP','RF','RM','RO','SF','SH','SO','total')

  #Model AIC sometimes doesn't work if species counts are low. In that case give a very large AIC.
  for(s in species) {
    species_abundance = abundances %>%
      extract2(s)

    if(sum(species_abundance) == 0){
      pred = zero_abund_forecast
      model_aic = 1e6
    } else {
      model=tsglm(species_abundance,model=list(past_obs=1,past_mean=12),distr="nbinom")
      pred=predict(model,num_forecast_months,level=CI_level)
      model_aic = ifelse(has_error(summary(model)),1e6,summary(model)$AIC)
    }
    newpred=data.frame(date=rep(forecast_date,num_forecast_months), forecastmonth=forecast_months, forecastyear=forecast_years,
                       NewMoonNumber=forecast_newmoons, currency="abundance", model=rep("NegBinom Time Series",num_forecast_months),
                       level=level, species=rep(s,num_forecast_months), estimate=pred$pred,
                       LowerPI=pred$interval[,1], UpperPI=pred$interval[,2])
    forecasts=rbind(forecasts,newpred)

    all_model_aic = all_model_aic %>%
      bind_rows(data.frame(date=forecast_date, model='NegBinom Time Series', currency='abundance', level=level, species=s, aic=model_aic))
  }

  #Species level time time series model with the best environmental covariates chosen by AIC

  #List of candiate environmental covariate models
  model_covariates = list(c('MaxTemp','MeanTemp','Precipitation','NDVI'),
                          c('MaxTemp','MinTemp','Precipitation','NDVI'),
                          c('MinTemp','MaxTemp','MeanTemp','Precipitation'),
                          c('Precipitation','NDVI'),
                          c('MinTemp','NDVI'),
                          c('MinTemp'),
                          c('MaxTemp'),
                          c('MeanTemp'),
                          c('Precipitation'),
                          c('NDVI'))

  for(s in species) {
    species_abundance = abundances %>%
      extract2(s)

    if(sum(species_abundance) == 0){
      pred = zero_abund_forecast
      model_aic = 1e6
    } else {
      best_model_aic = Inf
      best_model = NA
      for(proposed_model_covariates in model_covariates){
        proposed_model = tsglm(species_abundance, model=list(past_obs=1,past_mean=12), distr="poisson",
                               xreg=weather_data[,unlist(proposed_model_covariates)], link = "log")
        #tsglm sometimes outputs an error when the time series have many 0's, in that case set the AIC
        #to Inf so this proposed model covariate set get skipped
        proposed_model_aic = ifelse(has_error(summary(proposed_model)), Inf, summary(proposed_model)$AIC)
        if(proposed_model_aic < best_model_aic){
          best_model = proposed_model
          best_model_aic = proposed_model_aic
        }
      }

      #If no best model was chosen, ie. they all had infinit AIC's due to errors in model building
      #then forecast 0's. Also make an extremely high AIC so this isn't weighted heavily in the ensemble.
      if(is.na(best_model)){
        pred = zero_abund_forecast
        model_aic = 1e6
      } else {
        pred = predict(best_model,num_forecast_months,level=CI_level,newdata=weathermeans)
        model_aic = best_model_aic
      }
    }
    newpred = data.frame(date=rep(forecast_date,num_forecast_months), forecastmonth=forecast_months, forecastyear=forecast_years,
                         NewMoonNumber=forecast_newmoons, currency="abundance", model=rep("Poisson Env",num_forecast_months),
                         level=level, species=rep(s,num_forecast_months), estimate=pred$pred,
                         LowerPI=pred$interval[,1],UpperPI=pred$interval[,2])
    forecasts = rbind(forecasts,newpred)
    all_model_aic = all_model_aic %>%
      bind_rows(data.frame(date=forecast_date, model='Poisson Env', currency='abundance', level=level, species=s, aic=model_aic))
  }

  forecast_file_name = paste(as.character(forecast_date), level, filename_suffix, ".csv", sep="")
  model_aic_file_name = paste(as.character(forecast_date), level, filename_suffix, "_model_aic.csv", sep="")
  write.csv(forecasts, file.path('predictions', forecast_file_name), row.names=FALSE)
  write.csv(all_model_aic, file.path('predictions', model_aic_file_name), row.names=FALSE)

  return(forecasts)
}

######Run Models########################################################
allforecasts=forecastall(all,"All",weather_data,weathermeans)
controlsforecasts=forecastall(controls,"Controls",weather_data,weathermeans)
