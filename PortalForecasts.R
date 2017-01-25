library(tscount)
library(forecast)
library(lubridate)
library(dplyr)
library(testit)


#get Portal Data
moons <- read.csv("~/PortalData/Rodents/moon_dates.csv",header=T)
source("~/PortalData/DataSummaryScripts/RodentAbundances.R"); controls=abundance(level="Treatment",type="Rodents",length="Longterm")
controls$total = rowSums(controls[,-(1:2)])
controls=subset(controls,treatment=="control",select=-treatment)
controls=subset(controls,period>202)
controls=inner_join(moons,controls,by=c("Period"="period")) %>% select(-NewMoonDate,-CensusDate,-Period)

abundances=abundance(level="Site",type="Rodents",length="all")
abundances$total = rowSums(abundances[,-(1)])
abundances=subset(abundances,period>202)
abundances=inner_join(moons,abundances,by=c("Period"="period")) %>% select(-NewMoonDate,-CensusDate,-Period)

#get weather data
source("~/PortalData/DataSummaryScripts/Weather.R"); weather=weather("Monthly")
moons$Year=year(moons$NewMoonDate); moons$Month=month(moons$NewMoonDate)
NDVI=read.csv("C:/Users/gmyenni/Dropbox/Portal/PORTAL_primary_data/NDVI/CompositeNDVI/monthly_NDVI.csv")
NDVI$Month=as.numeric(gsub( ".*-", "", NDVI$Date )); NDVI$Year=as.numeric(gsub( "-.*$", "", NDVI$Date ))
weather=full_join(weather,NDVI) %>% select(-Date, -X) %>% arrange(Year,Month)
weather=right_join(weather,moons)

##Get 6 month weather forecast from monthly means of past 3 years
weatherforecast=weather[dim(weather)[1]-36:dim(weather)[1],] %>% group_by(Month) %>% 
  summarize(MinTemp=mean(MinTemp,na.rm=T),MaxTemp=mean(MaxTemp,na.rm=T),MeanTemp=mean(MeanTemp,na.rm=T),Precipitation=mean(Precipitation,na.rm=T),NDVI=mean(NDVI,na.rm=T)) %>%
  slice(match(c(7:12,1:6), Month))

NewMoonNumber= max(abundances$NewMoonNumber) + 1:12
forecastmonth=month(Sys.Date() %m+% months(0:11))
forecastyear=year(Sys.Date() %m+% months(0:11))

##Community level predictions

#naive models
model01=forecast(abundances$total,h=12,level=0.9,BoxCox.lambda(0),allow.multiplicative.trend=T)

forecasts01=data.frame(date=Sys.Date(), forecastmonth=forecastmonth,forecastyear=forecastyear, NewMoonNumber=NewMoonNumber,
                       currency="abundance",model="Forecast", level=level, species="total", estimate=model01$mean, 
                       LowerPI=model01$lower[,which(model01$level==90)], UpperPI=model01$upper[,which(model01$level==90)])
forecasts01[sapply(forecasts01, is.ts)] <- lapply(forecasts01[sapply(forecasts01, is.ts)],unclass)


model02=forecast(auto.arima(abundances$total,lambda = 0),h=12,level=0.9,fan=T)

forecasts02=data.frame(date=Sys.Date(), forecastmonth=forecastmonth,forecastyear=forecastyear,NewMoonNumber=NewMoonNumber,
                       currency="abundance", model="AutoArima", level=level, species="total", estimate=model02$mean, 
                       LowerPI=model02$lower[,which(model02$level==90)], UpperPI=model02$upper[,which(model02$level==90)])
forecasts02[sapply(forecasts02, is.ts)] <- lapply(forecasts02[sapply(forecasts02, is.ts)],unclass)


#Start builing results table
forecasts=rbind(forecasts01,forecasts02)

##Time Series Model and Species level predictions
species=colnames(abundances)

for(s in 2:23) {

model=tsglm(abundances[[s]],model=list(past_obs=1,past_mean=12),distr="nbinom")
pred=predict(model,12,level=0.9) 
newpred=data.frame(date=rep(Sys.Date(),12), forecastmonth=forecastmonth,forecastyear=forecastyear,NewMoonNumber=NewMoonNumber,
             currency="abundance",model=rep("NegBinom Time Series",12),level=level,
            species=rep(species[s],12), estimate=pred$pred, LowerPI=pred$interval[,1],UpperPI=pred$interval[,2])
forecasts=rbind(forecasts,newpred)
}


#Time Series model with environmental covariates, max, min and mean temp, precip and NDVI with 6 month lag

##Create environmental covariate models
X=list(c(3:7),c(4:7),c(3,4,6,7),c(3:6),c(6,7),c(3,7),3,4,5,6,7)
  
for(s in 2:23) {
  ##Find best covariate model
  model=tsglm(abundances[[s]],model=list(past_obs=1,past_mean=12),distr="poisson",xreg=weather[1:dim(abundances)[1],unlist(X[1])],link = "log")
  modelaic=ifelse(has_error(summary(model))==T,Inf,summary(model)$AIC)
  for(i in 2:11) {
    newmodel=tsglm(abundances[[s]],model=list(past_obs=1,past_mean=12),distr="poisson",xreg=weather[1:dim(abundances)[1],unlist(X[i])],link = "log")
    newmodelaic=ifelse(has_error(summary(newmodel))==T,Inf,summary(newmodel)$AIC)  
    if(newmodelaic < modelaic) {model=newmodel}}
  
  pred=predict(model,12,level=0.9,newdata=weatherforecast) 
  newpred=data.frame(date=rep(Sys.Date(),12), forecastmonth=forecastmonth,forecastyear=forecastyear,NewMoonNumber=NewMoonNumber,
                      currency,model=rep("Poisson Env",12),level=level, 
                     species=rep(species[s],12), estimate=pred$pred, LowerPI=pred$interval[,1],UpperPI=pred$interval[,2])
  forecasts=rbind(forecasts,newpred)
}


write.csv(forecasts,"forecasts.csv",row.names=FALSE,append = T)
