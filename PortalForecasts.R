library(tscount)
library(forecast)
library(lubridate)
library(dplyr)


#get Portal Data
source("~/PortalData/DataSummaryScripts/RodentAbundances.R"); abundances=abundance(level="Site",type="Rodents")
abundances$total = rowSums(abundances[,-1]); abundances=subset(abundances,period>202)
source("~/PortalData/DataSummaryScripts/Weather.R"); weather=weather("Monthly") 

forecastmonth=month(Sys.Date() %m+% months(0:11))
forecastyear=year(Sys.Date() %m+% months(0:11))

##Community level predictions

#naive models
model01=forecast(abundances$total,h=12,level=0.95,BoxCox.lambda(0),allow.multiplicative.trend=T)

forecasts01=data.frame(date=Sys.Date(), forecastmonth=forecastmonth,
                       forecastyear=forecastyear, model="Forecast", species="Total", estimate=model01$mean, 
                       LowerPI=model01$lower[,which(model01$level==90)], UpperPI=model01$upper[,which(model01$level==90)])
forecasts01[sapply(forecasts01, is.ts)] <- lapply(forecasts01[sapply(forecasts01, is.ts)],unclass)


model02=forecast(auto.arima(abundances$total,lambda = 0),h=12,level=0.95,fan=T)

forecasts02=data.frame(date=Sys.Date(), forecastmonth=forecastmonth,
                       forecastyear=forecastyear, model="AutoArima", species="Total", estimate=model02$mean, 
                       LowerPI=model02$lower[,which(model02$level==90)], UpperPI=model02$upper[,which(model02$level==90)])
forecasts02[sapply(forecasts02, is.ts)] <- lapply(forecasts02[sapply(forecasts02, is.ts)],unclass)


#Start builing results table
forecasts=rbind(forecasts01,forecasts02)

##Time Series Model and Species level predictions
species=colnames(abundances)

for(s in 2:23) {

model=tsglm(abundances[[s]],model=list(past_obs=1,past_mean=11),distr="nbinom")
pred=predict(model,12,level=0.9) 
newpred=data.frame(date=rep(Sys.Date(),12), forecastmonth=forecastmonth,
            forecastyear=forecastyear, model=rep("NegBinom Time Series",12), 
            species=rep(species[s],12), estimate=pred$pred, LowerPI=pred$interval[,1],UpperPI=pred$interval[,2])
forecasts=rbind(forecasts,newpred)
}


#Time Series model with environmental covariates, max and min temperature and precipitation with 6 month lag
weatherforeast=weather[388:428,] %>% group_by(Month) %>% 
  summarize(MeanTemp=mean(MeanTemp),Precipitation=sum(Precipitation)) %>% slice(match(c(11,12,1:10), Month))

for(s in 2:23) {
  
  model=tsglm(abundances[[s]],model=list(past_obs=1,past_mean=11),distr="poisson",xreg=weather[173:424,-(1:4)])
  pred=predict(model,12,level=0.9,newdata=weatherforecast[-1,]) 
  newpred=data.frame(date=rep(Sys.Date(),12), forecastmonth=forecastmonth,
                     forecastyear=forecastyear, model=rep("Poisson Env",12), 
                     species=rep(species[s],12), estimate=pred$pred, LowerPI=pred$interval[,1],UpperPI=pred$interval[,2])
  forecasts=rbind(forecasts,newpred)
}


write.csv(forecasts,"forecasts.csv")
