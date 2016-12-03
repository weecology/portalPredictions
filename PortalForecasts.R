library(tscount)
library(forecast)
library(lubridate)
library(dplyr)
library(testit)


#get Portal Data
source("~/PortalData/DataSummaryScripts/RodentAbundances.R"); abundances=abundance(level="Treatment",type="Rodents")
abundances$total = rowSums(abundances[,-(1:2)]); 
abundances=subset(abundances,treatment=="control",select=-treatment)
abundances=subset(abundances,period>202)
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


#Time Series model with environmental covariates, max, min and mean temp, precip and NDVI with 6 month lag

##Get 6 month weather forecast from monthly means
weatherforeast=weather[388:428,] %>% group_by(Month) %>% 
  summarize(MinTemp=mean(MinTemp),MaxTemp=mean(MaxTemp),MeanTemp=mean(MeanTemp),Precipitation=mean(Precipitation),NDVI=mean(NDVI,na.rm=T)) %>%
                      slice(match(c(12,1:11), Month))

##Create environmental covariate models
X=list(c(3:6,9),c(4:6,9),c(3,4,6,9),c(3:6),c(6,9),c(3,9),3,4,5,6,9)
  
for(s in 2:23) {
  ##Find best covariate model
  model=tsglm(abundances[[s]],model=list(past_obs=1,past_mean=11),distr="poisson",xreg=weather[173:425,unlist(X[1])],link = "log")
  modelaic=ifelse(has_error(summary(model))==T,Inf,summary(model)$AIC)
  for(i in 2:11) {
    newmodel=tsglm(abundances[[s]],model=list(past_obs=1,past_mean=11),distr="poisson",xreg=weather[173:425,unlist(X[i])],link = "log")
    newmodelaic=ifelse(has_error(summary(newmodel))==T,Inf,summary(newmodel)$AIC)  
    if(newmodelaic < modelaic) {model=newmodel}}
  
  pred=predict(model,12,level=0.9,newdata=weatherforecast) 
  newpred=data.frame(date=rep(Sys.Date(),12), forecastmonth=forecastmonth,
                     forecastyear=forecastyear, model=rep("Poisson Env",12), 
                     species=rep(species[s],12), estimate=pred$pred, LowerPI=pred$interval[,1],UpperPI=pred$interval[,2])
  forecasts=rbind(forecasts,newpred)
}


write.csv(forecasts,"controlsforecasts.csv")
