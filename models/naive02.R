###naive model####
#Model 2 is the default Forecast package auto.arima (lambda=0)
library(yaml)
library(forecast)

naive2=function(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months = 12, CI_level = .9) {

model=forecast(auto.arima(abundances$total,lambda = 0),h=num_forecast_months,level=CI_level,fan=T)

forecasts02=data.frame(date=forecast_date, forecastmonth=forecast_months, forecastyear=forecast_years, newmoonnumber=forecast_newmoons,
                       currency="abundance", model="AutoArima", level=level, species="total", estimate=model$mean,
                       LowerPI=model$lower[,which(model$level==CI_level*100)], UpperPI=model$upper[,which(model$level==CI_level*100)])
forecasts02[sapply(forecasts02, is.ts)] <- lapply(forecasts02[sapply(forecasts02, is.ts)],unclass)

  #########Include columns describing the data used in the forecast###############
  forecasts02$fit_start_newmoon = min(abundances$newmoonnumber)
  forecasts02$fit_end_newmoon = max(abundances$newmoonnumber)
  forecasts02$initial_newmoon = max(abundances$newmoonnumber)

aic = data.frame(date=as.Date(forecast_date), currency='abundance', model='AutoArima', level=level, species='total', 
                 aic=as.numeric(model$model$aic), fit_start_newmoon = min(abundances$newmoonnumber),
                 fit_end_newmoon = max(abundances$newmoonnumber), initial_newmoon = max(abundances$newmoonnumber))

return(list(forecasts02,aic))
}

#Run model on all plots and just controls

#Get data
all = read.csv("data/rodent_all.csv")
controls = read.csv("data/rodent_controls.csv")
model_metadata = yaml.load_file("data/model_metadata.yaml")
forecast_date = as.Date(model_metadata$forecast_date)
filename_suffix = model_metadata$filename_suffix
forecast_months = model_metadata$forecast_months
forecast_years = model_metadata$forecast_years
forecast_newmoons = model_metadata$forecast_newmoons

#Forecast All plots
allresults = naive2(all,forecast_date,forecast_months,forecast_years,forecast_newmoons,"All")

#Forecast Control plots
controlsresults = naive2(controls,forecast_date,forecast_months,forecast_years,forecast_newmoons,"Controls")

#Write results
write.csv(allresults[1],file.path('tmp', paste("naive2All", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(allresults[2],file.path('tmp', paste("naive2All", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)
write.csv(controlsresults[1],file.path('tmp', paste("naive2Controls", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(controlsresults[2],file.path('tmp', paste("naive2Controls", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)
