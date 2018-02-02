# weecology-esss
#
# Model "weecology-esss" is a flexible exponential smoothing state space model
#  fit using the forecast and ets functions with the possibilit of 
#  multiplicative trends. Unfortunately because of the seasonality and 
#  sampling occurring with different frequencies, which the ets function 
#  cannot accommodate, seasonal models are not included.









###naive model####

#Model 1 is the default Forecast package with BoxCox.lambda(0),allow.multiplicative.trend=T
library(yaml)
library(forecast)

#Write model function

naive1=function(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months = 12, CI_level = .9) {

model01=forecast(abundances$total,h=num_forecast_months,level=CI_level,BoxCox.lambda(0),allow.multiplicative.trend=T)

forecasts01=data.frame(date=forecast_date, forecastmonth=forecast_months,forecastyear=forecast_years, newmoonnumber=forecast_newmoons,
                         currency="abundance",model="Forecast", level=level, species="total", estimate=model01$mean,
                         LowerPI=model01$lower[,which(model01$level==CI_level*100)], UpperPI=model01$upper[,which(model01$level==CI_level*100)])
  forecasts01[sapply(forecasts01, is.ts)] <- lapply(forecasts01[sapply(forecasts01, is.ts)],unclass)
  
  #########Include columns describing the data used in the forecast###############
  forecasts01$fit_start_newmoon = min(abundances$newmoonnumber)
  forecasts01$fit_end_newmoon   = max(abundances$newmoonnumber)
  forecasts01$initial_newmoon   = max(abundances$newmoonnumber)
  
  aic = data.frame(date=as.Date(forecast_date), currency='abundance', model='Forecast', level=level, species='total', 
                   aic = as.numeric(model01$model$aic), fit_start_newmoon = min(abundances$newmoonnumber),
                    fit_end_newmoon = max(abundances$newmoonnumber), initial_newmoon = max(abundances$newmoonnumber))

  return(list(forecasts01,aic))
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
allresults = naive1(all,forecast_date,forecast_months,forecast_years,forecast_newmoons,"All")

#Forecast Control plots
controlsresults = naive1(controls,forecast_date,forecast_months,forecast_years,forecast_newmoons,"Controls")

#Combine
forecasts = bind_rows(allresults[1],controlsresults[1])
forecast_aics = bind_rows(allresults[2],controlsresults[2])

#Write results
write.csv(forecasts,file.path('tmp', paste("naive1", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(forecast_aics,file.path('tmp', paste("naive1", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)
