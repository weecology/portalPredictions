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
all = read.csv("tools/rodent_all.csv")
controls = read.csv("tools/rodent_controls.csv")
data = yaml.load_file("tools/model.yaml")
forecast_date = as.Date(data$forecast_date)
filename_suffix = data$filename_suffix
forecast_months = data$forecast_months
forecast_years = data$forecast_years
forecast_newmoons = data$forecast_newmoons

#Forecast All plots
allresults = naive1(all,forecast_date,forecast_months,forecast_years,forecast_newmoons,"All")

#Forecast Control plots
controlsresults = naive1(controls,forecast_date,forecast_months,forecast_years,forecast_newmoons,"Controls")

#Write results
write.csv(allresults[1],file.path('tmp', paste("naive1All", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(allresults[2],file.path('tmp', paste("naive1All", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)
write.csv(controlsresults[1],file.path('tmp', paste("naive1Controls", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(controlsresults[2],file.path('tmp', paste("naive1Controls", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)
