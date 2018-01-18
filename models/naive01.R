###naive model####

#Model 1 is the default Forecast package with BoxCox.lambda(0),allow.multiplicative.trend=T
library(yaml)
library(forecast)

#Write model function

naive1=function(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months = 12, CI_level = .9) {

# interpolate missing data

moons <- (min(abundances$newmoonnumber)):(max(abundances$newmoonnumber))
abunds <- rep(NA, length(moons))
for(i in 1:length(moons)){
  if(length(which(abundances$newmoonnumber == moons[i])) > 0){
    abunds[i] <- abundances$total[abundances$newmoonnumber == moons[i]]
  }
}

interpolated_abundances <- na.interp(abunds)


model01=forecast(interpolated_abundances,h=num_forecast_months,level=CI_level,BoxCox.lambda(0),allow.multiplicative.trend=T)

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
