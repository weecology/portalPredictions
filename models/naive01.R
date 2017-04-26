

library(forecast)

naive01=function(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months,CI_level) {


model01=forecast(abundances$total,h=num_forecast_months,level=CI_level,BoxCox.lambda(0),allow.multiplicative.trend=T)

forecasts01=data.frame(date=forecast_date, forecastmonth=forecast_months,forecastyear=forecast_years, NewMoonNumber=forecast_newmoons,
                         currency="abundance",model="Forecast", level=level, species="total", estimate=model01$mean,
                         LowerPI=model01$lower[,which(model01$level==CI_level*100)], UpperPI=model01$upper[,which(model01$level==CI_level*100)])
  forecasts01[sapply(forecasts01, is.ts)] <- lapply(forecasts01[sapply(forecasts01, is.ts)],unclass)
  
return(list(forecasts01,model01$model$aic))  
} 