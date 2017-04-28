
library(forecast)

naive02=function(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months,CI_level) {

model=forecast(auto.arima(abundances$total,lambda = 0),h=num_forecast_months,level=CI_level,fan=T)

forecasts02=data.frame(date=forecast_date, forecastmonth=forecast_months, forecastyear=forecast_years, NewMoonNumber=forecast_newmoons,
                       currency="abundance", model="AutoArima", level=level, species="total", estimate=model$mean,
                       LowerPI=model$lower[,which(model$level==CI_level*100)], UpperPI=model$upper[,which(model$level==CI_level*100)])
forecasts02[sapply(forecasts02, is.ts)] <- lapply(forecasts02[sapply(forecasts02, is.ts)],unclass)

return(list(forecasts02,model$model$aic))

}