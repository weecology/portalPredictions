library(yaml)
source('tools/model_functions.R')

#The date this forecast model is run. Always today's date.
forecast_date = Sys.Date()

portalr::download_observations()
moons = get_moon_data()
#get dates of 12 new moons following newmoon of interest
future_moons = get_future_moons(moons)

#Beginning and end of the forecast timeperiod
most_recent_newmoon = moons$newmoonnumber[which.max(moons$period)]
first_forecast_newmoon=most_recent_newmoon+1
last_forecast_newmoon=first_forecast_newmoon + 11
forecast_newmoons = first_forecast_newmoon:last_forecast_newmoon
forecast_months = future_moons$month[future_moons$newmoonnumber %in% forecast_newmoons]
forecast_years = future_moons$year[future_moons$newmoonnumber %in% forecast_newmoons]

rodent_data = get_rodent_data(moons, forecast_date)
weather_data = get_weather_data(moons, rodent_data$all, first_forecast_newmoon, last_forecast_newmoon)

#Get only relevent columns now that this is isn't needed to subset weather.
rodent_data$all = rodent_data$all %>%
  select(-newmoondate,-censusdate,-period,-year,-month)

#Write data files
write.csv(rodent_data$all,"tools/rodent_all.csv",row.names = FALSE)
write.csv(rodent_data$controls,"tools/rodent_controls.csv",row.names = FALSE)
write.csv(weather_data,"tools/weather_data.csv",row.names = FALSE)

#Write YAML
writeLines(as.yaml(list(filename_suffix = 'forecasts',forecast_date = forecast_date, forecast_newmoons = forecast_newmoons, 
                  forecast_months = forecast_months, forecast_years = forecast_years)),con = "tools/model.yaml")
