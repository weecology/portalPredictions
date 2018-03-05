'%>%' <- magrittr::"%>%"
library(yaml)
source('tools/data_tools.R')

filename_suffix = 'forecasts'

#The date this forecast is run. Always today's date.
forecast_date = Sys.Date()

portalr::download_observations(release_only = FALSE)
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
weather_data = weather("newmoon",fill=TRUE) %>% dplyr::ungroup() %>%
                dplyr::select("mintemp","maxtemp","meantemp","precipitation","newmoonnumber")
incompletes <- which(is.na(weather_data$newmoonnumber))
if(length(incompletes) > 0){
  weather_data <- weather_data[-incompletes, ]
}
ndvi_data <- summarize_ndvi("newmoon")

#Write data files
write.csv(rodent_data$all,"data/rodent_all.csv",row.names = FALSE)
write.csv(rodent_data$controls,"data/rodent_controls.csv",row.names = FALSE)
write.csv(weather_data,"data/weather_data.csv",row.names = FALSE)
write.csv(moons, "data/moon_data.csv", row.names = FALSE)
write.csv(ndvi_data, "data/ndvi_data.csv", row.names = FALSE)

#Write YAML
writeLines(as.yaml(list(filename_suffix = filename_suffix,forecast_date = as.character(forecast_date), forecast_newmoons = forecast_newmoons, 
                  forecast_months = forecast_months, forecast_years = forecast_years)),con = "data/model_metadata.yaml")
