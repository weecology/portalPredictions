'%>%' <- magrittr::"%>%"
library(yaml)
library(portalr)
source('tools/data_tools.R')

filename_suffix = 'forecasts'

#The date this forecast is run. Always today's date.
forecast_date = Sys.Date()

portalr::download_observations(release_only = FALSE)
moons = get_moon_data()
current_moons <- select(moons, newmoonnumber, newmoondate, period, censusdate)
current_moons$newmoondate <- as.Date(as.character(current_moons$newmoondate))
#get dates of 12 new moons following newmoon of interest
future_moons <- get_future_moons(moons)
total_moons <- rbind(current_moons, future_moons)


rodent_data = get_rodent_data(moons, forecast_date)
weather_data = weather("newmoon",fill=TRUE) %>% dplyr::ungroup() %>%
                dplyr::select("mintemp","maxtemp","meantemp","precipitation","newmoonnumber")
incompletes <- which(is.na(weather_data$newmoonnumber))
if(length(incompletes) > 0){
  weather_data <- weather_data[-incompletes, ]
}
ndvi_data <- ndvi("newmoon", fill = TRUE)

# Beginning and end of the forecast timeperiod
most_recent_newmoon <- moons$newmoonnumber[max(which(total_moons$newmoondate < forecast_date))]
most_recent_rodent_period <- max(tail(rodent_data$all, 1)$period, tail(rodent_data$control, 1)$period)
most_recent_rodent_newmoon <- moons$newmoonnumber[which(moons$period == most_recent_rodent_period)]
most_recent_covariate_newmoon <- min(tail(weather_data, 1)$newmoonnumber, tail(ndvi_data, 1)$newmoonnumber)
last_forecast_newmoon <- most_recent_newmoon + 12
first_forecast_covariate_newmoon <- most_recent_covariate_newmoon + 1
first_forecast_rodent_newmoon <- most_recent_rodent_newmoon + 1
covariate_forecast_newmoons <- first_forecast_covariate_newmoon:last_forecast_newmoon
rodent_forecast_newmoons <- first_forecast_rodent_newmoon:last_forecast_newmoon
covariate_nm_dates <- total_moons$newmoondate[total_moons$newmoonnumber %in% covariate_forecast_newmoons]
rodent_nm_dates <- total_moons$newmoondate[total_moons$newmoonnumber %in% rodent_forecast_newmoons]
covariate_forecast_months <- as.numeric(format(covariate_nm_dates, "%m"))
covariate_forecast_years <- as.numeric(format(covariate_nm_dates, "%Y"))
rodent_forecast_months <- as.numeric(format(rodent_nm_dates, "%m"))
rodent_forecast_years <- as.numeric(format(rodent_nm_dates, "%Y"))

#Write data files
write.csv(rodent_data$all,"data/rodent_all.csv",row.names = FALSE)
write.csv(rodent_data$controls,"data/rodent_controls.csv",row.names = FALSE)
write.csv(weather_data,"data/weather_data.csv",row.names = FALSE)
write.csv(moons, "data/moon_data.csv", row.names = FALSE)
write.csv(ndvi_data, "data/ndvi_data.csv", row.names = FALSE)

#Write YAML
writeLines(as.yaml(list(filename_suffix = filename_suffix, 
                        forecast_date = as.character(forecast_date), 
                        covariate_forecast_newmoons = covariate_forecast_newmoons, 
                        covariate_forecast_months = covariate_forecast_months, 
                        covariate_forecast_years = covariate_forecast_years,
                        rodent_forecast_newmoons = rodent_forecast_newmoons, 
                        rodent_forecast_months = rodent_forecast_months, 
                        rodent_forecast_years = rodent_forecast_years)), con = "data/model_metadata.yaml")
