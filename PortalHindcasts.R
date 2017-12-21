source('tools/forecast_tools.R')
source('tools/model_functions.R')

filename_suffix = 'hindcasts'

#The date this hindcast is run. Always today's date.
forecast_date = Sys.Date()


#Hindcast will set the time based on these NewMoonNumbers. For each one
#a hindcast will be made which pretends that sampling period had just happened. 
#403 to 490 is Jan,2010 - Jan,2017. 
initial_time_newmoons=490:403

#Get the most recent data loaded into the data folder
portalr::download_observations()
moons = get_moon_data()
rodent_data = get_rodent_data(moons, forecast_date)
weather_data = get_weather_data(moons, rodent_data$all, lag=6)

write.csv(rodent_data$all,"data/rodent_all.csv",row.names = FALSE)
write.csv(rodent_data$controls,"data/rodent_controls.csv",row.names = FALSE)
write.csv(weather_data,"data/weather_data.csv",row.names = FALSE)

trappings = read.csv(FullPath('PortalData/Rodents/Portal_rodent_trapping.csv', '~'))
incomplete_samples = portalr::find_incomplete_censuses(trappings)

for(this_newmoon in initial_time_newmoons){

  moons = get_moon_data()
  
  this_newmoon_sampling_date = moons %>%
    filter(newmoonnumber == this_newmoon) %>%
    pull(censusdate)
  
  this_newmoon_period = moons %>%
    filter(newmoonnumber == this_newmoon) %>%
    pull(period)
  
  #Don't do hindcasting from newmoons that had incomplete samplings
  #or were not sampled at all
  if(this_newmoon_period %in% incomplete_samples$period | is.na(this_newmoon_sampling_date)){
    next
  }
  
  ####Setup data for hindcasting####

  moons = get_moon_data() %>%
    filter(newmoonnumber<=this_newmoon)
  #get dates of 12 new moons following newmoon of interest
  future_moons = get_future_moons(moons)
  
  #Beginning and end of the forecast timeperiod
  most_recent_newmoon = moons$newmoonnumber[which.max(moons$period)]
  most_recent_newmoon_date = as.Date(moons$newmoondate[which.max(moons$period)])
  first_forecast_newmoon=most_recent_newmoon+1
  last_forecast_newmoon=first_forecast_newmoon + 11
  forecast_newmoons = first_forecast_newmoon:last_forecast_newmoon
  forecast_months = future_moons$month[future_moons$newmoonnumber %in% forecast_newmoons]
  forecast_years = future_moons$year[future_moons$newmoonnumber %in% forecast_newmoons]
  
  all = read.csv("data/rodent_all.csv") %>%
    filter(newmoonnumber <= this_newmoon)
  
  controls = read.csv("data/rodent_controls.csv") %>%
    filter(newmoonnumber <= this_newmoon)
  
  weather_data = read.csv("data/weather_data.csv") %>%
    filter(newmoonnumber <= this_newmoon)
  
  #Update files in tools directory to use in this specific hindcast
  #Write data files
  write.csv(all,"data/rodent_all.csv",row.names = FALSE)
  write.csv(controls,"data/rodent_controls.csv",row.names = FALSE)
  write.csv(weather_data,"data/weather_data.csv",row.names = FALSE)
  
  #Write YAML
  writeLines(yaml::as.yaml(list(filename_suffix = filename_suffix,forecast_date = as.character(forecast_date), forecast_newmoons = forecast_newmoons, 
                                forecast_months = forecast_months, forecast_years = forecast_years)),con = "data/model_metadata.yaml")
  
  #####Run all models########################  
  cat("Running models", "\n")
  dir.create("tmp")
  sapply( list.files("models", full.names=TRUE), source )
  ####Compile all hindcasts into one file
  allhindcasts=forecastall(forecast_date, filename_suffix)
  unlink("tmp/*")
}
