library(dplyr)
#' Interpolate missing (rodent) abundance data
#' 
#' Interpolation of missing data in the rodent abundance data set. Each 
#'   species is individually linearly interpolated, then the total number of 
#'   of rodents is calculated from the sum of the individual species.
#'
#' @param abundance data table with new moon column
#' @return data table of interpolation-inclusive counts for each species and 
#'  total
#' 
interpolate_abundance <- function(abundance){

  moons <- (min(abundance$newmoonnumber)):(max(abundance$newmoonnumber))
  nmoons <- length(moons)

  species <- colnames(abundance)[2:(ncol(abundance) - 4)]
  nspecies <- length(species)

  abunds <- matrix(NA, nrow = nmoons, ncol = nspecies)

  for(i in 1:nmoons){
    if(length(which(abundance$newmoonnumber == moons[i])) > 0){
      temp <- abundance[which(abundance$newmoonnumber == moons[i]),
                        which(colnames(abundance) %in% species)]
      abunds[i, ] <- as.numeric(temp)
    }
  }

  interpolated_abunds <- abunds
  colnames(interpolated_abunds) <- species

  for(j in 1:nspecies){
    interpolated_abunds[ , j] <- round(na.interp(abunds[ , j]))
  }

  interpolated_total <- apply(interpolated_abunds, 1, sum)

  out <- data.frame(moons, interpolated_abunds, total = interpolated_total)

  return(out)
}

###################################################################################
#' Realign weather data to rodent data with lag
#'
#' Including a lag offsets the weather data (all variables together) to match with
#'  the rodent data
#'
#' @param weather dataframe of weather data to apply lag
#' @param all dataframe of rodent data used in forecasting
#' @param lag lag between rodent and weather data, in months
#' 
#' @return a dataframe of weather data with added NewMoonNumber_with_lag column
#' @examples
#' lag_weather_data(weather, all, lag=6)
#'
lag_weather_data <- function(weather, all, lag){
  
  #Offset the newmoonnumber to create a lag between
  #rodent observations and weather for use in environmental models (eg pevGARCH)
  #which_is_lagged is a matching variable used to impose the lag on the 
  # weather data
  weather_data$NewMoonNumber_with_lag = weather_data$newmoonnumber + lag
  
  which_is_lagged = rep(NA, nrow(weather_data))
  for(i in 1:nrow(weather_data)){
    specific <- which(weather_data$newmoonnumber == weather_data$NewMoonNumber_with_lag[i])
    if(length(specific) > 0){
      which_is_lagged[i] = specific
    }
  }
  weather_data$year_with_lag = weather_data$year[which_is_lagged]
  weather_data$month_with_lag = weather_data$month[which_is_lagged]
  
  #Assign weather using lag to rodent observations.
  #This will match weather row numbers to the corrosponding rows associated
  # with each new moon (regardless of census occurring or not)
  all_new_moons = min(all$newmoonnumber):max(all$newmoonnumber)
  all_moons_table = data.frame(newmoonnumber = all_new_moons)
  weather_data = weather_data %>%
    select(-year, -month) %>%
    right_join(all_moons_table, by=c('NewMoonNumber_with_lag'='newmoonnumber')) %>%
    select(year_with_lag,month_with_lag,mintemp,maxtemp,meantemp,precipitation,ndvi,newmoonnumber,NewMoonNumber_with_lag)
  
  colnames(weather_data)[1:2] <- c("year", "month")
  return(weather_data)
}

####################################################################################
#' Download downscaled climate forecast data for a single location
#' 
#' Obtained from https://climate.northwestknowledge.net/RangelandForecast/download.php
#' 
#' @param climate_model Individual climate models available are 
#'                      c('CFSv2','CMC1','CMC2','GFDL-FLOR','GFDL','NASA','NCAR'),
#'                      'ENSMEAN' is the mean of all models.
#' @param lead_time the months into the future to obtain forecasts. Max of 7
#' @param lat latitude Default is for Portal, AZ
#' @param lon longitude Default is for Portal, AZ
#' 
#' @return a data.frame with precipitation(mm), temperature(C), year, and month. Temperature is the mean temperature
#'         for the month, while precipitation is the total forecasted precip.

get_climate_forecasts = function(climate_model = 'ENSMEAN', 
                                 lat = 31.9555, lon = -109.0744,
                                 lead_time = 6){
  
  valid_models = c('CFSv2', 'CMC1', 'CMC2', 'GFDL-FLOR', 'GFDL', 'NASA', 'NCAR', 'ENSMEAN')
  
  if(!climate_model %in% valid_models){
    stop(paste0('Unknown climate model: ',climate_model))
  }
  if(!lead_time %in% 1:7){
    stop(paste0('Lead time must an integer be between 1 and 7, got: ',lead_time))
  }
  
  today = Sys.Date()
  start_time = strftime(today, format='%Y-%m-%d')
  end_time = strftime(today %m+% months(lead_time), format='%Y-%m-%d')
  
  # add in timestamps for URL
  start_time = paste0(start_time,'T00%3A00%3A00Z')
  end_time = paste0(end_time,'T00%3A00%3A00Z')
  
  base_url = 'https://tds-proxy.nkn.uidaho.edu/thredds/ncss/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/monthlyForecasts/bcsd_nmme_metdata_'
  
  full_url = paste0(base_url, climate_model,
                    '_forecast_1monthAverage.nc?var=prate&var=tmp2m',
                    '&latitude=', lat, '&longitude=', lon,
                    '&time_start=', start_time, '&time_end=', end_time,
                    '&accept=csv')
  
  raw_download = RCurl::getURL(full_url)
  
  df=read.table(sep=',',skip=1,text=raw_download)
  colnames(df) = c('date','lat','lon','precipitation','temperature')
  
  df = df %>%
    mutate(date = as_date(date)) %>%
    mutate(year = year(date), month=month(date)) %>%
    select(-date, -lat, -lon) %>%
    rename(meantemp = temperature)
  
  # F to C and inches to mm
  df$meantemp = (df$meantemp - 32) * 5 / 9
  df$precipitation = df$precipitation * 25.4
  
  return(df)
  
}

####################################################################################
#' Build a weather forecast for model predictions by combining portal station data, 
#' climate forecasts, and historic means, impose specified lag
#' 
#' @param start end year of data and beginning of forecast
#' @param lag months by which weather data is lagged
#' @param lead_time the months into the future to obtain forecasts. 
#' Max of 7. lag + lead_time should equal 12
#' 
#' @return a data.frame with 12 months of weather values

build_weather_forecast = function(start = as.numeric(format(Sys.Date(), "%Y")), lag = 6, lead_time = 6){
  
  newweather <- portalr::weather("Monthly",fill=TRUE) %>%
    select(-c(locally_measured,battery_low)) %>% 
    filter(year>=start-5)
  
  weatherforecast <- tail(newweather,lag) %>% 
    bind_rows(get_climate_forecasts(lead_time = lead_time)) %>%
    select(-year) 
  
  weathermeans <- newweather %>%
    group_by(month) %>% 
    summarise_all(funs(mean(., na.rm=TRUE))) %>%
    slice(match(weatherforecast$month,month)) %>%
    select(-year)
 
  weatherforecast$mintemp = coalesce(weatherforecast$mintemp,weathermeans$mintemp)
  weatherforecast$maxtemp = coalesce(weatherforecast$maxtemp,weathermeans$maxtemp)
  weatherforecast$meantemp = coalesce(weatherforecast$meantemp,weathermeans$meantemp)
  weatherforecast$precipitation = coalesce(weatherforecast$precipitation,weathermeans$precipitation)
  weatherforecast$ndvi = coalesce(weatherforecast$ndvi,weathermeans$ndvi)
  
  return(weatherforecast)
}
