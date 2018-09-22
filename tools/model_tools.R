library(lubridate)
library(dplyr)
source('tools/data_tools.R')
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
#' Lag the covariate (weather or ndvi) data for use with analyses
#'
#' Lag the weather data together based on the new moons
#'
#' @param data dataframe of weather or ndvi data to be lagged
#' @param lag lag between census and weather data, in new moons
#' @param tail logical, if the data lagged to the tail end should be retained
#' @return a dataframe of weather data with newmoonnumber now reflecting the
#'  lag
#'
#' @examples
#' lag_data(weather, lag=6)
#'
lag_data <- function(data, lag, tail = FALSE){
  
  data$newmoonnumber_lag = data$newmoonnumber + lag
  
  if(tail == FALSE){
    oldest_included_newmoon <- data$newmoonnumber[1]
    most_recent_newmoon <- data$newmoonnumber[nrow(data)]
    hist_newmoons <- oldest_included_newmoon:most_recent_newmoon
    hist_moons_table <- data.frame(newmoonnumber = hist_newmoons)
    nm_match <- c('newmoonnumber_lag' = 'newmoonnumber')
    data <- right_join(data, hist_moons_table, by = nm_match) 
    data <- data[-(1:lag), ]
  }
  data <- select(data, -newmoonnumber)
  cn_data <- colnames(data)
  cn_nmn_l <- which(cn_data == "newmoonnumber_lag")
  colnames(data)[cn_nmn_l] <- "newmoonnumber"

  return(data)
}

####################################################################################
#' Download downscaled climate forecast data for a single location
#' 
#' Obtained from https://climate.northwestknowledge.net/RangelandForecast/download.php
#' 
#' @param climate_model Individual climate models available are 
#'                      c('CFSv2','CMC1','CMC2','GFDL-FLOR','GFDL','NASA','NCAR'),
#'                      'ENSMEAN' is the mean of all models.
#' @param lead_time the newmoons into the future to obtain forecasts. Max of 7
#' @param lat latitude Default is for Portal, AZ
#' @param lon longitude Default is for Portal, AZ
#' 
#' @return a data.frame with precipitation(mm), temperature(C), year, and month. Temperature is the mean temperature
#'         for the month, while precipitation is the total forecasted precip.
#'
get_climate_forecasts = function(climate_model = 'ENSMEAN', 
                                 lat = 31.9555, lon = -109.0744,
                                 lead_time = 6, moons){
  
  valid_models = c('CFSv2', 'CMC1', 'CMC2', 'GFDL-FLOR', 'GFDL', 'NASA', 'NCAR', 'ENSMEAN')
  
  if(!climate_model %in% valid_models){
    stop(paste0('Unknown climate model: ',climate_model))
  }
  if(!lead_time %in% 1:7){
    stop(paste0('Lead time must an integer be between 1 and 7, got: ',lead_time))
  }
  
  last_moon = tail(moons, 1)
  last_moon$newmoondate <- as.Date(as.character(last_moon$newmoondate))
  
  future_moons <- get_future_moons(moons, num_future_moons = lead_time)
  start_time = as.character(as.Date(strftime(last_moon$newmoondate, format='%Y-%m-%d')) + 1)
  end_time = strftime(future_moons$newmoondate[lead_time], format='%Y-%m-%d')
  days_for_forecast <- seq.Date(as.Date(start_time), as.Date(end_time), 1)
  
  # add in timestamps for URL
  start_time = paste0(start_time,'T00%3A00%3A00Z')
  end_time = paste0(end_time,'T00%3A00%3A00Z')
  
  base_url = 'https://tds-proxy.nkn.uidaho.edu/thredds/ncss/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/dailyForecasts/bcsd_nmme_metdata_'
  type_urls = c("tasmin", "tasmean", "tasmax", "pr")
  full_urls = paste0(base_url, climate_model,'_forecast_', type_urls, '_daily.nc?var=', type_urls, 
                    '&latitude=', lat, '&longitude=', lon,
                    '&time_start=', start_time, '&time_end=', end_time,
                    '&accept=csv')
  raw_download = RCurl::getURL(full_urls)


  daily_forecasts <- data.frame(date = days_for_forecast)

  df1 = read.table(sep=',',skip=1,text=raw_download[1])
  colnames(df1) = c('date','lat','lon','mintemp')
  df1[(df1[,"mintemp"] == -9999), "mintemp"] <- NA
  df2 = read.table(sep=',',skip=1,text=raw_download[2])
  colnames(df2) = c('date','lat','lon','meantemp')
  df2[(df2[,"meantemp"] == -9999), "meantemp"] <- NA
  df3 = read.table(sep=',',skip=1,text=raw_download[3])
  colnames(df3) = c('date','lat','lon','maxtemp')
  df3[(df3[,"maxtemp"] == -9999), "maxtemp"] <- NA
  df4 = read.table(sep=',',skip=1,text=raw_download[4])
  colnames(df4) = c('date','lat','lon','precipitation')
  df4[(df4[,"precipitation"] == -9999), "precipitation"] <- NA
  df4$precipitation[which(df4$precipitation < 0)] <- 0
  df <- right_join(df1, df2) %>% right_join(df3) %>% right_join(df4)
  
  df = df %>% mutate(date = as_date(date)) %>% select(-lat, -lon)

  # F to C and inches to mm
  df$mintemp = (df$mintemp - 32) * 5 / 9
  df$maxtemp = (df$maxtemp - 32) * 5 / 9
  df$meantemp = (df$meantemp - 32) * 5 / 9
  df$precipitation = df$precipitation * 25.4

  daily_forecasts <- full_join(daily_forecasts, df)

  historic <- portalr::weather("daily", fill = TRUE)
  historic$date <- as.Date(paste(historic$year, historic$month, historic$day, sep = "-"))

  avail_historic <- which(daily_forecasts$date %in% historic$date)
  n_avail_historic <- length(avail_historic)

  if(n_avail_historic > 0){

    dates_avail <- daily_forecasts$date[avail_historic]
    in_historic <- which(historic$date %in% dates_avail)
    in_forecast <- which(daily_forecasts$date %in% dates_avail)
    daily_forecasts$mintemp[in_forecast] <- historic$mintemp[in_historic]
    daily_forecasts$meantemp[in_forecast] <- historic$meantemp[in_historic]
    daily_forecasts$maxtemp[in_forecast] <- historic$maxtemp[in_historic]
    daily_forecasts$precipitation[in_forecast] <- historic$precipitation[in_historic]
  }

  temp_moons <- rbind(last_moon, future_moons)
  newmoon_number <- temp_moons$newmoonnumber[-1]
  newmoon_start <- temp_moons$newmoondate[-(1 + lead_time)] 
  newmoon_end <- temp_moons$newmoondate[-1]
  newmoon_match_number <- NULL
  newmoon_match_date <- NULL
  for(i in 1:lead_time){
    temp_dates <- as.character(seq.Date(newmoon_start[i] + 1, newmoon_end[i], 1))
    temp_numbers <- rep(newmoon_number[i], length(temp_dates))
    newmoon_match_date <- c(newmoon_match_date, temp_dates)
    newmoon_match_number <- c(newmoon_match_number, temp_numbers)   
  }  
  newmoon_match_date <- as.Date(newmoon_match_date)
  which_match <- match(daily_forecasts$date, newmoon_match_date)
  daily_forecasts$newmoonnumber <- newmoon_match_number[which_match]

  newmoon_forecasts <- daily_forecasts %>% group_by(newmoonnumber) %>%
         summarize(mintemp = min(mintemp, na.rm = T), maxtemp = max(maxtemp, na.rm = T), 
                   meantemp = mean(meantemp, na.rm = T), 
                   precipitation = sum(precipitation, na.rm = T))

  return(newmoon_forecasts)
}

####################################################################################
#' Build a weather forecast for model predictions by combining portal station data, 
#' climate forecasts, and historic means, impose specified lag
#' 
#' @param start end year of data and beginning of forecast
#' @param moons moon data
#' @param lag newmoons by which weather data is lagged
#' @param lead_time the number of newmoons into the future to obtain forecasts. 
#' Max of 7. lag + lead_time should equal 12
#' 
#' @return a data.frame with 12 new moons of weather values
#'
fcast_weather = function(start = as.numeric(format(Sys.Date(), "%Y")), moons, lag = 6, lead_time = 6){
  
  newweather <- portalr::weather("newmoon",fill=TRUE) %>%
    select(-c(locally_measured,battery_low)) %>% 
    mutate(year = as.numeric(format(date, "%Y"))) %>%
    filter(year>=start-5)
  incompletes <- which(is.na(newweather$newmoonnumber))
  if(length(incompletes) > 0 ){
    newweather <- newweather[-incompletes, ]  
  }

  weatherforecast <- tail(newweather,lag) %>% select(-year, -date) %>%
    bind_rows(get_climate_forecasts(lead_time = lead_time, moons = moons))

  return(weatherforecast)
}
