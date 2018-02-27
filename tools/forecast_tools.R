library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)
library(rmarkdown)
library(RCurl)

##########################Forecast processing############################
#' Combine all new forecasts (from the tmp directory), add ensembles
#' 
#' @param forecast_date
#' @param filename_suffix
#' @return list(forecasts,all_model_aic)
#' @example forecastall('forecasts')

forecastall <- function(forecast_date,filename_suffix = 'forecasts') {
  
  #Append results to forecasts and AIC tables
  forecasts = do.call(rbind,
                      lapply(list.files("tmp",pattern = paste(filename_suffix, ".csv",sep=""), full.names = TRUE), 
                             read.csv, na.strings = "", colClasses = c("Date", "integer", "integer", 
                                                                       "integer", "character", "character", "character", 
                                                                       "character", "numeric", "numeric", "numeric",
                                                                       "integer", "integer", "integer")))
  
  all_model_aic = do.call(rbind,
                          lapply(list.files("tmp",pattern = paste(filename_suffix, "_model_aic.csv",sep=""), full.names = TRUE), 
                                 read.csv, na.strings = ""))
  
  forecast_filename = file.path('predictions', paste(as.character(forecast_date), filename_suffix, ".csv", sep=""))
  model_aic_filename = file.path('predictions', paste(as.character(forecast_date), filename_suffix, "_model_aic.csv", sep=""))
  append_csv(forecasts, forecast_filename)
  append_csv(all_model_aic, model_aic_filename)
  
  ########Add ensembles to files############################################
  ensemble=make_ensemble(forecasts) %>% 
    subset(select=colnames(forecasts))
  append_csv(ensemble, forecast_filename)
  
  return(list(forecasts,all_model_aic))
}

######Tools for writing forecasts to file and aics to separate file###############
#Appending a csv without re-writing the header.
append_csv=function(df, filename){
  write.table(df, filename, sep = ',', row.names = FALSE, col.names = !file.exists(filename), append = file.exists(filename))
}

#Get all model aic values and calculate akaike weights
compile_aic_weights = function(forecast_folder='./predictions'){
  model_aic_filenames = list.files(forecast_folder, full.names = TRUE, recursive = TRUE)
  model_aic_filenames = model_aic_filenames[grepl('model_aic',model_aic_filenames)]

  all_model_aic = purrr::map(model_aic_filenames, ~read.csv(.x, na.strings = '', stringsAsFactors = FALSE)) %>% 
    bind_rows()

  all_weights = all_model_aic %>%
    group_by(date,currency, level, species, fit_start_newmoon, fit_end_newmoon, initial_newmoon) %>%
    mutate(delta_aic = aic-min(aic), weight = exp(-0.5*delta_aic) / sum(exp(-0.5*delta_aic))) %>%
    ungroup()
  return(all_weights)
}

#Create the ensemble model from all other forecasts
#Uses the weighted mean and weighted sample variance
#https://en.wikipedia.org/wiki/Weighted_arithmetic_mean
make_ensemble=function(all_forecasts, models_to_use=NA, CI_level = 0.9){
  weights = compile_aic_weights()
  weights$date=as.Date(weights$date)
  CI_quantile = qnorm((1-CI_level)/2, lower.tail = FALSE)

  #Mean is the weighted mean of all model means.
  #Variance is the weighted mean of all model variances + the variances of the weighted mean 
  #using the unbiased estimate of sample variance. See https://github.com/weecology/portalPredictions/pull/65
  #We only store the prediction interval for models, so backcalculate individual model variance
  #assuming the same CI_level throughout. 
  weighted_estimates = all_forecasts %>%
    mutate(model_var = ((UpperPI - estimate)/CI_quantile)^2) %>%
    left_join(weights, by=c('date','model','currency','level','species','fit_start_newmoon','fit_end_newmoon','initial_newmoon')) %>%
    group_by(date, newmoonnumber, forecastmonth, forecastyear,level, currency, species, fit_start_newmoon, fit_end_newmoon, initial_newmoon) %>%
    summarise(ensemble_estimate = sum(estimate*weight), 
              weighted_ss = sum(weight * (estimate - ensemble_estimate)^2) ,
              ensemble_var   = sum(model_var * weight) + weighted_ss / (n()*sum(weight)-1),
              sum_weight = sum(weight)) %>% #round because the numbers get very very small
    ungroup() 
              
  #Assert that the summed weight of all the model ensembles is 1, as that's what the above variance estimates assume.
  #Rounded to account for precision errors. Summed weights can also be NA if there are not weights availble for that ensemble. 
  if(!all(round(weighted_estimates$sum_weight, 10) == 1 | is.na(weighted_estimates$sum_weight))){ stop('Summed weights do not equal 1')}

  ensemble = weighted_estimates %>%
    mutate(LowerPI = ensemble_estimate - (sqrt(ensemble_var) * CI_quantile),
           UpperPI = ensemble_estimate + (sqrt(ensemble_var) * CI_quantile)) %>%
    mutate(LowerPI = ifelse(LowerPI<0, 0, LowerPI)) %>%
    rename(estimate = ensemble_estimate) %>%
    select(-ensemble_var, -weighted_ss, -sum_weight)
  
  ensemble$model='Ensemble'
  return(ensemble)
}

##########Tools for forecast presentation##############

#' 
#' @param data
#' @param lvl
#' @param lead_time
get_sp_predicts = function(data, lvl, lead_time) {
  data = transform(data, forecast_date = as.yearmon(paste(forecastmonth, "/", forecastyear, sep =
                                                            ""), format = "%m/%Y")) %>% transform(date = as.Date(date, "%Y-%m-%d"))
  data1 = filter(data, level == lvl,
                 date == max(as.Date(date)))
  target_moon = min(data1$newmoonnumber) + (lead_time - 1)
  data2 = filter(data1, newmoonnumber == target_moon)
}

#' this is the 'second plot on the 'Species-level Forecast' on the 'Current Forecast' page on the website
#' 
#' 
#' @param data
#' @param title main title for plot
#' @return sp_predict is a plot object -- plot(sp_predict) displays it
#' 
plot_species_forecast = function(data,title) {
  newmoons_table = read.csv(
    text = getURL(
      "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"))
  target_moon=unique(data$newmoonnumber)
  period_code = dplyr::filter(newmoons_table, newmoons_table$newmoonnumber == target_moon) %>%
    dplyr::select(period) %>%
    as.integer()
  species_table = read.csv(
    text = getURL(
      "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv"),stringsAsFactors = F,na.strings = '')
  species_names = species_table %>% 
    select('speciescode','scientificname') %>% 
    rbind(c('total','total')) %>%
    merge(data[,c('species','estimate')],by.x='speciescode',by.y='species')
  
  sp_predict = ggplot(data,
                      aes(
                        x = estimate,
                        y = reorder(species, estimate),
                        xmin = LowerPI,
                        xmax = UpperPI
                      )) +
    geom_point() +
    geom_errorbarh() +
    ggtitle(title) + 
    ylab("Species") +
    xlab("Abundance") +
    scale_y_discrete(breaks = reorder(data$species,data$estimate),labels = reorder(species_names$scientificname,species_names$estimate))
  
  return(sp_predict)
}

#' Compares forecasts to observations over different lead times.
#' Error can be any function. The level, species, and currency columns from
#' observations and forecasts must have matching values. Note this gives an average
#' error value over many forecast iterations.
#'
#' Will only return values where there are matching comparison columns (currency, level, species)
#'
#' @param observations dataframe Has the columns newmoonnumber, currency, level, species, actual
#' @param forecasts dataframe passes the forecast validity check. Must have matching values in
#'                  the comparison columns
#' @param error_metric chr either 'RMSE' for root mean squared error or 'coverage' for the coverage of the prediction
#' intervals
#' @param ci_value int The value of the forecast confidence interval to scale PI values for the likelihood metric
#' @return data.frame Data.frame with the columns model, error, lead_time, level, species, currency
#'
calculate_forecast_error = function(observations, forecasts, error_metric='RMSE', CI_level=0.9){
  #The tibble datatype output from dplyr causes issues here
  observations = as.data.frame(observations)
  forecasts = as.data.frame(forecasts)

  if(!forecast_is_valid(forecasts)) stop('Forecast dataframe not valid')

  valid_observation_columns = c('newmoonnumber','currency','level','species','actual')
  if(!all(valid_observation_columns %in% colnames(observations))) stop('observation data.frame does not have valid column names')

  #At least 1 matching value must be in each of these columns in the observations and forecasts
  #TODO: Ensure matching rows in all 3 columns at once instead of just one at a time.
  column_check=c()
  for(column in c('currency','level','species')){
    if(!any(unique(observations[,column]) %in% unique(forecasts[,column]))) column_check=c(column_check, column)
  }
  if(length(column_check)>0) stop(paste('Comparison columns do not match: ',column_check, collaps=' '))

  #Summarize to mean error by lead time. Lead time is number of new moons ahead of when the forecast was made.
  #This assumes a forecast was made with only the data available prior to the first NewMoonDate in the series.
  #TODO: Make the lead time the actual days or weeks once more frequent forecasts are being made( see #37)
  forecasts = forecasts %>%
    mutate(lead_time = newmoonnumber - initial_newmoon)
  
  #Calculate error
  if(error_metric == 'RMSE'){
    comparisons = forecasts %>%
      inner_join(observations, by=c('newmoonnumber','currency','level','species')) %>%
      mutate(error_value=(estimate-actual)^2) %>%
      group_by(model, currency, level, species, lead_time) %>%
      summarize(error_value=sqrt(mean(error_value))) %>%
      ungroup() %>%
      mutate(error_metric = 'RMSE')

  } else if(error_metric == 'coverage') {
    comparisons = forecasts %>%
      inner_join(observations, by=c('newmoonnumber','currency','level','species')) %>%
      mutate(within_prediction_interval = actual >= LowerPI & actual <= UpperPI, error_metric='coverage') %>%
      group_by(model, currency, level, species, lead_time, error_metric) %>%
      summarize(error_value=mean(within_prediction_interval)) %>%
      ungroup() %>%
      mutate(error_metric = 'coverage')
    
  } else if(error_metric == 'deviance') {
    stop('Deviance not implimented  yet')
  } else {
    stop(paste0('Error metric unknown: ',error_metric))
  }

  return(comparisons)
}

#' Plot the output of calculate_forecast_error(). Lead time on the x-axis,
#' error on the y-axis, different colored lines are different models.
#'
#' @param error_df data.frame The output from calculate_foreast_error()
#' @param level str Valid level
#' @param species str Valid species
#' @param currency str Valid currency
#' @param error_metric str error metric used
plot_lead_time_errors=function(error_df, level, species, currency, error_metric){
  plot_title = paste0('Level: ',level,', Species: ',species,', Currency: ',currency)

  graph = ggplot(error_df, aes(x=lead_time, y=error, group=model, color=model)) +
            geom_point()+
            geom_line() +
            labs(y=error_metric,x='Lead Time (New Moons)', title=plot_title)
  plot(graph)
}

#' Ensure that a forecast file is in the correct format
#'
#' Tools for working with forecast data expect a certain format.
#' This ensures a forecast file meets those formats. All column
#' and variable names are case sensitive. For specification see:
#' https://github.com/weecology/portalPredictions/wiki/forecast-file-format
#'
#' @param forecast_df dataframe A dataframe read from a raw forecast file
#' @param verbose boolean Output warnings of specific violations
#' @return boolean

forecast_is_valid=function(forecast_df, verbose=FALSE){
  is_valid=TRUE
  violations=c()
  #Define valid valeus
  valid_columns = c('date','forecastmonth','forecastyear','newmoonnumber','model','currency',
                    'level','species','estimate','LowerPI','UpperPI','fit_start_newmoon',
                    'fit_end_newmoon','initial_newmoon')
  valid_currencies = c('abundance','richness','biomass','energy')
  valid_levels = paste('Plot',1:24,' ', sep = '')
  valid_levels = c('All','Controls','FullExclosure','KratExclosure', valid_levels)
  valid_species = c('total','BA','DM','DO','DS','OL','OT','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO','SF','SH','SO','NA')

  #Colnames should match exactly, case and everything, no more, no less.
  #The rest of the check depend on valid column names, so bail out early
  #if this does not pass.
  if(!(all(colnames(forecast_df) %in% valid_columns) & all(valid_columns %in% colnames(forecast_df)))){
    if(verbose) print('Forecast file column names invalid')
    return(FALSE)
  }

  #date must be in the format YYYY-MM-DD. as.Date() will return NA if it
  #doesn't match this exactly.
  #TODO: Account for dates that are formatted correctly but potentially many years off.
  forecast_df$date = base::as.Date(forecast_df$date, '%Y-%m-%d')
  if(any(is.na(forecast_df$date))) { is_valid=FALSE; violations = c('date', violations) }

  #All of these must be ones listed in the forecast format wiki.
  if(!all(unique(forecast_df$currency) %in% valid_currencies)) { is_valid=FALSE; violations = c('currency', violations) }
  if(!all(unique(forecast_df$level) %in% valid_levels)) { is_valid=FALSE; violations = c('level', violations) }
  if(!all(unique(forecast_df$species) %in% valid_species)) { is_valid=FALSE; violations = c('species', violations) }

  #Estimates and PI's cannot have NA values
  if(any(is.na(forecast_df$estimate))) { is_valid=FALSE; violations = c('NA esimates', violations) }
  if(any(is.na(forecast_df$LowerPI))) { is_valid=FALSE; violations = c('NA LowerPI', violations) }
  if(any(is.na(forecast_df$UpperPI))) { is_valid=FALSE; violations = c('NA UpperPI', violations) }

  #All the newmoon columns should be whole numbers with nothing missing
  if(!is.integer(forecast_df$fit_start_newmoon)) { is_valid=FALSE; violations = c('fit_start_newmoon not int')}
  if(!is.integer(forecast_df$fit_end_newmoon)) { is_valid=FALSE; violations = c('fit_end_newmoon not int')}
  if(!is.integer(forecast_df$initial_newmoon)) { is_valid=FALSE; violations = c('initial_newmoon not int')}
  if(any(is.na(forecast_df$fit_start_newmoon))) { is_valid=FALSE; violations = c('fit_start_newmoon contains NA')}
  if(any(is.na(forecast_df$fit_end_newmoon))) { is_valid=FALSE; violations = c('fit_end_newmoon contains NA')}
  if(any(is.na(forecast_df$initial_newmoon))) { is_valid=FALSE; violations = c('initial_newmoon contains NA')}
  
  if(verbose & length(violations)>0) print(paste('Forecast validation failed: ', violations), sep='')
  return(is_valid)
}


#' Collect all separate forecasts file into a single dataframe.
#'
#' The base folder can include subfolders.
#' Will only include files which pass validation. Will issue
#' warnings if a file isn't valid. If verbose is True it will
#' print specifics about the validation.
#'
#' @param forecast_folder str Base folder holding all forecast files
#' @param verbose bool Output info on file violations
#' @return dataframe combined forecasts
compile_forecasts=function(forecast_folder='./predictions', verbose=FALSE, use_hindcasts=FALSE){
  if(use_hindcasts){
    search_string = 'hindcast'
  } else {
    search_string = 'forecast'
  }
  
  forecast_filenames = list.files(forecast_folder, pattern = search_string, full.names = TRUE, recursive = TRUE)
  #aic_weight files are also stored here and there can be many. 
  forecast_filenames = forecast_filenames[!grepl('model_aic',forecast_filenames)]
  all_forecasts=data.frame()

  for(this_forecast_file in forecast_filenames){
    this_forecast_data = try(read.csv(this_forecast_file, na.strings = '', stringsAsFactors = FALSE))
    if(class(this_forecast_data) %in% 'try-error'){
      if(verbose){
        print(paste('File not readable: ',this_forecast_file, sep=''))
      } else {
        warning(paste('File not readable: ',this_forecast_file, sep=''))
      }
      next
    }

    if(verbose) print(paste('Testing file ',this_forecast_file,sep=''))
    if(forecast_is_valid(this_forecast_data, verbose=verbose)){
      if(verbose) {
        print(paste('File format is valid: ', this_forecast_file, sep=''))
        print('-------')
      }
      all_forecasts = all_forecasts %>%
        dplyr::bind_rows(this_forecast_data)
    } else {
      if(verbose){
        print(paste('File format not valid: ', this_forecast_file, sep=''))
        print('-------')
      } else{
        warning(paste('File format not valid: ', this_forecast_file, sep=''))
      }
    }
  }
all_forecasts$date=as.Date(all_forecasts$date)
  return(all_forecasts)
}

#' Visualize a time-series forecast
#' Plots the observed time-series and the 1-step forecasts within it
#' Plots the forecast time-series along with the prediction interval for future observations
#' @param obs_data is a data.frame (observed data)
#' @param obs_date_col_name is a string: name of the date column from obs_data
#' @param obs_val_col_name is a string: name of the column of the value being forecast
#' @param for_data is a data.frame (forecast data)
#' @param for_date_col_name is a string: name of the date column from for_data
#' @param for_val_col_name is a string: name of the column of value being forecast, from for_data
#' @param for_model_name is a string: name of the model to be used from model column in for_data
#' @param for_lowerpi_col_name is a string: name of the column of the lower confidence interval from for_data
#' @param for_upperpi_col_name is a string: name of the column of the upper confidence interval from for_data
#' @param start_newmoon is numeric: first new moon number to be plotted
#' @param ylabel is a string: title for y-axis
forecast_viz <- function(obs_data, obs_date_col_name, obs_val_col_name, for_data,
                         for_date_col_name, for_val_col_name, for_model_name,
                         for_lowerpi_col_name, for_upperpi_col_name, start_newmoon,
                         ylabel){
  for_data_sub = filter(for_data, species == obs_val_col_name, model == for_model_name)
  obs_data_sub = filter(obs_data, newmoonnumber >= start_newmoon)
  ggplot(obs_data_sub, aes_string(x = obs_date_col_name)) +
    geom_ribbon(data = for_data_sub, mapping = aes_string(x = for_date_col_name, ymin = for_lowerpi_col_name, ymax = for_upperpi_col_name), fill = "lightblue") +
    geom_line(aes_string(y = obs_val_col_name)) +
    geom_line(data = for_data_sub, mapping = aes_string(x = for_date_col_name, y = for_val_col_name), color = "blue") +
    labs(x='',y=ylabel)
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
    select(-date, -lat, -lon)
  
  # F to C and inches to mm
  df$temperature = (df$temperature - 32) * 5 / 9
  df$precipitation = df$precipitation * 25.4
  
  return(df)
  
}
