library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)

#' Return normalized path for all operating systems
#'
#' @param ReferencePath a path to join with current working directory
#' @param BasePath Current working directory else path given
#'
#' @return
#' @export
#' @examples
#' FullPath('PortalData/Rodents/Portal_rodent.csv')
#' FullPath('PortalData/Rodents/Portal_rodent.csv', '~')
FullPath <- function( ReferencePath, BasePath=getwd()){
  BasePath = normalizePath(BasePath)
  Path = normalizePath(file.path(BasePath, ReferencePath), mustWork = FALSE)
  return (Path)
}

#Get all model aic values and calculate akaike weights
compile_aic_weights = function(forecast_folder='./predictions'){
  model_aic_filenames = list.files(forecast_folder, full.names = TRUE, recursive = TRUE)
  model_aic_filenames = model_aic_filenames[grepl('model_aic',model_aic_filenames)]

  all_model_aic = purrr::map(model_aic_filenames, ~read.csv(.x, na.strings = '', stringsAsFactors = FALSE)) %>% 
    bind_rows()

  all_weights = all_model_aic %>%
    group_by(date,currency, level, species) %>%
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
    left_join(weights, by=c('date','model','currency','level','species')) %>%
    group_by(date, NewMoonNumber, forecastmonth, forecastyear,level, currency, species) %>%
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

get_sp_predicts = function(data, lvl, lead_time) {
  data = transform(data, forecast_date = as.yearmon(paste(forecastmonth, "/", forecastyear, sep =
                                                            ""), format = "%m/%Y")) %>% transform(date = as.Date(date, "%Y-%m-%d"))
  data1 = filter(data, level == lvl,
                 date == max(as.Date(date)))
  target_moon = min(data1$NewMoonNumber) + (lead_time - 1)
  data2 = filter(data1, NewMoonNumber == target_moon)
}

plot_data = function(data) {
  newmoons_table = read.csv(
    text = getURL(
      "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"))
  target_moon=unique(data$NewMoonNumber)
  period_code = dplyr::filter(newmoons_table, newmoons_table$NewMoonNumber == target_moon) %>%
    dplyr::select(Period) %>%
    as.integer()
  sp_predict = ggplot(data,
                      aes(
                        x = estimate,
                        y = reorder(species, estimate),
                        xmin = LowerPI,
                        xmax = UpperPI
                      )) +
    geom_point() +
    geom_errorbarh() +
    ggtitle(paste(data$forecast_date[2], "All plots", sep = " ")) + # should make title better somehow
    ylab("Species") +
    xlab("Abundance")
  if(!is.na(period_code)){
    rodents = abundance("repo", shape='flat')
    observed = dplyr::filter(rodents, period == period_code)
    joined_data = left_join(data, observed, by = "species")
    joined_data[is.na(joined_data)] = 0
    joined_data[joined_data$species=='total','abundance'] = sum(joined_data$abundance,na.rm=T)
    joined_data[joined_data$species=='total','period'] = period_code
    sp_predict = sp_predict +
      geom_point(data = joined_data, mapping = aes(x = abundance, y = species),
                 color = "blue")
  }
  plot(sp_predict)
}


#' Compares forecasts to observations over different lead times.
#' Error can be any function. The level, species, and currency columns from
#' observations and forecasts must have matching values.
#'
#' Will only return values where there are matching comparison columns (currency, level, species)
#'
#' @param observations dataframe Has the columns NewMoonNumber, currency, level, species, actual
#' @param forecasts dataframe passes the forecast validity check. Must have matching values in
#'                  the comparison columns
#' @param error_metric chr either 'mse' for mean squared error or 'likelihood' for the likelihood of the observation
#' assuming the estimate and PI's fit a normal distribution
#' @param ci_value int The value of the forecast confidence interval to scale PI values for the likelihood metric
#' @return data.frame Data.frame with the columns model, error, lead_time, level, species, currency
#'
calculate_forecast_error = function(observations, forecasts, error_metric='MSE', ci_value=90){
  #The tibble datatype output from dplyr causes issues here
  observations = as.data.frame(observations)
  forecasts = as.data.frame(forecasts)

  if(!forecast_is_valid(forecasts)) stop('Forecast dataframe not valid')

  valid_observation_columns = c('NewMoonNumber','currency','level','species','actual')
  if(!all(valid_observation_columns %in% colnames(observations))) stop('observation data.frame does not have valid column names')

  #At least 1 matching value must be in each of these columns in the observations and forecasts
  #TODO: Ensure matching rows in all 3 columns at once instead of just one at a time.
  column_check=c()
  for(column in c('currency','level','species')){
    if(!any(unique(observations[,column]) %in% unique(forecasts[,column]))) column_check=c(column_check, column)
  }
  if(length(column_check)>0) stop(paste('Comparison columns do not match: ',column_check, collaps=' '))

  #Calculate error
  if(error_metric == 'MSE'){
    comparisons = forecasts %>%
      inner_join(observations, by=c('NewMoonNumber','currency','level','species')) %>%
      group_by(date, model, NewMoonNumber, currency, level, species) %>%
      summarise(error=(estimate-actual)^2) %>%
      ungroup()
  } else if(error_metric == 'Likelihood') {
    stop('Likelihood not implimented yet')
  } else {
    stop(paste0('Error metric unknown: ',error_metric))
  }

  #Summarize to mean error by lead time. Lead time is number of new moons ahead of when the forecast was made.
  #This assumes a forecast was made with only the data available prior to the first NewMoonDate in the series.
  #TODO: Make the lead time the actual days or weeks once more frequent forecasts are being made( see #37)
  forecast_date_new_moon_number = comparisons %>%
    group_by(date) %>%
    summarise(new_moon_of_forecast = min(NewMoonNumber)-1) %>%
    ungroup()

  comparisons_with_lead_time = comparisons %>%
    left_join(forecast_date_new_moon_number, by='date') %>%
    mutate(lead_time=NewMoonNumber - new_moon_of_forecast) %>%
    select(-new_moon_of_forecast, -NewMoonNumber, -date)

  comparisons_model_summary = comparisons_with_lead_time %>%
    group_by(model, currency, level, species, lead_time) %>%
    summarize(error=mean(error))

  return(comparisons_model_summary)
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
  valid_columns = c('date','forecastmonth','forecastyear','NewMoonNumber','model','currency',
                    'level','species','estimate','LowerPI','UpperPI')
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
compile_forecasts=function(forecast_folder='./predictions', verbose=FALSE){
  forecast_filenames = list.files(forecast_folder, full.names = TRUE, recursive = TRUE)
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
#' obs_data is a data.frame
#' date_col_name is a string with the name for the date column
#' val_col_name is a string with the name for the column of the value being forecast
forecast_viz <- function(obs_data, obs_date_col_name, obs_val_col_name, for_data,
                         for_date_col_name, for_val_col_name, for_model_name,
                         for_lowerpi_col_name, for_upperpi_col_name, start_newmoon){
  for_data_sub = filter(for_data, species == obs_val_col_name, model == for_model_name)
  obs_data_sub = filter(obs_data, NewMoonNumber >= start_newmoon)
  ggplot(obs_data_sub, aes_string(x = obs_date_col_name)) +
    geom_ribbon(data = for_data_sub, mapping = aes_string(x = for_date_col_name, ymin = for_lowerpi_col_name, ymax = for_upperpi_col_name), fill = "lightblue") +
    geom_line(aes_string(y = obs_val_col_name)) +
    geom_line(data = for_data_sub, mapping = aes_string(x = for_date_col_name, y = for_val_col_name), color = "blue")
}
