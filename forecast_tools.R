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

#Create the ensemble model from all other forecasts
#Currently just the mean of the esimates and confidence intervals.
make_ensemble=function(all_forecasts, model_weights=NA, models_to_use=NA){
  ensemble = all_forecasts %>%
    group_by(date, NewMoonNumber, forecastmonth, forecastyear,level, currency, species) %>%
    summarise(estimate = mean(estimate), LowerPI=mean(LowerPI), UpperPI=mean(UpperPI))
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

plot_data = function(data, lvl, observed = NULL) {
  if (!is.null(observed)){
    observed = filter(observed, level == lvl)
  }
  sp_predict = ggplot(data,
                       aes(
                         x = estimate,
                         y = reorder(species, estimate),
                         xmin = LowerPI,
                         xmax = UpperPI
                         )) +
    geom_point() +
    geom_errorbarh() +
    ggtitle(paste(data$forecast_date[2], lvl, sep = " ")) + # should make title better somehow
    ylab("Species") +
    xlab("Abundance")
  if (!is.null(observed)) {
    joined_data = left_join(data, observed, by = "species")
    joined_data[is.na(joined_data)] = 0
    sp_predict = sp_predict +
      geom_point(data = joined_data, mapping = aes(x = actual, y = species),
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


#' Collect all seperate forecasts file into a single dataframe.
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

  return(all_forecasts)
}

#' Download the PortalData repo
#'
#' This downloads the latest portal data regardless if they are
#' actually updated or not.
#'
#' TODO: incorperate data retriever into this when it's pointed at the github repo
#' @return None
download_observations = function(base_folder=normalizePath('~')){
  zip_download_path='https://github.com/weecology/PortalData/archive/master.zip'
  zip_download_dest=FullPath('PortalData.zip', base_folder)
  download.file(zip_download_path, zip_download_dest, quiet = TRUE)

  final_data_folder=FullPath('PortalData', base_folder)

  #Clear out the old files in the data folder without doing potentially dangerous
  #recursive deleting.
  if(file.exists(final_data_folder)) {
    old_files=list.files(final_data_folder, full.names = TRUE, all.files = TRUE, recursive = TRUE, include.dirs = FALSE)
    file.remove(old_files)
    old_dirs=list.files(final_data_folder, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
    file.remove(old_dirs)
  }

  #Github serves this up with the -master extension. Unzip and rename to remove that.
  unzip(zip_download_dest, exdir=base_folder)
  file.remove(zip_download_dest)
  file.rename(FullPath('PortalData-master',base_folder), final_data_folder)
}

#' Check if there are new rodent observations. This only checks the
#' Portal_rodent.csv file. If other things are updated this function
#' will not show that there is new data available.
#'
#' @return bool True if new observations are available
observations_are_new = function(base_folder=normalizePath('~')){
  md5_file = FullPath('Portal_rodent.md5')
  rodent_file = FullPath('PortalData/Rodents/Portal_rodent.csv', base_folder)
  if(!file.exists(rodent_file)) stop('Rodent observations not present. Please run download_observations()')

  if(!file.exists(md5_file)) {
    old_md5=''
  } else {
    old_md5 = read.csv(md5_file, header = FALSE, stringsAsFactors = FALSE)$V1
  }

  new_md5 = as.character(tools::md5sum(rodent_file))

  if(old_md5 == new_md5){
    return(FALSE)
  } else {
    sink(md5_file)
    writeLines(new_md5)
    sink()
    return(TRUE)
  }

}


