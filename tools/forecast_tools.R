##########Tools for forecast presentation##############

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
  for_data_sub = dplyr::filter(for_data, species == obs_val_col_name, model == for_model_name)
  obs_data_sub = dplyr::filter(obs_data, newmoonnumber >= start_newmoon)
  ggplot2::ggplot(obs_data_sub, ggplot2::aes_string(x = obs_date_col_name)) +
    ggplot2::geom_ribbon(data = for_data_sub, 
                         mapping = ggplot2::aes_string(x = for_date_col_name, ymin = for_lowerpi_col_name, 
                                              ymax = for_upperpi_col_name), fill = "lightblue") +
    ggplot2::geom_line(ggplot2::aes_string(y = obs_val_col_name)) +
    ggplot2::geom_line(data = for_data_sub, 
                       mapping = ggplot2::aes_string(x = for_date_col_name, y = for_val_col_name), 
                       color = "blue") +
    ggplot2::labs(x='',y=ylabel)
}

#' Plot species-level forecasts
#' This is the 'second plot on the 'Species-level Forecast' on the 'Current Forecast' page on the website
#' 
#' 
#' @param data
#' @param title main title for plot
#' @return sp_predict is a plot object -- plot(sp_predict) displays it
#' 
plot_species_forecast <- function(data,title) {
  newmoons_table = read.csv('PortalData/Rodents/moon_dates.csv')
  target_moon=unique(data$newmoonnumber)
  period_code = dplyr::filter(newmoons_table, newmoons_table$newmoonnumber == target_moon) %>%
    dplyr::select(period) %>%
    as.integer()
  species_table = read.csv('PortalData/Rodents/Portal_rodent_species.csv',stringsAsFactors = F,na.strings = '')
  species_names = species_table %>% 
    dplyr::select('speciescode','scientificname') %>% 
    rbind(c('total','total')) %>%
    merge(data[,c('species','estimate')],by.x='speciescode',by.y='species')
  
  sp_predict = ggplot2::ggplot(data,
                               ggplot2::aes(
                                 x = estimate,
                                 y = reorder(species, estimate),
                                 xmin = LowerPI,
                                 xmax = UpperPI
                               )) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbarh() +
    ggplot2::ggtitle(title) + 
    ggplot2::ylab("Species") +
    ggplot2::xlab("Abundance") +
    ggplot2::scale_y_discrete(breaks = reorder(data$species,data$estimate),
                              labels = reorder(species_names$scientificname,species_names$estimate))
  
  return(sp_predict)
}

#' Get the most recent species-level predictions
#' 
#' @param data
#' @param lvl
#' @param lead_time
get_sp_predicts <- function(data, lvl, lead_time) {
  data = transform(data, forecast_date = zoo::as.yearmon(paste(forecastmonth, "/", forecastyear, 
                                                          sep = ""), format = "%m/%Y")) %>% 
    transform(date = as.Date(date, "%Y-%m-%d"))
  data1 = dplyr::filter(data, level == lvl,
                 date == max(as.Date(date)))
  target_moon = min(data1$newmoonnumber) + (lead_time - 1)
  data2 = dplyr::filter(data1, newmoonnumber == target_moon)
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
calculate_forecast_error <- function(observations, forecasts, error_metric='RMSE', CI_level=0.9){
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
    dplyr::mutate(lead_time = newmoonnumber - initial_newmoon)
  
  #Calculate error
  if(error_metric == 'RMSE'){
    comparisons = forecasts %>%
      dplyr::inner_join(observations, by=c('newmoonnumber','currency','level','species')) %>%
      dplyr::mutate(error_value=(estimate-actual)^2) %>%
      dplyr::group_by(model, currency, level, species, lead_time) %>%
      dplyr::summarize(error_value=sqrt(mean(error_value))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(error_metric = 'RMSE')

  } else if(error_metric == 'coverage') {
    comparisons = forecasts %>%
      dplyr::inner_join(observations, by=c('newmoonnumber','currency','level','species')) %>%
      dplyr::mutate(within_prediction_interval = actual >= LowerPI & actual <= UpperPI, error_metric='coverage') %>%
      dplyr::group_by(model, currency, level, species, lead_time, error_metric) %>%
      dplyr::summarize(error_value=mean(within_prediction_interval)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(error_metric = 'coverage')
    
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
plot_lead_time_errors <- function(error_df, level, species, currency, error_metric){
  plot_title = paste0('Level: ',level,', Species: ',species,', Currency: ',currency)

  graph = ggplot2::ggplot(error_df, ggplot2::aes(x=lead_time, y=error, group=model, color=model)) +
    ggplot2::geom_point()+
    ggplot2::geom_line() +
    ggplot2::labs(y=error_metric,x='Lead Time (New Moons)', title=plot_title)
  ggplot2::plot(graph)
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

forecast_is_valid <- function(forecast_df, verbose=FALSE){
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

compile_forecasts <- function(forecast_folder='./predictions', verbose=FALSE, use_hindcasts=FALSE){
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


# Displaying the data from many forecasts in the past can create a mess of a plot. see #218.
# So here three forecasts are chosen from 1, 6, and 12 newmoons into the past. 
# Sometimes forecasts data is missing for a particular initial_newmoon, the recursive function here
# attempts to find another one from the same time pluts/minus 2 month period. This ensures 3 forecasts
# are always shown.
# If there is no data for initial_newmoon 500, this will check nearby dates in this order: 499,501,498,402
#'
#' @param potential_initial_newmoon
#' @param attempt

update_initial_newmoon <- function(potential_initial_newmoon, attempt=1){
  if(attempt==6){
    return(NA)
  }
  lead_time_data = forecast_errors %>%
    dplyr::filter(initial_newmoon == potential_initial_newmoon)
  
  # Alternate steps of -1, +2, -3, +4
  next_try_step = ifelse(attempt%%2 == 0, attempt, attempt * -1)
  
  # no data to show? then try one month back
  if(nrow(lead_time_data)==0){
    #print(paste('Try ',attempt, ', Nothing with ',potential_initial_newmoon,' trying ',potential_initial_newmoon+next_try_step))
    return(update_initial_newmoon(potential_initial_newmoon+next_try_step, attempt+1))
  } else {
    return(potential_initial_newmoon)
  }
  
}

