library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)

#Create the ensemble model from all other forecasts
#Currently just the mean of the esimates and confidence intervals.
make_ensemble=function(all_forecasts, model_weights=NA, models_to_use=NA){
  ensemble = all_forecasts %>%
    group_by(date, NewMoonNumber, forecastmonth, forecastyear,level, currency, species) %>%
    summarise(estimate = mean(estimate), LowerPI=mean(LowerPI), UpperPI=mean(UpperPI))
  ensemble$model='Ensemble'
  return(ensemble)
}

plot_sp_predicts = function(data, lvl, kind) {
  data = transform(data, forecast_date = as.yearmon(paste(forecastmonth, "/", forecastyear, sep =
                                                            ""), format = "%m/%Y")) %>% transform(Date = as.Date(Date, "%Y-%m-%d"))
  data1 = filter(data, level == lvl,
                 Date == max(as.Date(Date)))
  target_moon = min(data1$NewMoonNumber)
  data2 = filter(data1, NewMoonNumber == target_moon)
  title = paste(data2$forecast_date[2], lvl, sep = " ")
  plot_data(data2, title, kind)
}

plot_data = function(data, title, kind) {
  if (kind == 'forecast') {
    ggplot(data,
           aes(
             x = estimate,
             y = reorder(species, estimate),
             xmin = LowerPI,
             xmax = UpperPI
           )) +
      geom_point() +
      geom_errorbarh() +
      ggtitle(title) + # should make title better somehow
      ylab("Species") +
      xlab("Abundance")
  }
  else{
    print("not available")
  }
}

#' Ensure that a forecast file is in the correct format
#' 
#' Tools for working with forecast data expect a certain format.
#' This ensures a forecast file meets those formats.
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
    if(verbose) warning('Forecast file column names invalid')
    return(FALSE)
  }
  
  if(!all(unique(forecast_df$currency) %in% valid_currencies)) { is_valid=FALSE; violations = c('currency', violations) }
  if(!all(unique(forecast_df$level) %in% valid_levels)) { is_valid=FALSE; violations = c('level', violations) }
  if(!all(unique(forecast_df$species) %in% valid_species)) { is_valid=FALSE; violations = c('species', violations) }
  
  #Estimates and PI's cannot have NA values
  if(sum(is.na(forecast_df$estimate))>0) { is_valid=FALSE; violations = c('NA esimates', violations) }
  if(sum(is.na(forecast_df$LowerPI))>0) { is_valid=FALSE; violations = c('NA LowerPI', violations) }
  if(sum(is.na(forecast_df$UpperPI))>0) { is_valid=FALSE; violations = c('NA UpperPI', violations) }
  
  if(verbose) warning(paste('Forecast file invalid: ', violations), sep='')
  return(is_valid)
}


#' Collect all seperate forecasts file into a single dataframe.
#' Will only include files which pass validation.
#' 
#' @param forecast_folder str Base folder holding all forecast files
#' @param verbose bool Output warnings on file violations
#' @return dataframe combined forecasts
compile_forecasts=function(forecast_folder='./predictions', verbose=FALSE){
  forecast_filenames = list.files(forecast_folder, full.names = TRUE, recursive = TRUE)
  all_forecasts=data.frame()
  
  for(this_forecast_file in forecast_filenames){
    this_forecast_data = try(read.csv(this_forecast_file, na.strings = '', stringsAsFactors = FALSE))
    if(class(this_forecast_data) %in% 'try-error'){
      warning(paste('File not readable: ',this_forecast_file, sep=''))
      next
    }
    
    if(forecast_is_valid(this_forecast_data, verbose=verbose)){
      all_forecasts = all_forecasts %>%
        dplyr::bind_rows(this_forecast_data)
    } else {
      warning(paste('File format not valid: ', this_forecast_file, sep=''))
    }
  }
  
  return(all_forecasts)
}

