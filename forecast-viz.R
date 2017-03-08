library(ggplot2)
library(stringr)
library(dplyr)
source('forecast_tools.R')

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

# download_observations()
# source('https://raw.githubusercontent.com/weecology/PortalDataSummaries/master/RodentAbundances.R')
# obs_data = abundance()
# obs_data$total = rowSums(select(obs_data, -period))
# new_moons = read.csv('~/PortalData/Rodents/moon_dates.csv')
# obs_data_newmoon = inner_join(obs_data, new_moons, by = c("period" = "Period"))
# for_data = read.csv("predictions/2016-8-31Allforecasts.csv")
#
# forecast_viz(obs_data = obs_data_newmoon,
#              obs_date_col_name = "NewMoonNumber",
#              obs_val_col_name = "total",
#              for_data = for_data,
#              for_date_col_name = "NewMoonNumber",
#              for_val_col_name = "estimate",
#              for_model_name = "NegBinom Time Series",
#              for_lowerpi_col_name = "LowerPI",
#              for_upperpi_col_name = "UpperPI",
#              start_newmoon = 300)
