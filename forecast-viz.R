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
                         for_date_col_name, for_val_col_name, for_lowerpi_col_name,
                         for_upperpi_col_name){
  ggplot(obs_data, aes_string(x = obs_date_col_name)) +
    geom_line(aes_string(y = obs_val_col_name)) +
    geom_ribbon(data = for_data, mapping = aes_string(x = for_date_col_name, ymin = for_lowerpi_col_name, ymax = for_upperpi_col_name), fill = "lightblue") +
    geom_line(data = for_data, mapping = aes_string(x = for_date_col_name, y = for_val_col_name), color = "blue")
}

# download_observations()
# source('https://raw.githubusercontent.com/weecology/PortalDataSummaries/master/RodentAbundances.R')
# obs_data = abundance()
# new_moons = read.csv('~/PortalData/Rodents/moon_dates.csv')
# obs_data_newmoon = inner_join(obs_data, new_moons, by = c("period" = "Period"))
# obs_data_newmoon_dm = select(obs_data_newmoon, NewMoonNumber, estimate = DM)
# for_data = read.csv("predictions/2017-01-27allforecasts.csv")
# for_data_dm = filter(for_data, species == "DM", model == "NegBinom Time Series")
#
# forecast_viz(obs_data_newmoon, "NewMoonNumber", "DM", for_data_dm, "NewMoonNumber", "estimate", "LowerPI", "UpperPI")
