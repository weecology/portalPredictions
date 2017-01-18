library(ggplot2)
library(stringr)

#' Visualize a time-series forecast
#' Plots the observed time-series and the 1-step forecasts within it
#' Plots the forecast time-series along with the prediction interval for future observations
#' obs_data is a data.frame
#' date_col_name is a string with the name for the date column
#' val_col_name is a string with the name for the column of the value being forecast
forecast_viz <- function(obs_data, date_col_name, val_col_name){
  ggplot(obs_data, aes_string(x = date_col_name, y = val_col_name)) +
    geom_line()
}
