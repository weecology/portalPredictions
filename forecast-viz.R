library(ggplot2)
library(stringr)
library(dplyr)

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

#data <- data.frame(dates = c(1,2,3), counts = c(4,5,4))
#for_data <- data.frame(dates = c(4,5,6), estimate = c(5, 6, 6), LowerPI = c(3,2,1), UpperPI = c(6,7,8))
#forecast_viz(data, "dates", "counts", for_data, "dates", "estimate", "LowerPI", "UpperPI")
