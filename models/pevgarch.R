# pevGARCH
#
# Model "pevGARCH" is a generalized autoregresive conditional 
#  heteroscedasticity model with a Poisson response variable
#  

  library(tscount)
  library(forecast)

#' Function for pevGARCH
#'
#' @param abundances table of rodent abundances and time measures
#' @param forecast_date the dates to be forecast from
#' @param forecast_months the months of the dates to be forecast
#' @param forecast_years the years of the dates to be forecast
#' @param forecast_newmoons the numbers of the new moons to be forecast
#' @param level name of the type of plots included ("All" or "Controls")
#' @param num_forecast_newmoons number of new moons to forecast
#' @param CI_level confidence interval level used for forecast envelope
#' @return list of forecast and aic tables

  forecast_pevGARCH <- function(abundances, forecast_date, forecast_months, 
                               forecast_years, forecast_newmoons, level,
                               num_forecast_newmoons = 12, CI_level = 0.9){

  }

  all <- read.csv("data/rodent_all.csv")
  controls <- read.csv("data/rodent_controls.csv")
  model_metadata <- yaml.load_file("data/model_metadata.yaml")
  forecast_date <- as.Date(model_metadata$forecast_date)
  filename_suffix <- model_metadata$filename_suffix
  forecast_months <- model_metadata$forecast_months
  forecast_years <- model_metadata$forecast_years
  forecast_newmoons <- model_metadata$forecast_newmoons

  forecasts_all <- forecast_pevgarch(abundances = all, 
                                    forecast_date = forecast_date,
                                    forecast_months = forecast_months, 
                                    forecast_years = forecast_years,
                                    forecast_newmoons = forecast_newmoons,
                                    level = "All",
                                    num_forecast_newmoons = 12, 
                                    CI_level = 0.9)

  forecasts_controls <- forecast_pevgarch(abundances = controls, 
                                        forecast_date = forecast_date,
                                        forecast_months = forecast_months, 
                                        forecast_years = forecast_years,
                                        forecast_newmoons = forecast_newmoons,
                                        level = "Controls",
                                        num_forecast_newmoons = 12, 
                                        CI_level = 0.9)

  forecasts <- rbind(forecasts_all[[1]], forecasts_controls[[1]])
  aics <- rbind(forecasts_all[[2]], forecasts_controls[[2]])

  fcast_path <- paste("pevGARCH", filename_suffix, ".csv", sep = "")
  fcast_path <- file.path('tmp', fcast_path)
  write.csv(forecasts, fcast_path, row.names = FALSE)
  aic_path <- paste("pevGARCH", filename_suffix, "_model_aic.csv", sep = "")
  aic_path <- file.path('tmp', aic_path)
  write.csv(aics, aic_path, row.names = FALSE)