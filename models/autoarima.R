# AutoARIMA
#
# Model "AutoARIMA" is a flexible auto-regressive integrated moving
#  average model fit to the data using the auto.arima and forecast functions.
#  Unfortunately since the seasonality and sampling occurring with 
#  different frequencies, which the auto.arima function cannot accommodate, 
#  seasonal models are not included.
#  Also, while the auto.arima model can handle missing data, the other models
#  used currently cannot, so to provide an appropriate comparison, we 
#  interpolate missing data here as well.

  library(forecast)
  source('tools/model_tools.R')

#' Function for AutoARIMA
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
#'
  forecast_autoarima <- function(abundances, forecast_date, forecast_months, 
                                 forecast_years, forecast_newmoons, level,
                                 num_forecast_newmoons, CI_level = 0.9){

    cat("Fitting AutoARIMA model for total \n")
    interpolated_abundances <- interpolate_abundance(abundances)

    aa_model <- auto.arima(interpolated_abundances$total)
    aa_forecast <- forecast(aa_model, h = num_forecast_newmoons,
                            level = CI_level, fan = TRUE)

    fit_start_newmoon <- min(abundances$newmoonnumber)
    fit_end_newmoon <- max(abundances$newmoonnumber)
    initial_newmoon <- max(abundances$newmoonnumber)
    CI_match <- which(aa_forecast$level == CI_level * 100)
    estimate <- as.numeric(aa_forecast$mean)
    LowerPI <- as.numeric(aa_forecast$lower[ , CI_match]) 
    UpperPI <- as.numeric(aa_forecast$upper[ , CI_match])

    output_fcast <- data.frame(date = forecast_date, 
                               forecastmonth = forecast_months,
                               forecastyear = forecast_years, 
                               newmoonnumber = forecast_newmoons,
                               currency = "abundance",
                               model = "AutoArima", 
                               level = level, 
                               species = "total", 
                               estimate = estimate,
                               LowerPI = LowerPI, 
                               UpperPI = UpperPI,
                               fit_start_newmoon = fit_start_newmoon,
                               fit_end_newmoon = fit_end_newmoon,
                               initial_newmoon = initial_newmoon,
                               stringsAsFactors = FALSE)

    output_aic <- data.frame(date = as.Date(forecast_date), 
                             currency = 'abundance', 
                             model = 'AutoArima', 
                             level = level, species = 'total', 
                             aic = as.numeric(aa_model$aic), 
                             fit_start_newmoon = fit_start_newmoon,
                             fit_end_newmoon = fit_end_newmoon,
                             initial_newmoon = initial_newmoon,
                             stringsAsFactors = FALSE)

    output <- list(output_fcast, output_aic)
    names(output) <- c("forecast", "aic")

    return(output)
  }


  all <- read.csv("data/rodent_all.csv")
  controls <- read.csv("data/rodent_controls.csv")
  model_metadata <- yaml.load_file("data/model_metadata.yaml")
  forecast_date <- as.Date(model_metadata$forecast_date)
  file_suffix <- model_metadata$filename_suffix
  forecast_months <- model_metadata$rodent_forecast_months
  forecast_years <- model_metadata$rodent_forecast_years
  forecast_newmoons <- model_metadata$rodent_forecast_newmoons
  num_fcast_nmoons <- length(forecast_months)

  forecasts_all <- forecast_autoarima(abundances = all, 
                                forecast_date = forecast_date,
                                forecast_months = forecast_months, 
                                forecast_years = forecast_years,
                                forecast_newmoons = forecast_newmoons,
                                level = "All",
                                num_forecast_newmoons = num_fcast_nmoons, 
                                CI_level = 0.9)

  forecasts_controls <- forecast_autoarima(abundances = controls, 
                                     forecast_date = forecast_date,
                                     forecast_months = forecast_months, 
                                     forecast_years = forecast_years,
                                     forecast_newmoons = forecast_newmoons,
                                     level = "Controls",
                                     num_forecast_newmoons = num_fcast_nmoons, 
                                     CI_level = 0.9)

  forecasts <- rbind(forecasts_all[[1]], forecasts_controls[[1]])
  aics <- rbind(forecasts_all[[2]], forecasts_controls[[2]])

  fcast_filename <- paste("AutoArima", file_suffix, ".csv", sep = "")
  fcast_path <- file.path('tmp', fcast_filename)
  write.csv(forecasts, fcast_path, row.names = FALSE)

  aic_filename <- paste("AutoArima", file_suffix, "_model_aic.csv", sep = "")
  aic_path <- file.path('tmp', aic_filename)
  write.csv(aics, aic_path, row.names = FALSE)
