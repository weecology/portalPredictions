# ESSS
#
# Model "ESSS" is a flexible exponential smoothing state space model
#  fit using the forecast and ets functions with the possibilit of 
#  multiplicative trends. Unfortunately because of the seasonality and 
#  sampling occurring with different frequencies, which the ets function 
#  cannot accommodate, seasonal models are not included.


  library(forecast)

#' Function for ESSS
#'
#' @param abundances table of rodent abundances and time measures
#' @param forecast_date the dates to be forecast from
#' @param forecast_months the months of the dates to be forecast
#' @param forecast_years the years of the dates to be forecast
#' @param forecast_newmoons the numbers of the new moons to be forecast
#' @param level name of the type of plots included ("All" or "Controls")
#' @param num_forecast_months number of months forward to forecast
#' @param CI_level confidence interval level used for forecast envelope
#' @return list of forecast and aic tables

  esss <- function(abundances, forecast_date, forecast_months, 
                   forecast_years, forecast_newmoons, level,
                   num_forecast_months = 12, CI_level = 0.9){


 
    interpolated_abundances <- interpolate_abundance(abundances)

    ets_model <- ets(interpolated_abundances$total)
    ets_forecast <- forecast(ets_model, h = num_forecast_months,
                             level = CI_level, 
                             allow.multiplicative.trend = TRUE)

    fit_start_newmoon <- min(abundances$newmoonnumber)
    fit_end_newmoon <- max(abundances$newmoonnumber)
    initial_newmoon <- max(abundances$newmoonnumber)
    CI_match <- which(ets_forecast$level == CI_level * 100)

    output_fcast <- data.frame(date = forecast_date, 
                               forecastmonth = forecast_months,
                               forecastyear = forecast_years, 
                               newmoonnumber = forecast_newmoons,
                               currency = "abundance",
                               model = "ESSS", 
                               level = level, 
                               species = "total", 
                               estimate = ets_forecast$mean,
                               LowerPI = ets_forecast$lower[ , CI_match], 
                               UpperPI = ets_forecast$upper[ , CI_match],
                               fit_start_newmoon = fit_start_newmoon,
                               fit_end_newmoon = fit_end_newmoon,
                               initial_newmoon = initial_newmoon)
  
    output_aic <- data.frame(date = as.Date(forecast_date), 
                             currency = 'abundance', 
                             model = 'ESSS', 
                             level = level, species = 'total', 
                             aic = as.numeric(ets_model$aic), 
                             fit_start_newmoon = fit_start_newmoon,
                             fit_end_newmoon = fit_end_newmoon,
                             initial_newmoon = initial_newmoon)

    output <- list(output_fcast, output_aic)
    names(output) <- c("forecast", "aic")

    return(output)
  }




# Run model on all plots and just controls

  # Get data

    all <- read.csv("data/rodent_all.csv")
    controls <- read.csv("data/rodent_controls.csv")
    model_metadata <- yaml.load_file("data/model_metadata.yaml")
    forecast_date <- as.Date(model_metadata$forecast_date)
    filename_suffix <- model_metadata$filename_suffix
    forecast_months <- model_metadata$forecast_months
    forecast_years <- model_metadata$forecast_years
    forecast_newmoons <- model_metadata$forecast_newmoons

  # Forecast All plots

    allresults <- esss(abundances = all, 
                       forecast_date = forecast_date,
                       forecast_months = forecast_months, 
                       forecast_years = forecast_years,
                       forecast_newmoons = forecast_newmoons,
                       level = "All",
                       num_forecast_months = 12, CI_level = 0.9)

  # Forecast Control plots

    controlsresults <- esss(abundances = controls, 
                            forecast_date = forecast_date,
                            forecast_months = forecast_months, 
                            forecast_years = forecast_years,
                            forecast_newmoons = forecast_newmoons,
                            level = "Controls",
                            num_forecast_months = 12, 
                            CI_level = 0.9)

  # Combine output
  #  warnings are suppressed here because they're just associated with 
  #  coercing un-matching sets of factors to character vectors to bind

    forecasts <- suppressWarnings(bind_rows(allresults[1], 
                                            controlsresults[1]))
    forecast_aics <- suppressWarnings(bind_rows(allresults[2], 
                                                controlsresults[2]))

# Write results out

  write.csv(forecasts, 
            file.path('tmp', 
                paste("ESSS", filename_suffix, ".csv", sep = "")),
            row.names = FALSE)
  write.csv(forecast_aics, 
            file.path('tmp', 
                paste("ESSS", filename_suffix, "_model_aic.csv", 
                      sep = "")),
            row.names = FALSE)
