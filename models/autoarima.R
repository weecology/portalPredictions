# AutoARIMA
#
# Model "AutoARIMA" is a flexible auto-regressive integrated moving
#  average model fit to the data using the auto.arima and forecast functions.
#  Unfortunately because of the seasonality and sampling occurring with 
#  different frequencies, which the auto.arima function cannot accommodate, 
#  seasonal models are not included.
#  Also, while the auto.arima model can handle missing data, the other models
#  used currently cannot, so to provide an appropriate comparison, we 
#  interpolate missing data here as well.

  library(forecast)

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

  forecast_autoarima <- function(abundances, forecast_date, forecast_months, 
                                 forecast_years, forecast_newmoons, level,
                                 num_forecast_newmoons = 12, CI_level = 0.9){

    interpolated_abundances <- interpolate_abundance(abundances)

    aa_model <- auto.arima(interpolated_abundances$total, lambda = 0)
    aa_forecast <- forecast(aa_model, h = num_forecast_newmoons,
                            level = CI_level, fan = TRUE)

    # prep the forecast data tabe

      fdt <- data.frame(date = forecast_date, 
                        forecastmonth = forecast_months,
                        forecastyear = forecast_years, 
                        newmoonnumber = forecast_newmoons,
                        currency = "abundance",
                        model = "Weecology-ARIMA", 
                        level = level, species = "total", 
                        estimate = aamf$mean,
                        LowerPI = aamf$lower[,
                                        which(aamf$level == CI_level*100)], 
                        UpperPI = aamf$upper[,
                                        which(aamf$level == CI_level*100)])
       fdt[sapply(fdt, is.ts)] <- lapply(fdt[sapply(fdt, is.ts)], unclass)
  
       # Include columns describing the data used in the forecast

         fdt$fit_start_newmoon <- min(abundances$newmoonnumber)
         fdt$fit_end_newmoon <- max(abundances$newmoonnumber)
         fdt$initial_newmoon <- max(abundances$newmoonnumber)
  
    # prep the aic data tabe
      
      aic <- data.frame(date = as.Date(forecast_date), 
                        currency = 'abundance', 
                        model = 'Weecology-ARIMA', 
                        level = level, species = 'total', 
                        aic = as.numeric(aamf$model$aic), 
                        fit_start_newmoon = min(abundances$newmoonnumber),
                        fit_end_newmoon = max(abundances$newmoonnumber), 
                        initial_newmoon = max(abundances$newmoonnumber))

    # return the output

      return(list(fdt, aic))
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

    allresults <- weecology.arima(abundances = all, 
                                  forecast_date = forecast_date,
                                  forecast_months = forecast_months, 
                                  forecast_years = forecast_years,
                                  forecast_newmoons = forecast_newmoons,
                                  level = "All",
                                  num_forecast_newmoons = 12, CI_level = 0.9)

  # Forecast Control plots

    controlsresults <- weecology.arima(abundances = controls, 
                                       forecast_date = forecast_date,
                                       forecast_months = forecast_months, 
                                       forecast_years = forecast_years,
                                       forecast_newmoons = forecast_newmoons,
                                       level = "Controls",
                                       num_forecast_newmoons = 12, 
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
                paste("Weecology-ARIMA", filename_suffix, ".csv", sep = "")),
            row.names = FALSE)
  write.csv(forecast_aics, 
            file.path('tmp', 
                paste("Weecology-ARIMA", filename_suffix, "_model_aic.csv", 
                      sep = "")),
            row.names = FALSE)

