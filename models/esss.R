# ESSS
#
# Model "ESSS" is a flexible exponential smoothing state space model
#  fit using the forecast and ets functions with the possibilit of 
#  multiplicative trends. Unfortunately because of the seasonality and 
#  sampling occurring with different frequencies, which the ets function 
#  cannot accommodate, seasonal models are not included.


  library(forecast)

#' Function for ESSS


  esss <- function(abundances, forecast_date, forecast_months, 
                     forecast_years, forecast_newmoons, level,
                     num_forecast_months = 12, CI_level = 0.9){


 
      interpolated_abundances <- interpolate_abundance(abundances)

    # fit the ets model and forecast with it

      etsm <- ets(interpolated_abundances$total)
      etsmf <- forecast(etsm, h = num_forecast_months,
                          level = CI_level, 
                          allow.multiplicative.trend = TRUE)

    # prep the forecast data tabe

      fdt <- data.frame(date = forecast_date, 
                        forecastmonth = forecast_months,
                        forecastyear = forecast_years, 
                        newmoonnumber = forecast_newmoons,
                        currency = "abundance",
                        model = "ESSS", 
                        level = level, 
                        species = "total", estimate = etsmf$mean,
                        LowerPI = etsmf$lower[,
                                      which(etsmf$level == CI_level*100)], 
                        UpperPI = etsmf$upper[,
                                      which(etsmf$level == CI_level*100)])
       fdt[sapply(fdt, is.ts)] <- lapply(fdt[sapply(fdt, is.ts)], unclass)
  
       # Include columns describing the data used in the forecast

         fdt$fit_start_newmoon <- min(abundances$newmoonnumber)
         fdt$fit_end_newmoon <- max(abundances$newmoonnumber)
         fdt$initial_newmoon <- max(abundances$newmoonnumber)
  
    # prep the aic data tabe
      
      aic <- data.frame(date = as.Date(forecast_date), 
                        currency = 'abundance', 
                        model = 'ESSS', 
                        level = level, species = 'total', 
                        aic = as.numeric(etsmf$model$aic), 
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
