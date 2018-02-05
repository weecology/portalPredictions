# ESSS
#
# Model "ESSS" is a flexible exponential smoothing state space model
#  fit using the forecast and ets functions with the possibilit of 
#  multiplicative trends. Unfortunately because of the seasonality and 
#  sampling occurring with different frequencies, which the ets function 
#  cannot accommodate, seasonal models are not included.


# load dependencies

  library(forecast)

# Write model function

  esss <- function(abundances, forecast_date, forecast_months, 
                     forecast_years, forecast_newmoons, level,
                     num_forecast_months = 12, CI_level = 0.9){

    # interpolate missing data
    #  note that we interpolate based on the species-level counts and then
    #  sum (rather than interpolate the sum) because of Jensen's inequality
    #  and that we want our models that work with individual species to be
    #  the same total abundance as our models that just use total.
    #  [FYI a comparison found that an interpolation of just totals could be
    #   off by as much as 6%

      moons <- (min(abundances$newmoonnumber)):(max(abundances$newmoonnumber))
      nmoons <- length(moons)

      species <- c("BA", "DM", "DO", "DS", "NA.", "OL", "OT", "PB", "PE", 
                   "PF", "PH", "PL", "PM", "PP", "RF", "RM", "RO", "SF",
                   "SH", "SO")
      nspecies <- length(species)

      abunds <- matrix(NA, nrow = nmoons, ncol = nspecies)

      for(i in 1:nmoons){
        if(length(which(abundances$newmoonnumber == moons[i])) > 0){
          abundst <- abundances[which(abundances$newmoonnumber == moons[i]),
                                which(colnames(abundances) %in% species)]
          abunds[i, ] <- as.numeric(abundst)
        }
      }

      interpolated_abunds <- abunds

      for(j in 1:nspecies){
        interpolated_abunds[ , j] <- round(na.interp(abunds[, j]))
      }

      interpolated_total <- apply(interpolated_abunds, 1, sum)

    # fit the ets model and forecast with it

      etsm <- ets(interpolated_total)
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
