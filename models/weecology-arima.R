# Weecology-ARIMA
#
# Model "Weecology-ARIMA" is a flexible auto-regressive integrated moving
#  average model fit to the data using the auto.arima and forecast functions.
#  Unfortunately because of the seasonality and sampling occurring with 
#  different frequencies, which the auto.arima function cannot accommodate, 
#  seasonal models are not included.
#  Also, while the auto.arima model can handle missing data, the other models
#  used currently cannot, so to provide an appropriate comparison, we 
#  interpolate missing data here as well.


# load dependencies

  library(forecast)

# Write model function

  weecology.arima <- function(abundances, forecast_date, forecast_months, 
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

    # fit the arima model and forecast with it

      a.a <- auto.arima(interpolated_total, lambda = 0)
      a.a.f <- forecast(a.a, h = num_forecast_months,
                        level = CI_level, fan = TRUE)




    # prep the forecast data tabe

      fdt <- data.frame(date = forecast_date, 
                        forecastmonth = forecast_months,
                        forecastyear = forecast_years, 
                        newmoonnumber = forecast_newmoons,
                        currency = "abundance",
                        model = "Weecology-ARIMA", 
                        level = level, species = "total", 
                        estimate = a.a.f$mean,
                        LowerPI = a.a.f$lower[,
                                        which(model01$level == CI_level*100)], 
                        UpperPI = a.a.f$upper[,
                                        which(model01$level == CI_level*100)])
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
                        aic = as.numeric(a.a.f$model$aic), 
                        fit_start_newmoon = min(abundances$newmoonnumber),
                        fit_end_newmoon = max(abundances$newmoonnumber), 
                        initial_newmoon = max(abundances$newmoonnumber))

    # return the output

      return(list(fdt, aic))
  }



