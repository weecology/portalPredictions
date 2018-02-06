# nbGARCH
#
# Model "nbGARCH" is a generalized autoregresive conditional 
#  heteroscedasticity model with overdispersion (i.e. a negative binomial 
#  response variable)
#  

  library(tscount)
  library(forecast)

#' Function for nbGARCH
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

  forecast_nbgarch <- function(abundances, forecast_date, forecast_months, 
                               forecast_years, forecast_newmoons, level,
                               num_forecast_newmoons = 12, CI_level = 0.9){

    species <- c("BA", "DM", "DO", "DS", "NA.", "OL", "OT", "PB", "PE", 
                 "PF", "PH", "PL", "PM", "PP", "RF", "RM", "RO", "SF",
                 "SH", "SO", "total")

    interpolated_abundances <- interpolate_abundance(abundances)

    fit_start_newmoon <- min(abundances$newmoonnumber)
    fit_end_newmoon <- max(abundances$newmoonnumber)
    initial_newmoon <- max(abundances$newmoonnumber)

    zero_abund_mean <- rep(0, 12)
    zero_abund_interval <- matrix(0, ncol = 2, nrow = 12)
    colnames(zero_abund_interval) <- c("lower", "upper")

    zero_abund_forecast <- list(zero_abund_mean, zero_abund_interval)
    names(zero_abund_forecast) <- c("pred", "interval")


    output_fcast <- data.frame()
    output_aic <- data.frame()

    for(s in species){

      cat("Fitting negative binomial GARCH model for", s, "\n")

      species_abundance <- interpolated_abundances %>% extract2(s)
    
      if(sum(species_abundance) == 0){
        spec_forecast <- zero_abund_forecast
        spec_aic <- 1e6
      } else{
        nbgarch_mod <- tsglm(species_abundance, 
                             model = list(past_obs = 1, past_mean = 12),
                             distr = "nbinom", link = "log")
        spec_forecast <- predict(nbgarch_mod, num_forecast_newmoons, 
                                 level = CI_level)
        spec_aic <- AIC(nbgarch_mod)
      } 

      spec_output_fcast <- data.frame(date = forecast_date, 
                                      forecastmonth = forecast_months, 
                                      forecastyear = forecast_years,
                                      newmoonnumber = forecast_newmoons, 
                                      currency = "abundance", 
                                      model = "nbGARCH",
                                      level = level, 
                                      species = s, 
                                      estimate = spec_forecast$pred,
                                      LowerPI = spec_forecast$interval[,1], 
                                      UpperPI = spec_forecast$interval[,2],
                                      fit_start_newmoon = fit_start_newmoon,
                                      fit_end_newmoon = fit_end_newmoon,
                                      initial_newmoon = initial_newmoon)
      output_fcast <- rbind(output_fcast, spec_output_fcast)

      spec_output_aic <- data.frame(date = forecast_date, 
                                    currency = 'abundance', 
                                    model = 'nbGARCH', 
                                    level = level, species = s, 
                                    aic = as.numeric(spec_aic), 
                                    fit_start_newmoon = fit_start_newmoon,
                                    fit_end_newmoon = fit_end_newmoon,
                                    initial_newmoon = initial_newmoon)

      output_aic <- rbind(output_aic, spec_output_aic)

    }

    for(i in 5:8)
      output_fcast[ , i] <- as.character(output_fcast[ , i])
    for(i in 9:11)
      output_fcast[ , i] <- as.numeric(output_fcast[ , i])
    for(i in 2:5)
      output_aic[ , i] <- as.character(output_aic[ , i])

    output <- list(output_fcast, output_aic)
    names(output) <- c("forecast", "aic")

    return(output) 
  }

  all <- read.csv("data/rodent_all.csv")
  controls <- read.csv("data/rodent_controls.csv")
  model_metadata <- yaml.load_file("data/model_metadata.yaml")
  forecast_date <- as.Date(model_metadata$forecast_date)
  filename_suffix <- model_metadata$filename_suffix
  forecast_months <- model_metadata$forecast_months
  forecast_years <- model_metadata$forecast_years
  forecast_newmoons <- model_metadata$forecast_newmoons

  forecasts_all <- forecast_nbgarch(abundances = all, 
                                    forecast_date = forecast_date,
                                    forecast_months = forecast_months, 
                                    forecast_years = forecast_years,
                                    forecast_newmoons = forecast_newmoons,
                                    level = "All",
                                    num_forecast_newmoons = 12, 
                                    CI_level = 0.9)

  forecasts_controls <- forecast_nbgarch(abundances = controls, 
                                        forecast_date = forecast_date,
                                        forecast_months = forecast_months, 
                                        forecast_years = forecast_years,
                                        forecast_newmoons = forecast_newmoons,
                                        level = "Controls",
                                        num_forecast_newmoons = 12, 
                                        CI_level = 0.9)

  forecasts <- rbind(forecasts_all[[1]], forecasts_controls[[1]])
  aics <- rbind(forecasts_all[[2]], forecasts_controls[[2]])

  fcast_path <- paste("nbGARCH", filename_suffix, ".csv", sep = "")
  fcast_path <- file.path('tmp', fcast_path)
  write.csv(forecasts, fcast_path, row.names = FALSE)
  aic_path <- paste("nbGARCH", filename_suffix, "_model_aic.csv", sep = "")
  aic_path <- file.path('tmp', aic_path)
  write.csv(aics, aic_path, row.names = FALSE)
