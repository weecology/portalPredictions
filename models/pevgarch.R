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
                               num_forecast_newmoons = 12, CI_level = 0.9,
                               weather_data){

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

    covariates <- list(c('maxtemp','meantemp','precipitation','ndvi'),
                       c('maxtemp','mintemp','precipitation','ndvi'),
                       c('mintemp','maxtemp','meantemp','precipitation'),
                       c('precipitation','ndvi'),
                       c('mintemp','ndvi'),
                       c('mintemp'),
                       c('maxtemp'),
                       c('meantemp'),
                       c('precipitation'),
                       c('ndvi'))

    output_fcast <- data.frame()
    output_aic <- data.frame()

    for(s in species){

      ss <- s
      if(ss == "NA."){
        ss <- "NA"
      }
      cat("Fitting Poisson environmental GARCH models for", ss, "\n")

      species_abundance <- interpolated_abundances %>% extract2(s)
  
      if(sum(species_abundance) == 0){

        spec_forecast <- zero_abund_forecast
        spec_aic <- 1e6

      } else{

        best_model_aic <- Inf
        best_model <- NA
        model_count <- 1
        for(proposed_covariates in covariates){
          cat("Fitting Model", model_count, "\n")
          predictors <- weather_data[ , unlist(proposed_covariates)]
          prop_model = tsglm(species_abundance, 
                             model = list(past_obs = 1, past_mean = 12), 
                             distr = "poisson",
                             xreg = predictors, 
                             link = "log")
      #tsglm sometimes outputs an error when the time series have many 0's, in that case set the AIC
      #to Inf so this proposed model covariate set get skipped
      proposed_model_aic = tryCatch(summary(proposed_model)$AIC, error = function(x) {Inf})
      if(proposed_model_aic < best_model_aic){
        best_model = proposed_model
        best_model_aic = proposed_model_aic
      }
      model_count = model_count + 1
    }
    }
  }

  all <- read.csv("data/rodent_all.csv")
  controls <- read.csv("data/rodent_controls.csv")
  weather <- read.csv("data/weather_data.csv")
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
                                    CI_level = 0.9,
                                    weather_data = weather)

  forecasts_controls <- forecast_pevgarch(abundances = controls, 
                                        forecast_date = forecast_date,
                                        forecast_months = forecast_months, 
                                        forecast_years = forecast_years,
                                        forecast_newmoons = forecast_newmoons,
                                        level = "Controls",
                                        num_forecast_newmoons = 12, 
                                        CI_level = 0.9,
                                        weather_data = weather)

  forecasts <- rbind(forecasts_all[[1]], forecasts_controls[[1]])
  aics <- rbind(forecasts_all[[2]], forecasts_controls[[2]])

  fcast_path <- paste("pevGARCH", filename_suffix, ".csv", sep = "")
  fcast_path <- file.path('tmp', fcast_path)
  write.csv(forecasts, fcast_path, row.names = FALSE)
  aic_path <- paste("pevGARCH", filename_suffix, "_model_aic.csv", sep = "")
  aic_path <- file.path('tmp', aic_path)
  write.csv(aics, aic_path, row.names = FALSE)