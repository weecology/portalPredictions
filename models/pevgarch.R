# pevGARCH
#
# Model "pevGARCH" is a generalized autoregresive conditional 
#  heteroscedasticity model with a Poisson response variable
#  For this model, environmental data are included as predictors of abundance,
#  but with a 6 month lag between the covariate values and the abundances.
#  The specific lag is imposed in the get_weather_data function, which is 
#  used in the prepare_data script to produce the weather data here.

  library(tscount)
  library(forecast)
  source('tools/model_tools.R')

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
#'
  forecast_pevgarch <- function(abundances, forecast_date, forecast_months, 
                                forecast_years, forecast_newmoons, level,
                                num_forecast_newmoons, CI_level = 0.9,
                                weather_data, weathermeans){

    species <- colnames(abundances)[2:(ncol(abundances) - 3)]

    interpolated_abundances <- interpolate_abundance(abundances)

    fit_start_newmoon <- min(abundances$newmoonnumber)
    fit_end_newmoon <- max(abundances$newmoonnumber)
    initial_newmoon <- max(abundances$newmoonnumber)

    zero_abund_mean <- rep(0, num_forecast_newmoons)
    zero_abund_interval <- matrix(0, ncol = 2, nrow = num_forecast_newmoons)
    colnames(zero_abund_interval) <- c("lower", "upper")

    zero_abund_forecast <- list(zero_abund_mean, zero_abund_interval)
    names(zero_abund_forecast) <- c("pred", "interval")

    covariates <- list(c('maxtemp', 'meantemp', 'precipitation', 'ndvi'),
                       c('maxtemp', 'mintemp', 'precipitation', 'ndvi'),
                       c('mintemp', 'maxtemp', 'meantemp', 'precipitation'),
                       c('precipitation', 'ndvi'),
                       c('mintemp', 'ndvi'),
                       c('mintemp'),
                       c('maxtemp'),
                       c('meantemp'),
                       c('precipitation'),
                       c('ndvi'),
                       c(NULL))

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
          model_setup <- list(past_obs = 1, past_mean = 12)
          prop_model <- tryCatch(tsglm(species_abundance, 
                                       model = model_setup, 
                                       distr = "poisson",
                                       xreg = predictors, 
                                       link = "log"),
                                 error = function(x) {NA})
          prop_model_aic <- tryCatch(AIC(prop_model), 
                                     error = function(x) {Inf})
          if(prop_model_aic < best_model_aic){
            best_model <- prop_model
            best_model_aic <- prop_model_aic
          }
          model_count <- model_count + 1
        }

        if(is.na(best_model)){
          spec_forecast <- zero_abund_forecast
          spec_aic <- 1e6
        } else{
          spec_forecast <- tryCatch(predict(best_model, 
                                            num_forecast_newmoons, 
                                            level = CI_level, 
                                            newdata = weathermeans),
                                    error = function(x) {NA})
          spec_aic <- best_model_aic
 
          if(is.na(spec_forecast)){
            predictors <- weather_data[ , unlist(NULL)]
            prop_model <- tryCatch(tsglm(species_abundance, 
                                       model = model_setup, 
                                       distr = "poisson",
                                       xreg = predictors, 
                                       link = "log"),
                                    error = function(x) {NA})
            spec_forecast <- predict(prop_model, 
                                     num_forecast_newmoons, 
                                     level = CI_level, 
                                     newdata = weathermeans)
            spec_aic <- prop_model_aic 

            if(is.na(spec_forecast)){
              spec_forecast <- zero_abund_forecast
              spec_aic <- 1e6
            }
          }
        }

      }

      estimate <- as.numeric(spec_forecast$pred)
      LowerPI <- as.numeric(spec_forecast$interval[, 1]) 
      UpperPI <- as.numeric(spec_forecast$interval[, 2])
      spec_output_fcast <- data.frame(date = forecast_date, 
                                      forecastmonth = forecast_months, 
                                      forecastyear = forecast_years,
                                      newmoonnumber = forecast_newmoons, 
                                      currency = "abundance", 
                                      model = "pevGARCH",
                                      level = level, 
                                      species = ss, 
                                      estimate = estimate,
                                      LowerPI = LowerPI, 
                                      UpperPI = UpperPI,
                                      fit_start_newmoon = fit_start_newmoon,
                                      fit_end_newmoon = fit_end_newmoon,
                                      initial_newmoon = initial_newmoon,
                                      stringsAsFactors = FALSE)
      output_fcast <- rbind(output_fcast, spec_output_fcast)

      spec_output_aic <- data.frame(date = forecast_date, 
                                    currency = 'abundance', 
                                    model = 'pevGARCH', 
                                    level = level, species = ss, 
                                    aic = as.numeric(spec_aic), 
                                    fit_start_newmoon = fit_start_newmoon,
                                    fit_end_newmoon = fit_end_newmoon,
                                    initial_newmoon = initial_newmoon,
                                    stringsAsFactors = FALSE)

      output_aic <- rbind(output_aic, spec_output_aic)

    }
    output <- list(output_fcast, output_aic)
    names(output) <- c("forecast", "aic")

    return(output) 
  }

  all <- read.csv("data/rodent_all.csv")
  controls <- read.csv("data/rodent_controls.csv")
  weather <- read.csv("data/weather_data.csv")
  model_metadata <- yaml.load_file("data/model_metadata.yaml")
  forecast_date <- as.Date(model_metadata$forecast_date)
  file_suffix <- model_metadata$filename_suffix
  forecast_months <- model_metadata$forecast_months
  forecast_years <- model_metadata$forecast_years
  forecast_newmoons <- model_metadata$forecast_newmoons
  num_fcast_nmoons <- length(forecast_months)

  weathermeans <- weather[dim(weather)[1] - 36:dim(weather)[1], ] %>%
                   group_by(month) %>% 
                   summarise_all(funs(mean(., na.rm=TRUE))) %>%
                   select(-c(year, newmoonnumber, NewMoonNumber_with_lag)) %>%
                   slice(match(forecast_months, month))
  

  forecasts_all <- forecast_pevgarch(abundances = all, 
                                    forecast_date = forecast_date,
                                    forecast_months = forecast_months, 
                                    forecast_years = forecast_years,
                                    forecast_newmoons = forecast_newmoons,
                                    level = "All",
                                    num_forecast_newmoons = num_fcast_nmoons, 
                                    CI_level = 0.9,
                                    weather_data = weather,
                                    weathermeans = weathermeans)

  forecasts_controls <- forecast_pevgarch(abundances = controls, 
                                    forecast_date = forecast_date,
                                    forecast_months = forecast_months, 
                                    forecast_years = forecast_years,
                                    forecast_newmoons = forecast_newmoons,
                                    level = "Controls",
                                    num_forecast_newmoons =  num_fcast_nmoons, 
                                    CI_level = 0.9,
                                    weather_data = weather,
                                    weathermeans = weathermeans)

  forecasts <- rbind(forecasts_all[[1]], forecasts_controls[[1]])
  aics <- rbind(forecasts_all[[2]], forecasts_controls[[2]])

  fcast_filename <- paste("pevGARCH", file_suffix, ".csv", sep = "")
  fcast_path <- file.path('tmp', fcast_filename)
  write.csv(forecasts, fcast_path, row.names = FALSE)

  aic_filename <- paste("pevGARCH", file_suffix, "_model_aic.csv", sep = "")
  aic_path <- file.path('tmp', aic_filename)
  write.csv(aics, aic_path, row.names = FALSE)