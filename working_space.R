source('tools/model_functions.R')
source('tools/forecast_tools.R')
library(yaml)

model_metadata = yaml.load_file("data/model_metadata.yaml")
forecast_date = as.Date(model_metadata$forecast_date)


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

    allresults <- naive1(all, forecast_date, forecast_months, forecast_years,
                         forecast_newmoons, "All")


abundances = all
forecast_date
forecast_months
forecast_years
forecast_newmoons
level = "All"
num_forecast_months = 12
CI_level = .9



    # interpolate missing data

      moons <- (min(abundances$newmoonnumber)):(max(abundances$newmoonnumber))
      abunds <- rep(NA, length(moons))
      for(i in 1:length(moons)){
        if(length(which(abundances$newmoonnumber == moons[i])) > 0){
          abunds[i] <- abundances$total[abundances$newmoonnumber == moons[i]]
        }
      }

      interpolated_abundances <- round(na.interp(abunds))
    
    # convert to appropriate frequency (12) for seasonality in 

    # fit the ets model and forecast with it

      model01 <- forecast(interpolated_abundances, h = num_forecast_months,
                          level = CI_level, 
                          allow.multiplicative.trend = TRUE)

    # prep the forecast data tabe

      forecasts01 <- data.frame(date = forecast_date, 
                                forecastmonth = forecast_months,
                                forecastyear = forecast_years, 
                                newmoonnumber = forecast_newmoons,
                                currency = "abundance",
                                model = "Forecast", level = level, 
                                species = "total", estimate = model01$mean,
                                LowerPI = model01$lower[,
                                          which(model01$level==CI_level*100)], 
                                UpperPI = model01$upper[,
                                          which(model01$level==CI_level*100)])
       forecasts01[sapply(forecasts01, is.ts)] <- 
          lapply(forecasts01[sapply(forecasts01, is.ts)], unclass)
  
       # Include columns describing the data used in the forecast

         forecasts01$fit_start_newmoon <- min(abundances$newmoonnumber)
         forecasts01$fit_end_newmoon <- max(abundances$newmoonnumber)
         forecasts01$initial_newmoon <- max(abundances$newmoonnumber)
  
    # prep the aic data tabe
      
      aic <- data.frame(date = as.Date(forecast_date), 
                        currency = 'abundance', 
                        model = 'Forecast', level = level, species = 'total', 
                        aic = as.numeric(model01$model$aic), 
                        fit_start_newmoon = min(abundances$newmoonnumber),
                        fit_end_newmoon = max(abundances$newmoonnumber), 
                        initial_newmoon = max(abundances$newmoonnumber))

    # return the output

      return(list(forecasts01,aic))
  }


