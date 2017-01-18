library(tidyverse)
library(lubridate)



#Create the ensemble model from all other forecasts
#Currently just the mean of the esimates and confidence intervals.
make_ensemble=function(all_forecasts, model_weights=NA, models_to_use=NA){
  ensemble= all_forecasts %>%
    group_by(Date, forecastmonth, forecastyear,treatment, species) %>%
    summarise(estimate = mean(estimate), LowerPI=mean(LowerPI), UpperPI=mean(UpperPI))
  return(ensemble)
}


#forecast_data = read.csv('/home/shawn/data/portal-forecasts/PortalForecasts.csv', na.strings = '')
#ensemble_model = make_ensemble(forecast_data)