library(dplyr)
library(lubridate)
library(ggplot2)

#Create the ensemble model from all other forecasts
#Currently just the mean of the esimates and confidence intervals.
make_ensemble=function(all_forecasts, model_weights=NA, models_to_use=NA){
  ensemble = all_forecasts %>%
    group_by(Date, forecastmonth, forecastyear,treatment, species) %>%
    summarise(estimate = mean(estimate), LowerPI=mean(LowerPI), UpperPI=mean(UpperPI))
  ensemble$model='Ensemble'
  return(ensemble)
}

plot_sp_predicts <- function(data){
	# make a plot with mean and confidence intervals for species-specific predictions
  ggplot(data = data, aes(x = estimate, y = reorder(species, estimate), xmin = LowerPI, xmax = UpperPI))+
  geom_point()+
  geom_errorbarh()+
  ggtitle(paste(data$Date[2])) +
  ylab("Species")+
  xlab("Abundance")
}

#forecast_data = read.csv('/home/shawn/data/portal-forecasts/PortalForecasts.csv', na.strings = '')
#ensemble_model = make_ensemble(forecast_data)