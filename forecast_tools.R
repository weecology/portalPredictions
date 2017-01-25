library(dplyr)
library(lubridate)
library(zoo)
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
	## make a plot with mean and confidence intervals for species-specific predictions
  
  # filter data
  data = transform(data, forecast_date = as.yearmon(paste(forecastmonth,"/",forecastyear, sep=""), format="%m/%Y")) %>% 
         transform(Date = as.Date(Date, "%m/%d/%Y"))
  data = filter(data, treatment == 'all',
                    species != 'Total', species != 'total',
                    forecast_date == min(forecast_date),
                    Date == max(Date))
  # make plot
  ggplot(data = data, aes(x = estimate, y = reorder(species, estimate), xmin = LowerPI, xmax = UpperPI))+
  geom_point()+
  geom_errorbarh()+
  ggtitle(paste(data$forecast_date[2]))+ # should make title better somehow
  ylab("Species")+
  xlab("Abundance")
}

#forecast_data = read.csv('/home/shawn/data/portal-forecasts/PortalForecasts.csv', na.strings = '')
#ensemble_model = make_ensemble(forecast_data)
