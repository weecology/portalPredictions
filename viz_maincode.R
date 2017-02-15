source("forecast-viz.R")
source("forecast_tools.R")
source("actual_census_counts.R")

source("https://raw.githubusercontent.com/weecology/PortalDataSummaries/master/RodentAbundances.R")

data = compile_forecasts()
ensemble = make_ensemble(data)

# TODO: Automate picking the correct census rather than assigning it manually
obs_data = extract_recent_census_data(458)

# Plot just the forecasts
plot_sp_predicts(ensemble, 'All')

# Plot forecasts and outcomes
plot_sp_predicts(ensemble, 'All', observed = obs_data)

new_moons=read.csv('~/PortalData/Rodents/moon_dates.csv')

#Format raw observations to match forecast comparison columns and include NewMoonNumbers
#level='Site' in abundance() corresponds to level='All' from the portal forecasting spec. It should be changed in the abundance script.
#TODO: add a more generic function to generate this
all_observations = abundance(shape='flat', level='Site') %>%
  group_by(period) %>%
  summarise(actual=sum(abundance)) %>%
  ungroup() %>%
  mutate(level='All',currency='abundance',species='total') %>%
  left_join(new_moons, by=c('period'='Period')) 
  

all_forecasts = ensemble %>%
  bind_rows(data)

forecast_errors = calculate_forecast_error(all_observations, all_forecasts)
plot_lead_time_errors(forecast_errors, level='All',species='total',currency='abundance', error_metric='MSE')

