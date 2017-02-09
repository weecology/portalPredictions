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
