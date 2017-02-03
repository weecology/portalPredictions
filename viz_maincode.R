source("forecast-viz.R")
source("forecast_tools.R")

source("https://raw.githubusercontent.com/weecology/PortalDataSummaries/master/RodentAbundances.R")

data = compile_forecasts()
ensemble = make_ensemble(data)

plot_sp_predicts(ensemble, 'All', 'forecast')
