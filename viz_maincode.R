source("forecast-viz.R")
source("forecast_tools.R")

source("https://raw.githubusercontent.com/weecology/PortalData/master/DataSummaryScripts/RodentAbundances.R")

data = read.csv("./predictions/PortalForecasts.csv")
ensemble = make_ensemble(data)

