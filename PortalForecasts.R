source('tools/model_functions.R')
source('forecast_tools.R')

#####Run all models########################  
cat("Running models", "\n")
sapply( list.files("models", full.names=TRUE), source ) ###Temporary, while only modeling in R

cat("Compiling site level forecasts", "\n")
allforecasts=forecastall("All",filename_suffix = 'forecasts')

cat("Compiling control plot forecasts", "\n")
controlsforecasts=forecastall("Controls", filename_suffix = 'forecasts')

######Update Website####################################################
rmarkdown::render_site()
