source('tools/model_functions.R')
source('tools/forecast_tools.R')

#########Run all models##################################################  
cat("Running models", "\n")
dir.create("tmp")
sapply( list.files("models", full.names=TRUE), source ) ###Temporary, while only modeling in R

#####Collect all forecast results and save to predictions directory######
cat("Compiling forecasts", "\n")
newforecasts=forecastall(Sys.Date())
unlink("tmp/*")
######Update Website#####################################################
rmarkdown::render_site()