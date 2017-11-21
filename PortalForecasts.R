library(rmarkdown)
source('models/model_functions.R')

#####Run all models########################  
cat("Running models", "\n")
sapply( list.files("models", full.names=TRUE), source ) ###Temporary, while only modeling in R

cat("Compiling site level forecasts", "\n")
allforecasts=forecastall("All")

cat("Compiling control plot forecasts", "\n")
controlsforecasts=forecastall("Controls")

######Update Website####################################################
rmarkdown::render_site()
