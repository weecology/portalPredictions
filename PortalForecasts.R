source('tools/model_functions.R')
source('tools/model_tools.R')
source('tools/forecast_tools.R')
library(yaml)

model_metadata = yaml.load_file("data/model_metadata.yaml")
forecast_date = as.Date(model_metadata$forecast_date)

if(!(forecast_date == Sys.Date())){ stop('Data not updated') }

#########Run all models##################################################  
cat("Running models", "\n")
dir.create("tmp")
sapply( list.files("models", full.names=TRUE), source ) ###Temporary, while only modeling in R

#####Collect all forecast results and save to predictions directory######
cat("Compiling forecasts", "\n")
newforecasts=forecastall(forecast_date)
unlink("tmp/*")
######Update Website#####################################################
rmarkdown::render_site()