library(portalcasting)

#Update data and models
setup_dir()

###########
tree <- dirtree(".", "", c("predictions", "models", "PortalData", "data", "tmp"));
all <- read.csv(file_path(tree, "data/all.csv"));
controls <- read.csv(file_path(tree, "data/controls.csv"));
covariates <- read.csv(file_path(tree, "data/covariates.csv")); 
metadata <- yaml::yaml.load_file(file_path(tree, "data/metadata.yaml"));
print(covariates)
cat(metadata$forecast_date)
cat("\n")
###

#Run all models using portalcasting defaults
portalcast()

#Update Website
rmarkdown::render_site()

#Clean up temporary files
cleanup_dir()