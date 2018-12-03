library(portalcasting)

#Update data and models
setup_dir(options_all = all_options(main = "portalPredictions", download_existing_predictions = FALSE))

#Run all models using portalcasting defaults
#Save predictions to predictions directory in base directory

portalcast(options_all = all_options(main = "portalPredictions"))

#Update Website
rmarkdown::render_site()
