library(portalcasting)

#Update data and models
setup_dir(options_all = all_options(main = "portalPredictions", download_existing_predictions = FALSE))

#Run all models using portalcasting defaults

portalcast(options_all = all_options(main = "portalPredictions"))
message("models done")
#Update Website
rmarkdown::render_site()
