library(portalcasting)

#Update data and models
setup_dir(options_all = all_options(base = ".", main = "", download_existing_predictions = FALSE))
unlink(sub_paths(dirtree(base = ".", main = "", subs = "PortalData")), recursive = TRUE, force = TRUE)

#Run all models using portalcasting defaults

portalcast(options_all = all_options(base = ".", main = ""))
message("models done")

#Update Website
rmarkdown::render_site()
