library(portalcasting)

#Update data and models
setup_dir()

#Run all models using portalcasting defaults
#Save predictions to predictions directory in base directory

portalcast()

#Update Website
rmarkdown::render_site()
