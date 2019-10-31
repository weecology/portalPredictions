library(portalcasting)

#Update data and models
setup_production()

#Run all models using portalcasting defaults
portalcast(models = c("ESSS"))

#Update Website
rmarkdown::render_site()

