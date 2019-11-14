library(portalcasting)

#Update data and models
setup_production()

#Run all models using portalcasting defaults 
portalcast()

#Update Website
rmarkdown::render_site()

