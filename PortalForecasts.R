library(portalcasting)

#Update data and models
setup_production()

#Run all models using portalcasting defaults
portalcast(models = c("ESSS", "AutoArima", "NaiveArima", "pevGARCH", "nbGARCH", "nbsGARCH", "simplexEDM"))

#Update Website
rmarkdown::render_site()

