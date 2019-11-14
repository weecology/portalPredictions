library(portalcasting)

#Update data and models
setup_production()

#Run faster models using portalcasting defaults
portalcast(models = c("ESSS", "AutoArima", "NaiveArima", "nbGARCH", "nbsGARCH", "pevGARCH", "simplexEDM"))

#Update Website
rmarkdown::render_site()

