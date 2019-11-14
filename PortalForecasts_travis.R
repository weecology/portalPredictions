library(portalcasting)

#Update data and models
setup_production()

#Run quick models using portalcasting defaults
portalcast(models = c("ESSS", "AutoArima", "NaiveArima", "nbGARCH", "simplexEDM"))

#Update Website
rmarkdown::render_site()

