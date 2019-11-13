library(portalcasting)

#Update data and models
setup_production()

#Run all models using portalcasting defaults except jags_RW and pevGARCH for testing expedience
portalcast(models = c("ESSS", "AutoArima", "NaiveArima", "nbGARCH", "nbsGARCH", "simplexEDM"))

#Update Website
#rmarkdown::render_site()

