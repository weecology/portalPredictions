library(portalcasting)
#library(portalr)
print(sessionInfo())

#Update data and models
setup_production()

#Run all models using portalcasting defaults
portalcast(models = "ESSS")

#Update Website
#rmarkdown::render_site()

#Clean up temporary files 
#cleanup_dir()