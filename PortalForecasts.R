library(portalcasting)

#Downloading the full production archive is slow
options(timeout=600)

#Update data and models
setup_production()

#Run all models using portalcasting defaults 
portalcast()

#Evaluate all un-evaluated forecasts
evaluate_casts()
