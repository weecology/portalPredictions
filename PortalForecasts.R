library(portalcasting)

#Update data and models
setup_dir()

#Run all models using portalcasting defaults
portalcast()

# check
x1 <- read.csv("predictions/2019-02-12forecasts.csv")
print(x1[x1$model=="Ensemble" & x1$species == "total",])

#Update Website
rmarkdown::render_site()

#Clean up temporary files 
cleanup_dir()