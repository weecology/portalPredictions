library(portalcasting)

# Update data and models
setup_dir(all_options(from_zenodo=F))
 
# Run all models using portalcasting defaults
portalcast()

# Update Website
rmarkdown::render_site()

# Clean up temporary files
cleanup_dir()