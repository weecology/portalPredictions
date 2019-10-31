# base image for the stack is the rocker "tidyverse", which includes rstudio and devtools
# https://hub.docker.com/r/rocker/tidyverse
FROM rocker/tidyverse:latest

# Install `curl` and `jags` c libraries, which are not in the rocker image

 RUN apt-get update && apt-get install -y curl 
 RUN apt-get update && apt-get install -y jags

 RUN R -e "install.packages('portalr', repos = 'http://cran.us.r-project.org')"

# Copy the install script
# ADD install-packages.R .

# Install any needed packages specified in requirements.txt
# RUN Rscript install-packages.R
