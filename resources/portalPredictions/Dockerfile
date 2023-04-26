# base image for the stack is the rocker "tidyverse", which includes rstudio and devtools
# https://hub.docker.com/r/rocker/tidyverse

FROM rocker/tidyverse:latest

# Install `curl` and `jags` c libraries

RUN apt-get update && apt-get install -y curl 
RUN apt-get update && apt-get install -y jags

RUN apt-get update \
    && apt-get install -y \
       curl \
       jags

# Install portalcasting from github

RUN R -e "devtools::install_github('weecology/portalcasting')"
