# Use an official Python runtime as a parent image
FROM rocker/tidyverse:latest

# Install `curl` and `jags`, which are not in the rocker image

RUN apt-get update && apt-get install -y curl 
RUN apt-get update && apt-get install -y jags

# Copy the install script
 ADD install-packages.R .

# Install any needed packages specified in requirements.txt
 RUN Rscript install-packages.R
