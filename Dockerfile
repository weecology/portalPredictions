# Use an official Python runtime as a parent image
FROM rocker/tidyverse:latest

# Install `curl` since apparently it isn't in the Rocker images

RUN apt-get update && apt-get install -y curl

# Copy the install script
ADD install-packages.R .

# Install any needed packages specified in requirements.txt
RUN Rscript install-packages.R
