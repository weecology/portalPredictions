# Install pacman if it isn't already installed

if ("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")


# Install analysis packages using pacman

pacman::p_load(devtools, dplyr, forecast, ggplot2, lubridate, readr,
               tidyverse, zoo, magrittr, rmarkdown)
pacman::p_load_gh('weecology/PortalDataSummaries')
