# Install pacman if it isn't already installed

if ("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")


# Install analysis packages using pacman

pacman::p_load(devtools, dplyr, forecast, ggplot2, htmltab, lubridate, ltsa, magrittr, 
               parallel, RCurl, readr, rmarkdown, testthat, tidyverse, tscount, yaml, zoo)
pacman::p_load_gh('weecology/portalr')
pacman::p_load_gh('weecology/portalcasting')
