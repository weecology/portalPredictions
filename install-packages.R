# Install pacman if it isn't already installed

if ("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")


# Install analysis packages using pacman

pacman::p_load(devtools, dplyr, forecast, ggplot2, lubridate, readr,testthat,
               tidyverse, zoo, magrittr, rmarkdown, ltsa, parallel, htmltab)
pacman::p_load_gh('weecology/portalr')

# Manually install tscount from CRAN archive

if (!("tscount" %in% rownames(installed.packages()))){
  download.file(url = "https://cran.r-project.org/src/contrib/Archive/tscount/tscount_1.3.0.tar.gz",
                destfile = "~/tscount_1.3.0.tar.gz")
  install.packages(pkgs = "~/tscount_1.3.0.tar.gz", type = "source", repos = NULL)
}
