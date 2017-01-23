library(dplyr)
library(readxl)
library(readr)


# If necessary, download the Portal data ----------------------------------

if(!file.exists("data")){
  library(ecoretriever)
  dir.create("data")
  ecoretriever::download("PortalMammals", path = "data")
}


# Read in data ------------------------------------------------------------

rodents = read_csv(
  "data/Portal_rodents_19772002.csv", 
  col_types = "iiiidiiicccccccccii__________"
)
plots = read_csv("data/portal_plots.txt", col_names = FALSE)

# This data set is not part of the retriever/Ecological Archives data set
precip = read_excel("data/corrected monthly ppt totals and est.xls")



# Munge precip data -------------------------------------------------------

precip$Season = NA
precip$Season[precip$Month %in% c(11:12, 1:2)] = "winter"
precip$Season[precip$Month %in% (7:10)] = "summer"


# Munge rodent abundances -------------------------------------------------

control_nums = plots[grep("Spectab|Control", plots$X4), ][[1]]

precip %>% group_by(Year, Season) %>% summarise(sum(`TotPrecip(cm)`)) %>%
  na.omit %>% arrange(Year, desc(Season))

yearly_abundances_all = table(
  rodents[rodents$plot %in% control_nums , c("yr", "species")]
)

