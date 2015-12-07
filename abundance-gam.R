library(dplyr)
library(lubridate)
library(gamm4)
library(tidyr)

dataFolder='/home/harrisd/portalPredictions/data/'


rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"), colClasses=c('tag'='character'))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))

rodents=rodents[rodents$yr>=1990,]

rodents=rodents[rodents$period>0,]
rodents=rodents[!is.na(rodents$plot),]
rodents=rodents[!is.na(rodents$species),]

rodents = rodents %>% mutate(date = as.Date(paste(yr, mo, dy, sep = "-")))

# Total precip over a long time period ≈ "resources"
precipMonthLag=6
precipDayLag = 30 * precipMonthLag # assuming 30-day months

# A version of `ifelse` that retains the class of the objects.
# See https://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects#comment7892723_6669237
safe_ifelse = function(cond, yes, no){
  structure(ifelse(cond, yes, no), class = class(yes))
}

weatherRaw = read.csv(paste0(dataFolder,'Hourly_PPT_mm_1989_present_fixed.csv')) %>%
  mutate(TimeOfDay = ifelse(Hour>1200, "evening", "morning")) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-")))


weather = weatherRaw %>%
  filter((Hour>=1800 & Hour <=2400) | (Hour>=100 & Hour<=600)) %>%
  mutate(date = safe_ifelse(TimeOfDay == "morning", date, date + 1)) %>%
  group_by(date) %>%
  summarize(precip = sum(Precipitation), lowTemp = min(TempAir))

# Note: some dates are missing!
daily_precip = weatherRaw %>% 
  group_by(date) %>% 
  summarize(Precipitation = sum(Precipitation))

daily_precip$totalPrecip = sapply(
  daily_precip$date,
  function(today){
    days = (daily_precip$date < today) & (daily_precip$date >= today - precipDayLag)
    sum(daily_precip[["Precipitation"]][days])
  }
)

climate = full_join(weather, daily_precip, "date")

# Identify control and k-rat exclosure plots
controlPlots=c(2,4,8,11,12,14,17,22) #controls
kratPlots=c(3,6,13,18,19,20) #krat exclosure


rodents$treatment = "full_exclosure"
rodents$treatment[rodents$plot %in% controlPlots] = "control"
rodents$treatment[rodents$plot %in% kratPlots] = "krat_exclosure"


# Note that yr_continuous may drift a bit with leap years, but should never be
# more than one day from the correct value
munge_species = function(sp){
  rodents %>% 
    group_by(plot, date, treatment) %>%
    summarize(abundance = sum(species == sp)) %>%
    ungroup() %>%
    inner_join(climate, "date") %>%
    mutate(yday = yday(date)) %>%
    mutate(yr_continuous = julian(date, origin = as.Date("1900-01-01")) / 365.24 + 1900) %>%
    mutate(date_factor = factor(date)) %>%
    mutate(plot_factor = factor(plot)) %>%
    mutate(species = sp)
}

spp = names(sort(table(rodents$species), decreasing = TRUE))

abundances = lapply(spp, munge_species) %>%
  bind_rows %>%
  spread(species, abundance) %>%
  mutate(total_abundance = rowSums(.[ , spp]))




fit_gam = function(species){
  # A "success" occurs when the species is found in a trap
  successes = abundances[[species]]
  
  # A "failure" occurs only when a trap is empty (not when it's occupied by
  # a nontarget species; this is neither a success nor a failure)
  failures = 49 - abundances[["total_abundance"]]
  
  # note the periodic spline for yday with knots between 1 and 365.24
  
  data = cbind(successes = successes, failures = failures, abundances)
  
  
  # This distribution/family may not be ideal for this data set.
  
  gamm4(
    cbind(successes, failures) ~ 
      treatment + 
      s(totalPrecip) + s(precip) + s(lowTemp) + 
      s(yday, bs = "cc") + s(yr_continuous),
    random = ~(1|plot_factor) + (1|date_factor),
    data = data,
    family = binomial,
    knots=list(yday=c(1,365.24))
  )
}

# totalPrecip   : cumulative rain over previous 6 months
# precip        : rain over the previous night
# lowTemp       : lowest temp over the previous night
# yday          : periodic/seasonal trend
# yr_continuous : long-term trend
# The last plot is special: it shows the plot-level random effects

saveRDS(fit_gam("PP"), file = "PP_gamm4.rds")
