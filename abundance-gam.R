library(dplyr)
library(lubridate)
library(mgcv)

dataFolder='~/data/portal/'


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
  mutate(date = safe_ifelse(TimeOfDay == "morning", date, date - 1)) %>%
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



# Note that yr_continuous may drift a bit with leap years, but should never be
# more than one day from the correct value
munge_species = function(sp){
  rodents %>% 
    group_by(plot, date) %>%
    summarize(abundance = sum(species == sp)) %>%
    ungroup() %>%
    inner_join(climate, "date") %>%
    mutate(is_control = plot %in% controlPlots) %>% 
    mutate(is_exclosure = plot %in% kratPlots) %>%
    mutate(yday = yday(date)) %>%
    mutate(yr_continuous = julian(date, origin = as.Date("1900-01-01")) / 365.24 + 1900) %>%
    mutate(date_factor = factor(date)) %>%
    mutate(plot_factor = factor(plot))
}

fit_gam = function(species){
  data = munge_species(species)
  
  # note the periodic spline for yday with knots between 1 and 365.24
  # This distribution/family may not be ideal for this data set.
  
  # Note the weights are reduced as a cheap hack to flatten the likelihood surface.
  # The intuition is that---without random effects on date---the model thinks that
  # it's seen more independent data points than it really has. 
  # The reduced weights are a clumsy way to counteract that effect. The "re"
  # basis won't work with this many dates, unfortunately. Eventually, we should 
  # switch to the random effects in gamm or gamm4.
  gam(
    cbind(abundance, 49 - abundance) ~ is_control + is_exclosure + 
      s(totalPrecip) + s(precip) + s(lowTemp) + 
      s(yday, bs = "cc") + s(yr_continuous) + 
      s(plot_factor, bs = "re"),
    data = data,
    family = binomial,
    knots=list(yday=c(1,365.24)),
    weights = rep(1/10, nrow(data))
  )
}

# totalPrecip   : cumulative rain over previous 6 months
# precip        : rain over the previous night
# lowTemp       : lowest temp over the previous night
# yday          : periodic/seasonal trend
# yr_continuous : long-term trend
# The last plot is special: it shows the plot-level random effects


plot(fit_gam("PP"), pages = 1, n = 1000, main = "PP", shade = TRUE, cex.lab = 1.5)
plot(fit_gam("DM"), pages = 1, n = 1000, main = "DM", shade = TRUE, cex.lab = 1.5)
plot(fit_gam("DO"), pages = 1, n = 1000, main = "DO", shade = TRUE, cex.lab = 1.5)
plot(fit_gam("OT"), pages = 1, n = 1000, main = "OT", shade = TRUE, cex.lab = 1.5)
plot(fit_gam("RM"), pages = 1, n = 1000, main = "RM", shade = TRUE, cex.lab = 1.5)


# The model loses its mind when it sees zero PBs thousands of times in
# a row during the early 1990's because it can't explain that many "independent" 
# results all turning out the same way without extreme measures
plot(fit_gam("PB"), pages = 1, n = 1000, main = "PB", shade = TRUE, cex.lab = 1.5)
