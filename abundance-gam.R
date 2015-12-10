library(dplyr)
library(lubridate)
library(gamm4)
library(tidyr)

rodents=read.csv('data/RodentsAsOfSep2015.csv', na.strings=c("","NA"), colClasses=c('tag'='character'), stringsAsFactors = FALSE)

rodents = rodents %>% mutate(date = as.Date(paste(yr, mo, dy, sep = "-")))


# Discard unnecessary rows ------------------------------------------------

# Creating a "pseudo-species" for when all the rodents are absent at a plot
# prevents those plots from being thrown away.
rodents$species[rodents$note1 == 2] = "all_absent"

# Throw away bad rows
rodents=rodents[!is.na(rodents$species),]
rodents=rodents[rodents$period>0,]
rodents=rodents[!is.na(rodents$plot),]

# Some weather data is spotty before 1990
rodents=rodents[rodents$yr>=1990,]



# Treatment ---------------------------------------------------------------

# Identify control and k-rat exclosure plots
controlPlots=c(2,4,8,11,12,14,17,22) #controls
kratPlots=c(3,6,13,18,19,20) #krat exclosure

rodents$treatment = "full_exclosure"
rodents$treatment[rodents$plot %in% controlPlots] = "control"
rodents$treatment[rodents$plot %in% kratPlots] = "krat_exclosure"


# Weather -----------------------------------------------------------------

# Total precip over a long time period ≈ "resources"
precipMonthLag=6
precipDayLag = 30 * precipMonthLag # assuming 30-day months

weatherRaw = read.csv('data/Hourly_PPT_mm_1989_present_fixed.csv') %>%
  mutate(TimeOfDay = ifelse(Hour>1200, "evening", "morning")) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-")))


# A version of `ifelse` that retains the class of the objects.
# See https://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects#comment7892723_6669237
safe_ifelse = function(cond, yes, no){
  structure(ifelse(cond, yes, no), class = class(yes))
}

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


# Full data set -----------------------------------------------------------

# Note that yr_continuous may drift a bit with leap years, but should never be
# more than one day from the correct value

abundances = rodents %>% 
  mutate(species = factor(species)) %>% 
  group_by(plot, date, treatment, period) %>%
  do(data.frame(x = table(.$species))) %>% 
  spread(x.Var1, x.Freq) %>%
  ungroup() %>%
  select(-all_absent) %>% 
  inner_join(climate, "date") %>%
  mutate(yday = yday(date)) %>%
  mutate(yr_continuous = julian(date, origin = as.Date("1900-01-01")) / 365.24 + 1900)



# Model fitting -----------------------------------------------------------

fit_gam = function(species){
  # Binomial response variable:
  
  # A "success" occurs when the species is found in a trap
  successes = abundances[[species]]
  
  # A "failure" occurs in any trap that doesn't have the focal species.
  failures = 49 - successes
  
  data = cbind(successes = successes, failures = failures, abundances)
  
  # Fixed effects:
  # totalPrecip   : cumulative rain over previous 6 months
  # precip        : rain over the previous night
  # lowTemp       : lowest temp over the previous night
  # yday          : periodic/seasonal trend (knots at year endpoints)
  # yr_continuous : long-term trend
  
  model = gamm4(
    cbind(successes, failures) ~ 
      treatment + 
      s(totalPrecip) + s(precip) + s(lowTemp) + 
      s(yday, bs = "cc") + s(yr_continuous),
    random = ~(1|plot) + (1|date) + (1|period),
    data = data,
    family = binomial,
    knots=list(yday=c(1,365.24))
  )
  
  pdf(paste0(sp, ".pdf"))
  plot(model$gam, , pages = 1, n = 1000, main = sp, shade = TRUE, cex.lab = 1.5)
  
  plot(
    x = as.Date(row.names(ranef(model$mer)$date_factor)),
    y = ranef(model$mer)$date_factor[[1]],
    pch = 16,
    cex = .5,
    main = "Daily residuals",
    ylab = "Residual for the day"
  )
  abline(0, 0, col = "#00000020")
  
  
  plot(
    summary(model$gam)$p.table[-1, "Estimate"] ~ factor(names(summary(model$gam)$p.table[-1, "Estimate"])),
    ylab = "Treatment effect"
  )
  
  
  plot(
    ranef(model$mer)$plot_factor[[1]],
    cex = .5,
    pch = 16,
    col = "darkgray",
    ylab = "plot residual"
  )
  text(
    1:24,
    ranef(model$mer)$plot_factor[[1]],
    1:24
  )
  abline(0, 0, col = "#00000020")
  dev.off()
  
  return(model)
}


saveRDS(fit_gam("PP"), file = "PP_gamm4.rds")


