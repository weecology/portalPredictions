abundance-gam=function(level,date,forecastmonth,forecastyear,NewMoonNumber,CI_level) {

mc.cores = 8 # How many species to run in parallel
new_yday = yday(Sys.Date()) # Replace Sys.Date with the next Portal sampling date

library(MASS)
library(dplyr)
library(magrittr)
library(lubridate)
library(gamm4)
library(tidyr)
library(parallel)
library(coda)
source('forecast_tools.R')

base_folder = normalizePath('~')

rodents = read.csv(FullPath('PortalData/Rodents/Portal_rodent.csv', base_folder), na.strings = c("","NA"), 
                   colClasses = c('tag' = 'character'), 
                   stringsAsFactors = FALSE)
sppCodes = read.csv(FullPath('PortalData/Rodents/Portal_rodent_species.csv', base_folder)) %>%
  dplyr::select(species = Species.Code, Rodent, Unidentified)

rodents = rodents %>% mutate(date = as.Date(paste(yr, mo, dy, sep = "-")))
plots=read.csv(FullPath('PortalData/SiteandMethods/Portal_plots.csv', base_folder))


# Discard unnecessary rows ------------------------------------------------

# Creating a "pseudo-species" for when all the rodents are absent at a plot
# prevents those plots from being thrown away.
rodents$species[rodents$note1 == 2] = "all_absent"

# Throw away bad rows
rodents = rodents[!is.na(rodents$species), ]
rodents = rodents[rodents$period > 0, ]
rodents = rodents[!is.na(rodents$plot), ]

# Some weather data is missing before 1990
rodents = rodents[rodents$yr >= 1990,]

# Remove non-rodents and unidentified rodents
rodents = rodents %>%
  left_join(sppCodes, by = c('species')) %>%
  filter(Rodent == 1, Unidentified == 0) 
rodents = subset(rodents,select=-c(Rodent,Unidentified))

# Treatment ---------------------------------------------------------------

# Identify control and k-rat exclosure plots
rodents = left_join(rodents,plots,by=c("yr","mo"="month","plot"))


# Weather -----------------------------------------------------------------

# Total precip over a long time period ??? "resources"
precipMonthLag = 6
precipDayLag = 30 * precipMonthLag # assuming 30-day months

weatherRaw = read.csv(FullPath('PortalData/Weather/Portal_weather.csv', base_folder)) %>%
  mutate(TimeOfDay = ifelse(Hour > 1200, "evening", "morning")) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-")))


# A version of `ifelse` that retains the class of the objects.
# See https://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects#comment7892723_6669237
safe_ifelse = function(cond, yes, no){
  structure(ifelse(cond, yes, no), class = class(yes))
}

weather = weatherRaw %>%
  filter((Hour >= 1800 & Hour <= 2400) | (Hour >= 100 & Hour <= 600)) %>%
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
 # select(-all_absent) %>% 
  inner_join(climate, "date") %>%
  mutate(yday = yday(date)) %>%
  mutate(yr_continuous = julian(date, origin = as.Date("1900-01-01")) / 365.24 + 1900)

# keep only long-term controls if forecasting control abundances

if(level="Controls") {
  rodents = rodents %>% 
    filter(plot %in%
             c(3, 4, 10, 11, 14, 15, 16, 17, 19, 21, 23))
  
  abundances = rodents %>% 
    mutate(species = factor(species)) %>% 
    group_by(plot, date, treatment, period) %>%
    do(data.frame(x = table(.$species))) %>% 
    spread(x.Var1, x.Freq) %>%
    ungroup() %>%
    # select(-all_absent) %>% 
    inner_join(climate, "date") %>%
    mutate(yday = yday(date)) %>%
    mutate(yr_continuous = julian(date, origin = as.Date("1900-01-01")) / 365.24 + 1900)
}

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
  # treatment     : control, k-rat exclosure, or full exclosure
  
  model = gamm4(
    cbind(successes, failures) ~ 
      treatment + 
      s(totalPrecip) + s(precip) + s(lowTemp) + 
      s(yday, bs = "cc") + s(yr_continuous),
    random = ~(1|plot) + (1|date) + (1|period),
    data = data,
    family = binomial,
    knots = list(yday = c(1,365.24))
  )
  
  
  # # Plotting
  # pdf(paste0(species, ".pdf"))
  # 
  # # Plot the splines
  # plot(model$gam, pages = 1, n = 1000, main = species, shade = TRUE, cex.lab = 1.5)
  # 
  
  # # Plot the period-level random effects
  # plot(
  #   x = row.names(ranef(model$mer)$period),
  #   y = ranef(model$mer)$period[[1]],
  #   pch = 16,
  #   cex = .5,
  #   main = "Period residuals",
  #   ylab = "Residual for the period"
  # )
  # abline(0, 0, col = "#00000020")
  # 
  # 
  
  # # Plot the date-level random effects
  # plot(
  #   x = as.Date(row.names(ranef(model$mer)$date)),
  #   y = ranef(model$mer)$date[[1]],
  #   pch = 16,
  #   cex = .5,
  #   main = "Daily residuals",
  #   ylab = "Residual for the day"
  # )
  # abline(0, 0, col = "#00000020")
  # 
  # # Plot the treatment effects
  # plot(
  #   summary(model$gam)$p.table[-1, "Estimate"] ~ factor(names(summary(model$gam)$p.table[-1, "Estimate"])),
  #   ylab = "Treatment effect"
  # )
  
  # # Plot the plot-level effects
  # plot(
  #   ranef(model$mer)$plot[[1]],
  #   cex = .5,
  #   pch = 16,
  #   col = "darkgray",
  #   ylab = "plot residual"
  # )
  # text(
  #   1:24,
  #   ranef(model$mer)$plot[[1]],
  #   1:24
  # )
  # abline(0, 0, col = "#00000020")
  # dev.off()
  
  return(model)
}


# Run the model for all species -------------------------------------------

species = rodents %>% 
  group_by(species) %>% 
  summarize(total = n()) %>% 
  filter(total > 25, species != "all_absent") %>% 
  extract2("species")

models = mclapply(species,
                  fit_gam)
                  #mc.cores = mc.cores)
saveRDS(
  models,
  "models.rds"
)


# Make predictions from the model -----------------------------------------

CIs = function(sp, new_date, n_samples = 10000){
  model = models[[which(species == sp)]]
  
  new_yday = yday(new_date)
  
  # I think this code is basically used to predict the kind of weather
  # that can be expected around new_date, based on comparing to the same season
  # of other years.  The is_nearby stuff has to do with spanning
  # annual boundaries
  
  abundances$is_nearby = abs(abundances$yday - new_yday) < 30 | 
    abundances$yday - new_yday + 365 < 30
  
  get_nearby = function(colname){
    abundances %>%
      select_("yr_continuous", colname, "yday","is_nearby") %>%
      filter(is_nearby) %>%
      distinct() %>%
      extract2(colname)
  }
  
  nearby_precip = get_nearby("precip")
  
  # Random samples for possible environmental conditions at the next sampling
  # event
  samples = data.frame(
    date_ranef = rnorm(n_samples, sd = attr(summary(model$mer)$varcor$date, "stddev")),
    period_ranef = rnorm(n_samples, sd = attr(summary(model$mer)$varcor$period, "stddev")),
    lowTemp = rnorm(n_samples, mean(get_nearby("lowTemp")), sd(get_nearby("lowTemp"))),
    precip = rexp(n_samples, 
                  median(nearby_precip[nearby_precip > 0])) * 
      rbinom(n_samples, size = 1, prob = mean(nearby_precip > 0)))
  # Make every combination of plot and sample
  grid = expand.grid(plot = 1:24, sample = 1:n_samples)
  full_samples = cbind(samples[grid$sample, ], plot = grid$plot)
  
  plot_effects = data.frame(plot = 1:24, plot_ranef = ranef(model$mer)$plot[[1]])
  
  # to feed to predict()
  newx = abundances %>%
    dplyr::select(plot, treatment) %>%
    distinct() %>%
    mutate(totalPrecip = 156) %>%
    mutate(yr_continuous = julian(new_date, origin = as.Date("1900-01-01")) / 365.24 + 1900) %>%
    mutate(yday = yday(new_date)) %>%
    inner_join(full_samples, "plot") %>%
    inner_join(plot_effects, "plot")
  
  
  raw.predictions = predict(model$gam, newx, type = "link")
  
  predictions = plogis(raw.predictions + newx$date_ranef + newx$period_ranef + 
                         newx$plot_ranef)

  new = cbind(newx, sample_counts = rbinom(nrow(newx), size = 49,
                                          prob = predictions))
  
  simulated_totals = new %>%
   group_by(period_ranef) %>%
  summarize(total = sum(sample_counts)) %>%
   extract2("total")
  
  # 
  # plot(
  #   table(simulated_totals),
  #   main = "model predictions",
  #   sub = sp,
  #   yaxs = "i",
  #   xaxs = "i",
  #   bty = "l",
  #   axes = FALSE,
  #   xlim = c(0, max(simulated_totals))
  # )
  # axis(1, seq(0, max(simulated_totals), 
  #             25 * ceiling(max(simulated_totals)/10 / 25)))
  # abline(v = names(sort(table(simulated_totals),decreasing=TRUE))[1], col = 2, lwd=2)
  # abline(v = HPDinterval(as.mcmc(simulated_totals), .95), col = 2, lty = 2)
  # abline(v = HPDinterval(as.mcmc(simulated_totals), .9), col = 2, lty = 3)
  # abline(v = HPDinterval(as.mcmc(simulated_totals), .5), col = 2, lty = 1)
  
  data.frame(
    species = sp, estimate = as.numeric(names(sort(table(simulated_totals),decreasing=TRUE))[1]),
        t(as.numeric(HPDinterval(as.mcmc(simulated_totals), CI_level)))
  )
  
}

ci_predictions = lapply(species, CIs, new_date=Sys.Date())

return(bind_rows(date,forecastmonth,forecastyear,NewMoonNumber,"abundance","GAM",level,ci_predictions) %>%
  rename(LowerPI = X1, UpperPI = X2))

}