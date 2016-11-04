library(MASS)
library(rethinking)
n_samples = 10000

new_date = as.Date("2015-12-12")
new_yday = yday(new_date)

models = readRDS("models.rds")

CIs = function(sp){
  model = models[[which(species == sp)]]
  
  is_nearby = abs(abundances$yday - new_yday) < 30 | abundances$yday - new_yday + 365 < 30
  
  get_nearby =function(colname){
    abundances %>% 
      select_("yr_continuous", colname, "yday") %>% 
      filter(is_nearby) %>%
      distinct() %>% 
      extract2(colname)
  }
  
  nearby_precip = get_nearby("precip")
  
  
  samples = data.frame(
    date_ranef = rnorm(n_samples, sd = attr(summary(model$mer)$varcor$date, "stddev")),
    period_ranef = rnorm(n_samples, sd = attr(summary(model$mer)$varcor$period, "stddev")),
    lowTemp = rnorm(n_samples, mean(get_nearby("lowTemp")), sd(get_nearby("lowTemp"))),
    precip = rexp(n_samples, median(nearby_precip[nearby_precip>0])) * rbinom(n_samples, size = 1, prob = mean(nearby_precip > 0))
  )
  
  grid = expand.grid(plot = 1:24, sample = 1:n_samples)
  
  full_samples = cbind(samples[grid$sample, ], plot = grid$plot)
  
  plot_effects = data.frame(plot = 1:24, plot_ranef = ranef(model$mer)$plot[[1]])
  
  newx = abundances %>%
    dplyr::select(plot, treatment) %>%
    distinct() %>%
    mutate(totalPrecip = 156) %>%
    mutate(yr_continuous = julian(new_date, origin = as.Date("1900-01-01")) / 365.24 + 1900) %>%
    mutate(yday = yday(new_date)) %>%
    inner_join(full_samples, "plot") %>% 
    inner_join(plot_effects, "plot")
  
  
  
  raw.predictions = predict(model$gam, newx, type = "link")
  
  predictions = plogis(raw.predictions + newx$date_ranef + newx$period_ranef + newx$plot_ranef)
  
  new = cbind(newx, sample_counts = rbinom(nrow(newx), size = 49, prob = predictions))
  
  simulated_totals = new %>% 
    group_by(period_ranef) %>% 
    summarize(total = sum(sample_counts)) %>% 
    extract2("total")
  
  plot(
    table(simulated_totals), 
    main = "model predictions", 
    sub = sp,
    yaxs = "i", 
    xaxs = "i",
    bty = "l",
    axes = FALSE,
    xlim = c(0, max(simulated_totals))
  )
  axis(1, seq(0, max(simulated_totals), 25 * ceiling(max(simulated_totals)/10 / 25)))
  abline(v = HPDI(simulated_totals, .95), col = 2, lty = 2)
  abline(v = HPDI(simulated_totals, .5), col = 2, lty = 1)
  
  cbind(
    sp,
    as.data.frame(
      rbind(
        c(level = 50, as.numeric(HPDI(simulated_totals, .5))),
        c(level = 95, as.numeric(HPDI(simulated_totals,.95)))
      )
    ),
    stringsAsFactors = FALSE
  )
  
}

ci_predictions = lapply(species, CIs)

bind_rows(ci_predictions) %>% 
  filter(level == 50) %>% 
  rename(lower = V2, upper = V3) %>%
  bind_rows(
    bind_rows(ci_predictions) %>% 
      filter(level == 95) %>% 
      rename(lower = V2, upper = V3)
  ) %>%
  write.csv(file = "predictions/2015-12-12_predictions from 2015-12-11.csv", row.names = FALSE)