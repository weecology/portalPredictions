library(yaml)
library(pomp)
library(doParallel)

source('tools/model_tools.R')

process.sim <- "
  double Z_last = Z;
  double Z_mean_lt = b1 * flt + b2 * flt * flt;
  double Z_mean_de = b3 * cos(M_2PI * fod) + b4 * sin(M_2PI * fod);
  double Z_mean_yr = b5 * cos(M_2PI * foy) + b6 * sin(M_2PI * foy); 
  double Z_mean = Z_last + Z_mean_lt + Z_mean_de + Z_mean_yr;
  //double Z_sigma = 1 / sqrt(Z_tau);
  Z = Z_mean; //rnorm(Z_mean, Z_sigma);
"

count.rmeas <- "
  double Z_back = exp(Z);
  double Z_count = rpois(Z_back);
  Y = rbinom(Z_count, rho);
"

count.dmeas <- "
  double Z_back = exp(Z);
  double Z_count = rpois(Z_back);
  lik = dbinom(Y, Z_count, rho, give_log);
"

to.scale <- "
  TZ_tau = log(Z_tau);
"

from.scale <- "
  TZ_tau = exp(Z_tau);
"






forecast_prototypePOMP <- function(abundances, moons

  params <- c("b1" = 0.00, "b2" = 0.00, 
              "b3" = -0.0014, "b4" = 0.0001,
              "b5" = 0.004, b6 = -0.001,
              "Z_tau" = 98, 
              "rho" = 0.78,
              "Z.0" = 1.67) 
  statenames <- c("Z")           
   
  rprocess_C <- Csnippet(process.sim)
  rprocess <- discrete.time.sim(step.fun = rprocess_C, delta.t = 1)
  rmeasure <- Csnippet(count.rmeas)
  dmeasure <- Csnippet(count.dmeas)
  toEstimationScale <- Csnippet(to.scale)
  fromEstimationScale <- Csnippet(from.scale)

  census_newmoons <- abundances$newmoonnumber
  n_census_newmoons <- length(census_newmoons)
  census_total <- abundances$total
  t0 <- min(census_newmoons) - 1

  poss_newmoons <- min(census_newmoons):max(census_newmoons)
  n_poss_newmoons <- length(poss_newmoons)
  poss_total <- rep(NA, n_poss_newmoons)
  for(i in 1:n_census_newmoons){
    which_poss <- which(poss_newmoons == census_newmoons[i])
    poss_total[which_poss] <- census_total[i]
  }
  obs_data <- data.frame(time = poss_newmoons, Y = poss_total)

  census_date <- as.Date(as.character(moons$censusdate))
  newmoon_date <- as.Date(as.character(moons$newmoondate))
  missing_census <- which(is.na(census_date) == TRUE)
  filled_date <- census_date
  filled_date[missing_census] <- newmoon_date[missing_census]
  incl_newmoons <- t0:max(census_newmoons)
  incl_dates <- filled_date[incl_newmoons]

  yr <- format(incl_dates, "%Y")
  jday <- as.numeric(format(incl_dates, "%j"))
  nye <- as.Date(paste(yr, "-12-31", sep = ""))
  nye_jday <- as.numeric(format(nye, "%j"))
  foy <- jday / nye_jday

  f_day_of_decade <- as.Date(paste(substr(yr, 1, 3), "0-01-01", sep = ""))
  l_day_of_decade <- as.Date(paste(substr(yr, 1, 3), "9-12-31", sep = ""))
  days_in_decade <- as.numeric(l_day_of_decade - f_day_of_decade)
  days_into_decade <- as.numeric(incl_dates - f_day_of_decade)
  fod <- days_into_decade / days_in_decade

  long_t <- incl_newmoons - t_0
  flt <- long_t / max(long_t)
  covariates <- data.frame(time = incl_census, flt, fod, foy)


  params <- c("b1" = 0.00, "b2" = 0.000, 
              "b3" = 0.00, "b4" = 0.000,
              "b5" = 0., "b6" = 0.002,
              #"Z_tau" = 1e100, 
              "rho" = 0.78,
              "Z.0" = 4) 

  mod <- pomp(data = obs_data, times = "time", t0 = t0,
              covar = covariates, tcovar = "time",
              rprocess = rprocess, rmeasure = rmeasure, dmeasure = dmeasure,
              paramnames = names(params), params = params,
              statenames = statenames)#,
#              toEstimationScale = toEstimationScale,
 #             fromEstimationScale = fromEstimationScale)

ff <- simulate(mod)
plot(ff)

par(mfrow = c(2,1))
plot(all$newmoonnumber, all$total, type = 'l')
plot(all$newmoonnumber, log(all$total), type = 'l')


plot
plot(foy[-1], log(all$total))

x <- seq(0, 1, 0.001)
y <- 0.0*cos(2*pi*x) + 0.0018*sin(2*pi*x)
plot(x,y, type = 'l')
y <- -0.0014*cos(2*pi*x) + 0.0001*sin(2*pi*x)
plot(x,y, type = 'l')




moons <- read.csv("data/moon_data.csv")
all <- read.csv("data/rodent_all.csv")
controls <- read.csv("data/rodent_controls.csv")
model_metadata <- yaml.load_file("data/model_metadata.yaml")
forecast_date <- as.Date(model_metadata$forecast_date)
file_suffix <- model_metadata$filename_suffix
forecast_months <- model_metadata$forecast_months
forecast_years <- model_metadata$forecast_years
forecast_newmoons <- model_metadata$forecast_newmoons
num_fcast_nmoons <- length(forecast_months)






forecast_prototypePOMP(abundances = all 









