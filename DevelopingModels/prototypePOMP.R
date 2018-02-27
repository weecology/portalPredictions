library(yaml)
library(pomp)
library(doParallel)

source('tools/model_tools.R')

process.sim <- "
  double Z_last = Z;
  double Z_mean_lt = b0 + b1 * flt + b2 * flt * flt + b3 * flt * flt * flt;
  double Z_mean_de = b4 * cos(M_2PI * fod) + b5 * sin(M_2PI * fod);
  double Z_mean_yr = b6 * cos(M_2PI * foy) + b7 * sin(M_2PI * foy); 
  double Z_mean = Z_last + Z_mean_lt + Z_mean_de + Z_mean_yr;
  double Z_sigma = 1 / sqrt(Z_tau);
  Z = rnorm(Z_mean, Z_sigma);
"

count.rmeas <- "
  double Z_back = exp(Z);
  double Z_count = rpois(Z_back);
  Y = rbinom(Z_count, rho);
"

count.dmeas <- "
  if(ISNA(Y)) {
    lik = (give_log) ? 0 : 1;
  } else {
    double Z_back = exp(Z);
    double Z_count = rpois(Z_back);
    lik = dbinom(Y, Z_count, rho, give_log);
  }
"

to.scale <- "
  TZ_tau = log(Z_tau);
  Trho = logit(rho);
"

from.scale <- "
  TZ_tau = exp(Z_tau);
  Trho = expit(rho);
"




  statenames <- c("Z")           
   
  rprocess_C <- Csnippet(process.sim)
  rprocess <- discrete.time.sim(step.fun = rprocess_C, delta.t = 1)
  rmeasure <- Csnippet(count.rmeas)
  dmeasure <- Csnippet(count.dmeas)
  toEstimationScale <- Csnippet(to.scale)
  fromEstimationScale <- Csnippet(from.scale)

  obs_newmoons <- abundances$newmoonnumber
  n_obs_newmoons <- length(obs_newmoons)
  obs_total <- abundances$total
  t0 <- min(obs_newmoons) - 1

  poss_newmoons <- min(obs_newmoons):max(obs_newmoons)
  n_poss_newmoons <- length(poss_newmoons)
  poss_total <- rep(NA, n_poss_newmoons)
  for(i in 1:n_obs_newmoons){
    which_poss <- which(poss_newmoons == obs_newmoons[i])
    poss_total[which_poss] <- obs_total[i]
  }
  obs_data <- data.frame(time = poss_newmoons, Y = poss_total)

  newmoon_date <- as.Date(as.character(moons$newmoondate))
  cov_newmoons <- t0:max(census_newmoons)
  cov_dates <- newmoon_date[incl_newmoons]

  yr <- format(cov_dates, "%Y")
  jday <- as.numeric(format(cov_dates, "%j"))
  nye <- as.Date(paste(yr, "-12-31", sep = ""))
  nye_jday <- as.numeric(format(nye, "%j"))
  foy <- jday / nye_jday

  f_day_of_decade <- as.Date(paste(substr(yr, 1, 3), "0-01-01", sep = ""))
  l_day_of_decade <- as.Date(paste(substr(yr, 1, 3), "9-12-31", sep = ""))
  days_in_decade <- as.numeric(l_day_of_decade - f_day_of_decade)
  days_into_decade <- as.numeric(cov_dates - f_day_of_decade)
  fod <- days_into_decade / days_in_decade

  long_t <- cov_newmoons - t0
  flt <- long_t / max(long_t)

  cov_data <- data.frame(time = cov_newmoons, flt, fod, foy)


  params <- c("b0" = -0.0001, "b1" = -0.001, "b2" = 0.012, "b3" = -0.01,
              "b4" = -0.015, "b5" = 0.01,
              "b6" = 0.07, "b7" = 0.07,
              "Z_tau" = 80, 
              "rho" = 0.8,
              "Z.0" = 3.6) 

  mod <- pomp(data = obs_data, times = "time", t0 = t0,
              covar = covariates, tcovar = "time",
              rprocess = rprocess, rmeasure = rmeasure, dmeasure = dmeasure,
              paramnames = names(params), params = params,
              statenames = statenames,
              toEstimationScale = toEstimationScale,
              fromEstimationScale = fromEstimationScale)

  



  npfs <- 1000
  Z0s <- runif(npfs, 3.6, 5)
  b0s <- runif(npfs, -0.1, 0.1)
  b1s <- runif(npfs, -0.001, -0.001)
  b2s <- runif(npfs, -0.012, -0.012)
  b3s <- runif(npfs, -0.01, -0.01)
  b4s <- runif(npfs, -0.015, -0.015)
  b5s <- runif(npfs, 0.01, 0.01)
  b6s <- runif(npfs, 0.07, 0.07)
  b7s <- runif(npfs, 0.07, 0.07)
  Z_taus <- runif(npfs, 10, 200)
  rhos <- runif(npfs, 0.5, 0.95)

  ep <- data.frame(Z0s, b0s, b1s, b2s, b3s, b4s, b5s, b6s, b7s, Z_taus, rhos)
  colnames(ep) <- c("Z0", "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "Z_tau", "rho")
 
  #NPops <- c(500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000)
  #NPs <- sample(NPops, npfs, replace = T)
  NPs <- rep(5000, npfs)

  ncores <- 6
  doParallel::registerDoParallel(ncores)

  lls <- foreach::foreach(i = 1:nrow(ep), .combine = rbind, .errorhandling = "pass", 
                           .packages = c("pomp", "magrittr")) %dopar% {
     pfilter(mod, params = c(Z_tau = ep$Z_tau[i], b0 = ep$b0[i], b1 = ep$b1[i], b2 = ep$b2[i], 
                              b3 = ep$b3[i], b4 = ep$b4[i],  b5 = ep$b5[i], b6 = ep$b6[i], 
                             b7 = ep$b7[i], Z.0 = ep$Z0[i], rho = ep$rho[i]), 
             Np = NPs[i])$loglik
  }

  stopImplicitCluster()

  llsp <- rep(NA, npfs)
  for(i in 1:npfs){
    llsp[i] <- as.numeric(lls[i,1][[1]][1])
  }
lls<-llsp

par(mfrow = c(5,2), mar = c(5,4,1,1))
plot(ep$Z0, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$Z_tau, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$b1, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$b2, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$b3, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$b4, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$b5, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$b6, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$b7, lls, col = rgb(0.5, 0.5, 0.5, 0.3))
plot(ep$rho, lls, col = rgb(0.5, 0.5, 0.5, 0.3))



  nifs <- 12
  Z0s <- runif(nifs, 3.6, 5)
  b0s <- runif(nifs, -0.1, 0.1)
  b1s <- runif(nifs, -0.1, 0.1)
  b2s <- runif(nifs, -0.1, 0.1)
  b3s <- runif(nifs, -0.1, 0.1)
  b4s <- runif(nifs, -0.1, 0.1)
  b5s <- runif(nifs, -0.1, 0.1)
  b6s <- runif(nifs, -0.1, 0.1)
  b7s <- runif(nifs, -0.1, 0.1)
  Z_taus <- runif(nifs, 10, 200)
  rhos <- runif(nifs, 0.5, 0.95)

  guesses <- data.frame(Z0s, Z_taus, b0s, b1s, b2s, b3s, b4s, b5s, b6s, b7s, rhos)
  colnames(guesses) <- c("Z0", "Z_tau", "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "rho")


  ncores <- 6
  doParallel::registerDoParallel(ncores)

  runs <- foreach::foreach(i = 1:nrow(guesses), .errorhandling = "pass",  
                           .packages = c("pomp", "magrittr")) %dopar% {
                 mif2(mod, 
                      start = c("Z.0" = guesses[i, "Z0"],
                                "Z_tau" = guesses[i, "Z_tau"],
                                "b0" = guesses[i, "b0"],
                                "b1" = guesses[i, "b1"],
                                "b2" = guesses[i, "b2"],
                                "b3" = guesses[i, "b3"],
                                "b4" = guesses[i, "b4"],
                                "b5" = guesses[i, "b5"],
                                "b6" = guesses[i, "b6"],
                                "b7" = guesses[i, "b7"],
                                "rho" = guesses[i, "rho"]),
                      Nmif = 100, Np = 5000, 
                      rw.sd = rw.sd(Z.0 = 0.01,
                                    Z_tau = 0.002,
                                    b0 = 0.001,
                                    b1 = 0.001,
                                    b2 = 0.001,
                                    b3 = 0.001,
                                    b4 = 0.001,
                                    b5 = 0.001,
                                    b6 = 0.001,
                                    b7 = 0.001,
                                    rho = 0.001),
                      cooling.fraction.50 = 0.25, 
                      cooling.type = "geometric",
                      transform = TRUE)  %>% 
                 continue(Nmif = 100, cooling.fraction = 0.5) %>%
                 continue(Nmif = 100, cooling.fraction = 0.75) %>%
                 continue(Nmif = 100, cooling.fraction = 0.95) -> m1
                 m1
  }



stopImplicitCluster()


























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









