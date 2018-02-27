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

abundances = all

first_newmoon <- min(abundances$newmoonnumber)
last_newmoon <- max(abundances$newmoonnumber)
obs_newmoons <- first_newmoon:last_newmoon
obs_counts <- rep(NA, length(obs_newmoons))
for(i in 1:nrow(abundances)){
  which_nm <- which(obs_newmoons == abundances$newmoonnumber[i])
  obs_counts[which_nm] <- abundances$total[i]
}
cov_newmoons <- (first_newmoon - 1):last_newmoon
cov_foy <- rep(NA, length(cov_newmoons))
for(i in 1:length(cov_newmoons)){
  spec_nm <- which(moons$newmoonnumber == cov_newmoons[i])
  spec_date <- NA
  spec_date <- moons$newmoondate[spec_nm]
  if(is.na(spec_date)){
    spec_date <- moons$newmoondate[spec_nm]
  }
  spec_date <- as.Date(as.character(spec_date))
  jday <- as.numeric(format(spec_date, "%j"))
  yr <- format(spec_date, "%Y")
  nye <- as.Date(paste(yr, "-12-31", sep = ""))
  nye_jday <- as.numeric(format(nye, "%j"))
  cov_foy[i] <- jday / nye_jday
}

process.sim <- "
double change = b1 * cos(M_2PI * foy) + b2 * sin(M_2PI * foy);
Z = Z + change;
"

sample.sim <- "
Y = rnorm(Z, 0);
"

dd <- data.frame(time = obs_newmoons, Y = obs_counts)
cc <- data.frame(time = cov_newmoons, foy = ((216:500) %% (12.5))/12.5)


mod <- pomp(data = dd, times = "time", t0 = 216,
covar = cc, tcovar = "time",
rprocess = discrete.time.sim(step.fun = Csnippet(process.sim), delta.t = 1),
rmeasure = Csnippet(sample.sim),
params = c(b1 = 0.002, b2 = 0.001, Z.0 = 1), 
paramnames = c("b1", "b2", "Z.0"), statenames = c("Z"))

plot(simulate(mod))





