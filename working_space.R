
model_1=forecast(auto.arima(abundances$total,lambda = 0),h=num_forecast_months,level=CI_level,fan=T)
model_2=forecast(auto.arima(abunds,lambda = 0),h=num_forecast_months,level=CI_level,fan=T)
model_3=forecast(auto.arima(interpolated_abundances,lambda = 0),h=num_forecast_months,level=CI_level,fan=T)


# interpolate missing data

moons <- (min(abundances$newmoonnumber)):(max(abundances$newmoonnumber))
abunds <- rep(NA, length(moons))
for(i in 1:length(moons)){
  if(length(which(abundances$newmoonnumber == moons[i])) > 0){
    abunds[i] <- abundances$total[abundances$newmoonnumber == moons[i]]
  }
}

interpolated_abundances <- na.interp(abunds)


species_abundance

abunds <- rep(NA, length(moons))
for(i in 1:length(moons)){
  if(length(which(abundances$newmoonnumber == moons[i])) > 0){
    abunds[i] <- species_abundance[abundances$newmoonnumber == moons[i]]
  }
}
interpolated_abundances <- na.interp(abunds)


      model=tsglm(species_abundance,model=list(past_obs=1,past_mean=12),distr="nbinom", link = "log")
      model2=tsglm(abunds,model=list(past_obs=1,past_mean=12),distr="nbinom", link = "log")
      model3=tsglm(interpolated_abundances,model=list(past_obs=1,past_mean=12),distr="nbinom", link = "log")
