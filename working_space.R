# interpolate missing data

moons <- (min(abundances$newmoonnumber)):(max(abundances$newmoonnumber))
abunds <- rep(NA, length(moons))
for(i in 1:length(moons)){
  if(length(which(abundances$newmoonnumber == moons[i])) > 0){
    abunds[i] <- abundances$total[abundances$newmoonnumber == moons[i]]
  }
}

interpolated_abundances <- na.interp(abunds)

