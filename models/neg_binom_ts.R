####### Species level predictions #################
#total is also included, for a community-level prediction

##Negative Binomial Time Series Model
library(yaml)
library(tscount)

nbts=function(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months = 12, CI_level = .9) {
  
#Note: PI is missing. It has an error in the Poison Env model which produces NA values.
  species=c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PL','PM','PP','RF','RM','RO','SF','SH','SO','total')
  
  #tscount::tsglm() will not model a timeseries of all 0's. So for those species, which are
  #ones that just haven't been observed in a while, make a forecast of all 0's.
  zero_abund_forecast = list(pred=rep(0,12), interval=matrix(rep(0,24), ncol=2))
  colnames(zero_abund_forecast$interval) = c('lower','upper')
  
#Start model    
allforecasts=data.frame()
allaic=data.frame()
  
  for(s in species) {
    cat("Fitting Negative Binomial Time Series model for", s, "\n")
    species_abundance = abundances %>%
      extract2(s)
    
    if(sum(species_abundance) == 0){
      pred = zero_abund_forecast
      #Model AIC sometimes doesn't work if species counts are low. In that case give a very large AIC.
      model_aic = 1e6
    } else {
      model=tsglm(species_abundance,model=list(past_obs=1,past_mean=12),distr="nbinom", link = "log")
      pred=predict(model,num_forecast_months,level=CI_level)
      model_aic = tryCatch(summary(model)$AIC, error = function(x) {1e6})
    }
    newpred=data.frame(date=rep(forecast_date,num_forecast_months), forecastmonth=forecast_months, forecastyear=forecast_years,
                       newmoonnumber=forecast_newmoons, currency="abundance", model=rep("NegBinom Time Series",num_forecast_months),
                       level=level, species=rep(s,num_forecast_months), estimate=pred$pred,
                       LowerPI=pred$interval[,1], UpperPI=pred$interval[,2])
    allforecasts=rbind(allforecasts,newpred)
    
    allaic = allaic %>%
      bind_rows(data.frame(date=forecast_date,  currency='abundance', model='NegBinom Time Series',level=level, species=s, aic=model_aic))
  }
  
  #########Include columns describing the data used in the forecast###############
  allforecasts$fit_start_newmoon = min(abundances$newmoonnumber)
  allforecasts$fit_end_newmoon   = max(abundances$newmoonnumber)
  allforecasts$initial_newmoon   = max(abundances$newmoonnumber)
  allaic$fit_start_newmoon = min(abundances$newmoonnumber)
  allaic$fit_end_newmoon   = max(abundances$newmoonnumber)
  allaic$initial_newmoon = max(abundances$newmoonnumber)

#write results to file
return(list(allforecasts,allaic)) 
}

#Get data
all = read.csv("tools/rodent_all.csv")
controls = read.csv("tools/rodent_controls.csv")
data = yaml.load_file("tools/model.yaml")
forecast_date = as.Date(data$forecast_date)
filename_suffix = data$filename_suffix
forecast_months = data$forecast_months
forecast_years = data$forecast_years
forecast_newmoons = data$forecast_newmoons

#Forecast All plots
cat("Creating site level forecasts for negative binomial time series", "\n")
allresults = nbts(all,forecast_date,forecast_months,forecast_years,forecast_newmoons,"All")

#Forecast Control plots
cat("Creating control plot forecasts for negative binomial time series", "\n")
controlsresults = nbts(controls,forecast_date,forecast_months,forecast_years,forecast_newmoons,"Controls")

#Write results
write.csv(allresults[1],file.path('tmp', paste("nbtsAll", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(allresults[2],file.path('tmp', paste("nbtsAll", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)
write.csv(controlsresults[1],file.path('tmp', paste("nbtsControls", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(controlsresults[2],file.path('tmp', paste("nbtsControls", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)

  