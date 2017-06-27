#Species level time series model with the best environmental covariates chosen by AIC




pois_env_ts=function(abundances,weather_data,weathermeans,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months,CI_level) {
  
  species=c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PL','PM','PP','RF','RM','RO','SF','SH','SO','total')
  allforecasts=data.frame()
  allaic=data.frame()

#List of candiate environmental covariate models
model_covariates = list(c('MaxTemp','MeanTemp','Precipitation','NDVI'),
                        c('MaxTemp','MinTemp','Precipitation','NDVI'),
                        c('MinTemp','MaxTemp','MeanTemp','Precipitation'),
                        c('Precipitation','NDVI'),
                        c('MinTemp','NDVI'),
                        c('MinTemp'),
                        c('MaxTemp'),
                        c('MeanTemp'),
                        c('Precipitation'),
                        c('NDVI'))

for(s in species) {
  print(paste("Fitting Poisson environmental models for", s))
  species_abundance = abundances %>%
    extract2(s)
  
  if(sum(species_abundance) == 0){
    pred = zero_abund_forecast
    model_aic = 1e6
  } else {
    best_model_aic = Inf
    best_model = NA
    for(proposed_model_covariates in model_covariates){
      proposed_model = tsglm(species_abundance, model=list(past_obs=1,past_mean=12), distr="poisson",
                             xreg=weather_data[,unlist(proposed_model_covariates)], link = "log")
      #tsglm sometimes outputs an error when the time series have many 0's, in that case set the AIC
      #to Inf so this proposed model covariate set get skipped
      proposed_model_aic = tryCatch(summary(proposed_model)$AIC, error = function(x) {Inf})
      if(proposed_model_aic < best_model_aic){
        best_model = proposed_model
        best_model_aic = proposed_model_aic
      }
    }
    
    #If no best model was chosen, ie. they all had infinit AIC's due to errors in model building
    #then forecast 0's. Also make an extremely high AIC so this isn't weighted heavily in the ensemble.
    if(is.na(best_model)){
      pred = zero_abund_forecast
      model_aic = 1e6
    } else {
      pred = predict(best_model,num_forecast_months,level=CI_level,newdata=weathermeans)
      model_aic = best_model_aic
    }
  }
  newpred = data.frame(date=rep(forecast_date,num_forecast_months), forecastmonth=forecast_months, forecastyear=forecast_years,
                       NewMoonNumber=forecast_newmoons, currency="abundance", model=rep("Poisson Env",num_forecast_months),
                       level=level, species=rep(s,num_forecast_months), estimate=pred$pred,
                       LowerPI=pred$interval[,1],UpperPI=pred$interval[,2])
  allforecasts = rbind(allforecasts,newpred)
  allaic = allaic %>%
    bind_rows(data.frame(date=forecast_date, model='Poisson Env', currency='abundance', level=level, species=s, aic=model_aic))
}

return(list(allforecasts,allaic))

}
