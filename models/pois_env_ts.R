####### Species level predictions #################
#Species level time series model with the best environmental covariates chosen by AIC

pois_env_ts=function(abundances, weather_data, weathermeans, forecast_date, forecast_months, forecast_years,
                     forecast_newmoons, level, num_forecast_months = 12, CI_level = .9) {
  
  species=c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PL','PM','PP','RF','RM','RO','SF','SH','SO','total')
  allforecasts=data.frame()
  allaic=data.frame()
  
  #For all-zero time series, make a forecast of all 0's.
  zero_abund_forecast = list(pred=rep(0,12), interval=matrix(rep(0,24), ncol=2))
  colnames(zero_abund_forecast$interval) = c('lower','upper')

#List of candiate environmental covariate models
model_covariates = list(c('maxtemp','meantemp','precipitation','NDVI'),
                        c('maxtemp','mintemp','precipitation','NDVI'),
                        c('mintemp','maxtemp','meantemp','precipitation'),
                        c('precipitation','NDVI'),
                        c('mintemp','NDVI'),
                        c('mintemp'),
                        c('maxtemp'),
                        c('meantemp'),
                        c('precipitation'),
                        c('NDVI'))

for(s in species) {
  cat("Fitting Poisson environmental models for", s, "\n")
  species_abundance = abundances %>%
    extract2(s)
  
  if(sum(species_abundance) == 0){
    pred = zero_abund_forecast
    model_aic = 1e6
  } else {
    best_model_aic = Inf
    best_model = NA
    model_count = 1
    for(proposed_model_covariates in model_covariates){
      cat("    Fitting Model", model_count, "\n")
      proposed_model = tsglm(species_abundance, model=list(past_obs=1,past_mean=12), distr="poisson",
                             xreg=weather_data[,unlist(proposed_model_covariates)], link = "log")
      #tsglm sometimes outputs an error when the time series have many 0's, in that case set the AIC
      #to Inf so this proposed model covariate set get skipped
      proposed_model_aic = tryCatch(summary(proposed_model)$AIC, error = function(x) {Inf})
      if(proposed_model_aic < best_model_aic){
        best_model = proposed_model
        best_model_aic = proposed_model_aic
      }
      model_count = model_count + 1
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
                       newmoonnumber=forecast_newmoons, currency="abundance", model=rep("Poisson Env",num_forecast_months),
                       level=level, species=rep(s,num_forecast_months), estimate=pred$pred,
                       LowerPI=pred$interval[,1],UpperPI=pred$interval[,2])
  allforecasts = rbind(allforecasts,newpred)
  allaic = allaic %>%
    bind_rows(data.frame(date=forecast_date, model='Poisson Env', currency='abundance', level=level, species=s, aic=model_aic))
}

return(list(allforecasts,allaic))

}

#Get data
  all = read.csv("tools/rodent_all.csv")
  controls = read.csv("tools/rodent_controls.csv")
  weather = read.csv("tools/weather_data.csv")
  data = yaml.load_file("tools/model.yaml")
  forecast_date = as.Date(data$forecast_date)
  filename_suffix = data$filename_suffix
  forecast_months = data$forecast_months
  forecast_years = data$forecast_years
  forecast_newmoons = data$forecast_newmoons
  
##Use monthly weather means for past 3 years for weather forecast
  
  weathermeans=weather[dim(weather)[1]-36:dim(weather)[1],] %>%
    group_by(month) %>% summarise_all(funs(mean(., na.rm=TRUE))) %>%
    select(-c(year,newmoonnumber,NewMoonNumber_with_lag)) %>% slice(match(forecast_months, month))
  
#Forecast All plots
allresults = pois_env_ts(all,weather,weathermeans,forecast_date,forecast_months,forecast_years,forecast_newmoons,"All")

#Forecast Control plots
controlsresults = pois_env_ts(controls,weather,weathermeans,forecast_date,forecast_months,forecast_years,forecast_newmoons,"Controls")

#Write results
write.csv(allresults$allforecasts,file.path('tmp', paste("petsAll", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(allresults$allaic,file.path('tmp', paste("petsAll", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)
write.csv(controlsresults$allforecasts,file.path('tmp', paste("petsControls", filename_suffix, ".csv", sep="")),row.names = FALSE)
write.csv(controlsresults$allaic,file.path('tmp', paste("petsControls", filename_suffix, "_model_aic.csv", sep="")),row.names = FALSE)
