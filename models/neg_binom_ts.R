####### Species level predictions #################
#total is also included, for a community-level prediction

##Negative Binomial Time Series Model

library(tscount)

neg_binom_ts=function(abundances,forecast_date,forecast_months,forecast_years,forecast_newmoons,level,num_forecast_months,CI_level) {
  
  #Note: PI is missing. It has an error in the Poison Env model which produces NA values.
  species=c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PL','PM','PP','RF','RM','RO','SF','SH','SO','total')
  allforecasts=data.frame()
  allaic=data.frame()
  #Model AIC sometimes doesn't work if species counts are low. In that case give a very large AIC.
  for(s in species) {
    species_abundance = abundances %>%
      extract2(s)
    
    if(sum(species_abundance) == 0){
      pred = zero_abund_forecast
      model_aic = 1e6
    } else {
      model=tsglm(species_abundance,model=list(past_obs=1,past_mean=12),distr="nbinom")
      pred=predict(model,num_forecast_months,level=CI_level)
      model_aic = tryCatch(summary(model)$AIC, error = function(x) {1e6})
    }
    newpred=data.frame(date=rep(forecast_date,num_forecast_months), forecastmonth=forecast_months, forecastyear=forecast_years,
                       NewMoonNumber=forecast_newmoons, currency="abundance", model=rep("NegBinom Time Series",num_forecast_months),
                       level=level, species=rep(s,num_forecast_months), estimate=pred$pred,
                       LowerPI=pred$interval[,1], UpperPI=pred$interval[,2])
    allforecasts=rbind(allforecasts,newpred)
    
    allaic = allaic %>%
      bind_rows(data.frame(date=forecast_date, model='NegBinom Time Series', currency='abundance', level=level, species=s, aic=model_aic))
  }
  
  return(list(allforecasts,allaic))
  
  }