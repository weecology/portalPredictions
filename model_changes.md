# Model Changes


### Removal of Box-Cox transformation in AutoARIMA

In Mar 2018, the Box-Cox transformation was removed from the AutoARIMA model
to have all of the models be predicting the same data.


### Switch predictions of covariates

In Mar 2016, the covariate (weather and NDVI) were changed from recent 
averages to predicted values. We are currently using downscaled ENSMEAN 
climate predictions and a local NDVI forecast (using a seasonal auto ARIMA).
(PR 251)

### Add intercept-only option in pevGARCH

In Feb 2018, associated with the shift to newmoons, the pevGARCH model set
was expanded to include an intercept-only model. (PR 212)

### Shift to newmoon-based modeling & interpolate missing values

In Feb 2018, the models were edited to work on the newmoon numbers, thereby 
requiring acknowledgment of missing surveys (i.e. newmoons where a complete 
survey was not conducted). Because the models do not handle missing values, we
decided to interpolate missing data for the time being. 

Four PRs are associated with these updates:
PRs 196, 197, 207, and 212 deal with the ESSS, AutoARIMA, nbGARCH, and 
pevGARCH



