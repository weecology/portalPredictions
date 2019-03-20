# Model Changes

### Setting up pevGARCH to properly hindcast
*2019-03-20*

`pevGARCH()` was not set up for hindcasting within the pipeline based off
of the historical covariate forecasts. There's now smooth integration
via a few additional elements in the metadata list (`covariate_source`
and `covariate_date_made`) which are only presently used for hindcasts
but may prove to be useful with future models in forecast mode as well.

In [portalcasting 0.6.0](https://github.com/weecology/portalcasting/releases/tag/v0.6.0)

[Relevant PR](https://github.com/weecology/portalPredictions/pull/324)

### Addition of nbsGARCH
*2019-03-19*

In [portalcasting 0.5.0](https://github.com/weecology/portalcasting/releases/tag/v0.5.0)

[Relevant PR](https://github.com/weecology/portalPredictions/pull/323)

### Addition of species-level fits for AutoArima and ESSS
*2018-12-12*

With the migration of code to [portalcasting](https://github.com/weecology/portalcasting

[Relevant PR](https://github.com/weecology/portalPredictions/pull/295)

### Removal of Box-Cox transformation in AutoArima
*2018-03-20*

The Box-Cox transformation was removed from the AutoArima model
to have all of the models be predicting the same data.

[Relevant PR](https://github.com/weecology/portalPredictions/pull/260)

### Switch predictions of covariates
*2018-03-15*

Covariate (weather and NDVI) were changed from recent 
averages to predicted values. We are currently using downscaled ENSMEAN 
climate predictions and a local NDVI forecast (using a seasonal auto ARIMA).

[Relevant PR](https://github.com/weecology/portalPredictions/pull/251)

### Add intercept-only option in pevGARCH
*2018-02-08*

Associated with the shift to newmoons, the pevGARCH model set
was expanded to include an intercept-only model. 

[Relevant PR](https://github.com/weecology/portalPredictions/pull/221)

### Shift to newmoon-based modeling & interpolate missing values
*2018-02-08*

In Feb 2018, the models were edited to work on the newmoon numbers, thereby 
requiring acknowledgment of missing surveys (i.e. newmoons where a complete 
survey was not conducted). Because the models do not handle missing values, we
decided to interpolate missing data for the time being. 

Relevant PRs:
[ESSS](https://github.com/weecology/portalPredictions/pull/196),
[AutoArima](https://github.com/weecology/portalPredictions/pull/197),
[nbGARCH](https://github.com/weecology/portalPredictions/pull/207), and 
[pevGARCH](https://github.com/weecology/portalPredictions/pull/212).





