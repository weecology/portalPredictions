---
title: "Current Models"
bibliography: refs.bibtex
---

We currently analyze and forecast rodent data at Portal using ten models:

<br>

## ESSS

ESSS (Exponential Smoothing State Space) is a flexible exponential smoothing 
state space model [@Hyndman2008] fit to the data at the composite (full site
and just control plots) spatial level and both the composite (community) and 
the articulated (species) ecological levels. The model is selected and fitted 
using the `ets` and `forecast` functions in the **forecast** package 
[@Hyndman2017] with the `allow.multiplicative.trend` argument set to `TRUE` 
and the `ESSS` function in our 
[**portalcasting** package](https://github.com/weecology/portalcasting). 
Models fit using `ets` implement what is known as the "innovations" approach 
to state space modeling, which assumes a single source of noise that is 
equivalent for the process and observation errors [@Hyndman2008].

In general, ESSS models are defined according to three model structure 
parameters: error type, trend type, and seasonality type [@Hyndman2008].
Each of the parameters can be an N (none), A (additive), or M (multiplicative)
state [@Hyndman2008]. However, because of the difference in period 
between seasonality and sampling of the Portal rodents combined with the 
hard-coded single period of the `ets` function, we could not include the 
seasonal components to the ESSS model. ESSS is fit flexibly, such that the 
model parameters can vary from fit to fit.

<br>

## AutoArima

AutoArima (Automatic Auto-Regressive Integrated Moving Average) is a 
flexible Auto-Regressive Integrated Moving Average (ARIMA) model fit to the 
data at the composite (full site and just control plots) spatial level
and both the composite (community) and the articulated (species) ecological 
levels. The model is selected and fitted using the `auto.arima` and `forecast`
 functions in the **forecast** package [@Hyndman2013; @Hyndman2017] and the 
`AutoArima` function in our 
[**portalcasting** package](https://github.com/weecology/portalcasting).

Generally, ARIMA models are defined according to three model structure 
parameters: the number of autoregressive terms (p), the degree of differencing 
(d), and the order of the moving average (q), and are represented as 
ARIMA(p, d, q) [@Box1970]. While the `auto.arima` function allows for seasonal
models, the seasonality is hard-coded to be on the same period as the sampling,
which is not the case for the Portal rodent surveys. As a result, no seasonal 
models were evaluated. AutoArima is fit flexibly, such that the model 
parameters can vary from fit to fit.

<br>

## NaiveArima

NaiveArima (Naive Auto-Regressive Integrated Moving Average) is a fixed
Auto-Regressive Integrated Moving Average (ARIMA) model of order (0,1,0) 
fit to the data at the composite (full site and just control plots) spatial
level and both the composite (community) and the articulated (species) 
ecological levels. The model is selected and fitted using the `Arima` 
and `forecast` functions in the **forecast** package 
[@Hyndman2013; @Hyndman2017] and the 
`NaiveArima` function in our 
[**portalcasting** package](https://github.com/weecology/portalcasting).

<br>

## nbGARCH

nbGARCH (Negative Binomial Auto-Regressive Conditional Heteroskedasticity) 
is a generalized autoregressive conditional heteroskedasticity (GARCH) 
model with overdispersion (*i.e.*, a negative binomial response) fit to the 
data at the composite (full site and just control plots) spatial level and both 
the composite (community) and the articulated (species) ecological levels. The 
model for each species and the community total is selected and fitted using the
`tsglm` function in the **tscount** package [@Liboschik2017a] and the 
`nbGARCH` function in our
[**portalcasting** package](https://github.com/weecology/portalcasting). 

GARCH models are generalized ARMA models and are defined according to their 
link function, response distribution, and two model structure parameters: the 
number of autoregressive terms (p) and the order of the moving average (q), 
and are represented as GARCH(p, q) [@Liboschik2017a]. The nbGARCH model is fit
using the log link and a negative binomial response (modeled as an
over-dispersed Poisson), as well as with p = 1 (first-order autoregression)
and q = 12 (approximately yearly moving average).

The `tsglm` function in the **tscount** package [@Liboschik2017a] uses a 
(conditional) quasi-likelihood based approach to inference and models the 
overdispersion as an additional parameter in a two-step approach. This 
two-stage approach has only been minimally evaluated, although preliminary 
simulation-based studies are promising [@Liboschik2017b].    

<br>

## nbsGARCH

nbsGARCH (Negative Binomial Seasonal Auto-Regressive Conditional 
Heteroskedasticity) is a generalized autoregressive conditional 
heteroskedasticity (GARCH) model with overdispersion (*i.e.*, a negative 
binomial response) with seasonal predictors modeled using two Fourier 
series terms (sin and cos of the fraction of the year) fit to the 
data at the composite (full site and just control plots) spatial level and both 
the composite (community) and the articulated (species) ecological levels. The 
model for each species and the community total is selected and fitted using the
`tsglm` function in the **tscount** package [@Liboschik2017a] and the 
`nbsGARCH` function in our
[**portalcasting** package](https://github.com/weecology/portalcasting). 

GARCH models are generalized ARMA models and are defined according to their 
link function, response distribution, and two model structure parameters: the 
number of autoregressive terms (p) and the order of the moving average (q), 
and are represented as GARCH(p, q) [@Liboschik2017a]. The nbsGARCH model is fit
using the log link and a negative binomial response (modeled as an
over-dispersed Poisson), as well as with p = 1 (first-order autoregression)
and q = 12 (approximately yearly moving average).

The `tsglm` function in the **tscount** package [@Liboschik2017a] uses a 
(conditional) quasi-likelihood based approach to inference and models the 
overdispersion as an additional parameter in a two-step approach. This 
two-stage approach has only been minimally evaluated, although preliminary 
simulation-based studies are promising [@Liboschik2017b].    

<br>


## pevGARCH

pevGARCH (Poisson Environmental Variable Auto-Regressive Conditional
Heteroskedasticity) is a generalized autoregressive conditional
heteroskedasticity (GARCH) model fit to the data at the composite (full site 
and just control plots) spatial level and both the composite (community) and 
the articulated (species) ecological levels. The response variable is Poisson, 
and a variety of environmental variables are considered as covariates. The 
model for each species is selected and fitted using the `tsglm` function in the
**tscount** package [@Liboschik2017a] and the `pevGARCH` function in
our [**portalcasting** package](https://github.com/weecology/portalcasting). 

GARCH models are generalized ARMA models and are defined according to their 
link function, response distribution, and two model structure parameters: 
the number of autoregressive terms (p) and the order of the moving average 
(q), and are represented as GARCH(p, q) [@Liboschik2017a]. The pevGARCH model 
is fit using the log link and a Poisson response, as well as with p = 1 
(first-order autoregression) and q = 12 (yearly moving average). The 
environmental variables potentially included in the model are min, mean, and
max temperatures, precipitation, and NDVI. 

The `tsglm` function in the **tscount** package [@Liboschik2017a] uses a
(conditional) quasi-likelihood based approach to inference. This approach has
only been minimally evaluated for models with covariates, although preliminary
simulation-based studies are promising [@Liboschik2017b].  

Each species is fit using the following (nonexhaustive) sets of the 
environmental covariates:

  * max temp, mean temp, precipitation, NDVI
  * max temp, min temp, precipitation, NDVI
  * max temp, mean temp, min temp, precipitation
  * precipitation, NDVI
  * min temp, NDVI
  * min temp
  * max temp
  * mean temp
  * precipitation 
  * NDVI
  * -none-
  
The final model is an intercept-only model. The single best model of the 11 is 
selected based on AIC. 

<br>

## simplexEDM

simplexEDM (simplex projection using Empirical Dynamic Modeling) is a state-space reconstruction model adapted for forecasting and fit to the interpolated data at the composite (full site and just control plots) spatial level and both the composite (community) and the articulated (species) ecological levels. The method uses time-delay embedding to reconstruct a state-space for the dynamics underlying a time series [@Packard1980, @Takens1981]. A forecast from a point in the state space is then computed as a weighted average of the trajectories of nearest neighbors of that point, a minimal algorithm known as "simplex projection" [@Sugihara1990].

In applications to ecological time series, many of the parameters are set automatically, with the exception of the dimension of the time-delay embedding. Here, the embedding dimension ($E$) is selected as the value (between `1` and the `max_E` argument to `simplexEDM()`) that minimizes the mean absolute error over the in-sample portion of the data.

<br>

## GPEDM

GPEDM (Gaussian processes using Empirical Dynamic Modeling) is a state-space reconstruction model adapted for forecasting and fit to the interpolated data at the composite (full site and just control plots) spatial level and both the composite (community) and the articulated (species) ecological levels. . The method uses time-delay embedding to reconstruct a state-space for the dynamics underlying a time series [@Packard1980, @Takens1981]. The forecast function is approximate using Gaussian processes.

As with `simplexEDM()`, many of the parameters are fit automatically, such as the length-scale, and variance parameters (see `rEDM::block_gp()` for details). One exception is the dimension of the time-delay embedding. Here, the embedding dimension ($E$) is selected as the value (between `1` and the `max_E` argument to `GPEDM()`) that minimizes the mean absolute error over the in-sample portion of the data.

<br>

## jags_RW

jags_RW fits a hierarchical log-scale density random walk model with a Poisson observation process using the JAGS (Just Another Gibbs Sampler) infrastructure [@Plummer2003] fit to the  data at the composite (full site and just control plots) spatial level and both the composite (community) and the articulated (species) ecological levels. Similar to the NaiveArima model, jags_RW has an ARIMA order of (0,1,0), but in jags_RW, it is the underlying density that takes a random walk on the log scale, whereas in NaiveArima, it is the raw counts that take a random walk on the observation scale. The jags_RW model is rather simple, but provides a starting template and underlying machinery for more articulated models using the JAGS infrastructure. 

There are two process parameters: mu (the density of the species at the beginning of the time series) and tau (the precision (inverse variance) of the random walk, which is Gaussian on the log scale). The observation model has no additional parameters. The prior distributions for mu and tau are informed by the available data collected prior to the start of the data used in the time series. mu is normally distributed with a mean equal to the average log-scale density and a variance that is twice as large as the observed variance. Due to the presence of 0s in the data and the modeling on the log scale, an offset of `count + 0.1` is used prior to taking the log and then is removed after the reconversion (exponentiation) as `density - 0.1` (where `density` is on the same scale as `count`, but can take non-integer values). 

<br>

## Ensemble

In addition to the base models, we include a starting-point ensemble. In versions before November 2019, the ensemble was based on AIC weights, but in the shift to separating the interpolated from non-interpolated data in model fitting, we had to transfer to an unweighted average ensemble model. The ensemble mean is calculated as the mean of all model means and the ensemble variance is estimated as the sum of the mean of all model variances and the variance of the estimated mean, calculated using the unbiased estimate of sample variances.

<br>

# References
