# Portal Predictions
[![Build Status](https://travis-ci.org/weecology/portalPredictions.svg?branch=master)](https://travis-ci.org/weecology/portalPredictions)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.833438.svg)](https://doi.org/10.5281/zenodo.833438)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalPredictions/master/LICENSE)

This is the main repository for predictions made on the Portal rodent census data [Portal Project](http://portal.weecology.org/).

Predictions are made approximately monthly, immediately prior to a new trapping session (trapping occurs as close to each new moon as possible.

Portalforecasts.R is the main function used to produce forecasts. Multiple models are used for each new forecast, and are descibed in their respective files. 

Models currently in use are in the models directory. Models that were used or we're developing but not currently using are under DevelopingModels.

Predictions contains all predictions made thusfar and model aics. Each new prediction made on a particular time series (varies by level and currency) is saved in a new file in this directory.

