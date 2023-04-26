# Portal Predictions
[![Metadata Check](https://github.com/weecology/portalPredictions/actions/workflows/r.yml/badge.svg)](https://github.com/weecology/portalPredictions/actions/workflows/r.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.833438.svg)](https://doi.org/10.5281/zenodo.833438)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalPredictions/main/LICENSE)
[![NSF-1929730](https://img.shields.io/badge/NSF-1929730-blue.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=1929730)

#### [Portal Forecasting Website](https://portal.naturecast.org/)

This is the main repository for predictions made on the Portal rodent census data [Portal Project](http://portal.weecology.org/).

Predictions are made and archived weekly. Approximately once a month, one of these forecasts is made immediately prior to a new trapping session (trapping occurs as close to each new moon as possible).

The website code is now located in [its own repository](https://github.com/weecology/portal-forecast-web).

## How to add a new model

Modeling is driven by the [portalcasting package](https://github.com/weecology/portalcasting). New models should be added there following instructions in the ["adding a model" vignette](https://weecology.github.io/portalcasting/articles/adding_model_and_data.html).

## Docker builds

Forecasts are run using [continuous integration](https://en.wikipedia.org/wiki/Continuous_integration) based on a [docker](https://hub.docker.com/) image. This makes the builds faster and more reproducible. The code in this repo uses the [latest portalcasting image](https://hub.docker.com/repository/docker/weecology/portalcasting)

## Developer notes

This code runs weekly on the UF HiPerGator using a cron job on `daemon2` using Ethan White's account.
The cron job runs a version of `portal_weekly_forecast.sh` that is separate from the one in the repository, but generally just a copy of it.
The version of `portal_weekly_forecast.sh` in the repo is automatically updated to the one that is run on HiPerGator after the weekly forecasts are complete. 

`portal_weekly_forecast.sh` does the following:
* Updates the `portalPredictions` repository to it's current version
* Updates the `forecasts` repository to it's current version (this repository is used for archiving full forecasts)
* Runs the forecasts
* Pushes the results of the forecasts in the `forecasts` and `models` and `data` directories to the `portalPredictions` repository
* Pushes the results of the forecasts in the `forecasts` and `models` and `data` directories and also the `fits` directory to the `forecasts` repository
* Tests to see if forecasts ran correctly

`portal_dryrun_forecast.sh` is automatically run twice each week to check to see if the forecasts are working prior to a production run.
It does the same thing as `portal_weekly_forecast.sh` but does not actually push a release. 

The root directory for all work is `/orange/ewhite/PortalForecasts/`.

There are 4 log files:
* `portal_weekly_forecast_log.out` - the main log file for the weekly forecast
* `portal_dryrun_forecast_log.out` - the main log file for the dryrun forecast
* `testthat.log` - the log file for the tests of whether the forecasts ran correctly
* `cron.log` - the cron log file, which is just a list of SLURM submissions

If necessary to create a fresh setup this system:
1. Create a root directory (`PortalForecasts`)
2. Clone `portalPredictions` into that directory
3. Clone `forecasts` into that directory
4. Copy `portal_weekly_forecast.sh` and `portal_dryrun_forecast.sh` from `portalPredictions` into the root directory.  
