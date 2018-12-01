# Portal Predictions
[![Build Status](https://travis-ci.org/weecology/portalPredictions.svg?branch=master)](https://travis-ci.org/weecology/portalPredictions)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.833438.svg)](https://doi.org/10.5281/zenodo.833438)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalPredictions/master/LICENSE)

#### [Portal Forecasting Website](http://portal.naturecast.org/)

This is the main repository for predictions made on the Portal rodent census data [Portal Project](http://portal.weecology.org/).

Predictions are made and archived weekly. Approximately once a month, one of these forecasts is made immediately prior to a new trapping session (trapping occurs as close to each new moon as possible).

## How to add a new model

For details on adding a model to the current set, see the [Adding a new model wiki page](https://github.com/weecology/portalPredictions/wiki/Adding-a-new-model).

## Docker builds

Forecasts are run using Travis CI based on a docker image. This makes the builds
faster and more reproducible. When adding new packages to this repo it may be
necessary to update the Docker container. When building, please tag the image with 
the `latest` tag, as well as a named tag for the date (yyyy-mm-dd).
Use the following commands:

```
sudo docker build -t weecology/portal_predictions:latest -t weecology/portal_predictions:yyyy-mm-dd . 
sudo docker push weecology/portal_predictions
```
