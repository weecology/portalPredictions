# Portal Predictions
[![Build Status](https://travis-ci.org/weecology/portalPredictions.svg?branch=master)](https://travis-ci.org/weecology/portalPredictions)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.833438.svg)](https://doi.org/10.5281/zenodo.833438)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalPredictions/master/LICENSE)

#### [Portal Forecasting Website](http://portal.naturecast.org/)

This is the main repository for predictions made on the Portal rodent census data [Portal Project](http://portal.weecology.org/).

Predictions are made and archived weekly. Approximately once a month, one of these forecasts is made immediately prior to a new trapping session (trapping occurs as close to each new moon as possible).

## How to add a new model

Modeling is driven by the [portalcasting package](https://github.com/weecology/portalcasting). New models should be added there following instructions in the ["adding a model" vignette](https://weecology.github.io/portalcasting/articles/adding_model_and_data.html).

## Docker builds

Forecasts are run using [continuous integration](https://en.wikipedia.org/wiki/Continuous_integration) based on a [docker](https://hub.docker.com/) image. This makes the builds faster and more reproducible. The image is built using the [Dockerfile](https://github.com/weecology/portalPredictions/blob/master/Dockerfile), with [v0.17.0](https://github.com/weecology/portalcasting/releases/tag/v0.17.0) of `portalcasting`.

Rebuilding of the Docker container is required to pass updates to `portalcasting` along to the executed code in the Portal Predictions pipeline. When building the image, give it two tags: `latest` and the date (as yyyy-mm-dd) using the following commands (with the actual date input):

```
sudo docker build -t weecology/portal_predictions:latest -t weecology/portal_predictions:yyyy-mm-dd . 
sudo docker push weecology/portal_predictions
```

(Windows users will not need to include the `sudo` command.)
