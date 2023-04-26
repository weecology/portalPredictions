---
title: "About Portal Forecasts"
---

<br>
<br>

Welcome to Portal Forecasting! 

This is a website run by the [Weecology](http://weecology.org/) team, comprised of [Ethan White's](http://whitelab.weecology.org/) and [Morgan Ernest's](http://ernestlab.weecology.org/) lab groups at the [University of Florida](http://www.wec.ufl.edu/). We are a group of interdisciplinary ecologists broadly interested in collaborative approaches to empirical and computation ecology, open science, and open data.

On this website, you'll find information about our ongoing efforts to forecast a time series of rodent abundances from [The Portal project](http://portal.weecology.org/), a long-term experimental monitoring project in desert ecology. Enjoy! 

<br>

## Contributors

[Ethan P. White](https://orcid.org/0000-0001-6728-7745), [Glenda M. Yenni](https://orcid.org/0000-0001-6969-1848), [Henry Senyondo](https://orcid.org/0000-0001-7105-5808), [Shawn D. Taylor](https://orcid.org/0000-0002-6178-6903), [Erica M. Christensen](https://orcid.org/0000-0002-5635-2502), [Ellen K. Bledsoe](https://orcid.org/0000-0002-3629-7235), [Juniper L. Simonis](https://orcid.org/0000-0001-9798-0460), and [S. K. Morgan Ernest](https://orcid.org/0000-0002-6026-8530)

<br>

## Ecological Forecasting

Most forecasts for the future state of ecological systems are conducted once and never updated or assessed. As a result, many available ecological forecasts are not based on the most up-to-date data, and the scientific progress of ecological forecasting models is slowed by a lack of feedback on how well the forecasts perform. Iterative near-term ecological forecasting involves repeated daily to annual scale forecasts of an ecological system and regular assessment of the resulting predictions as new data become available. More frequent updating and assessment will advance ecological forecasting as a field by accelerating the identification of the best models for individual forecasts and improving our understanding of how to best design forecasting approaches for ecology in general. 

<br>

## The Portal Project

[The Portal project](http://portal.weecology.org/), located in the Chihuahuan desert of southern Arizona, is a long-term experimental monitoring project in desert ecology. Established in 1977 by Jim Brown, we have over 40 years of data on rodents, plants, ants, and weather at the site. Rodent data are collected approximately monthly, an ideal scenario for short-term forecasts of rodent abundance.

<br>

## Automated Predictions

The main modeling and forecasting for this project is done using the [portalcasting](https://weecology.github.io/portalcasting/) R package ([Simonis et al. 2022](https://doi.org/10.21105/joss.03220)). We use code in a separate [portalPredictions GitHub repository](https://github.com/weecology/portalPredictions) to drive the production forecasts. This code runs automatically once a week on [the University's of Florida's high performance computing system (the HiPerGator)](https://www.rc.ufl.edu/get-started/hipergator/)) and completed forecasts are automatically archived to [Zenodo](https://doi.org/10.5281/zenodo.2581421). The translation of the raw [Portal Data](https://github.com/weecology/PortalData) into model-ready formats is done via the [portalr package](https://github.com/weecology/Portalr) and the [portalcasting package](https://github.com/weecology/Portalcasting) is used to connect the data to the models, execute the models, synthesize the predictions, and produce the output figures.

For further a big picture overview of the system see [our paper on this forecasting system (White et al. 2019)](https://doi.org/10.1111/2041-210X.13104), but note that due to increased computational demands of the growing model suite and [Travis CI's changes in fee structure](https://daniel.haxx.se/blog/2021/06/14/bye-bye-travis-ci/), we no longer use Travis CI to run the predictions.

<br>

## Acknowledgements 

This project is developed in active collaboration with [DAPPER Stats](https://www.dapperstats.com/).

This research was supported in part by the [National Science Foundation](http://nsf.gov/) through grant [DEB-1929730](hhttps://www.nsf.gov/awardsearch/showAward?AWD_ID=1929730) to S.K.M. Ernest and by the [Gordon and Betty Moore Foundation's Data-Driven Discovery Initiative](http://www.moore.org/programs/science/data-driven-discovery) through [Grant GBMF4563](http://www.moore.org/grants/list/GBMF4563) to E. P. White. 

We thank Hao Ye for feedback on documents and code, Heather Bradley for logistical support, John Abatzoglou for assistance with climate forecasts, and James Brown for establishing the Portal Project.

<br>

<img src="portalcasting.png" alt="hexagon software logo, light grey blue background, basic lettering at the top says portalcasting, main image is a drawn all black rodent standing on two feet with a fishing rod in hand and a brown fishing hat on head, standing next to a tan and green tackle box." width="200px">   
<img src="portalr.png" alt="hexagon software logo, divided into three squares (left, right, bottom) that make it look like a cut-out of a room, with an orange oval on the floor and a rat coming through it and a blue oval on the wall with a rat butt leaving it. the word portalr is on the right square." width="200px">   
