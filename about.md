---
title: "About Portal Forecasts"
---

<br>

# Contributors

[Ethan P. White](https://orcid.org/0000-0001-6728-7745), [Glenda M. Yenni](https://orcid.org/0000-0001-6969-1848), [Shawn D. Taylor](https://orcid.org/0000-0002-6178-6903), [Erica M. Christensen](https://orcid.org/0000-0002-5635-2502), [Ellen K. Bledsoe](https://orcid.org/0000-0002-3629-7235), [Juniper L. Simonis](https://orcid.org/0000-0001-9798-0460), and [S. K. Morgan Ernest](https://orcid.org/0000-0002-6026-8530)

<br>

# Ecological Forecasting

Most forecasts for the future state of ecological systems are conducted once and never updated or assessed. As a result, many available ecological forecasts are not based on the most up-to-date data, and the scientific progress of ecological forecasting models is slowed by a lack of feedback on how well the forecasts perform. Iterative near-term ecological forecasting involves repeated daily to annual scale forecasts of an ecological system and regular assessment of the resulting predictions as new data become available. More frequent updating and assessment will advance ecological forecasting as a field by accelerating the identification of the best models for individual forecasts and improving our understanding of how to best design forecasting approaches for ecology in general. 

<br>

# The Portal Project

[The Portal project](http://portal.weecology.org/), located in the Chihuahuan desert of southern Arizona, is a long-term experimental monitoring project in desert ecology. Established in 1977 by Jim Brown, we have over 40 years of data on rodents, plants, ants, and weather at the site. Rodent data are collected approximately monthly, an ideal scenario for short-term forecasts of rodent abundance.

<br>

# Automated Predictions

Code and data underlying the predictions are housed in a [GitHub repository](https://github.com/weecology/portalPredictions), continuous integration and automated running of the code occurs via [Travis](https://travis-ci.org/weecology/portalPredictions), and completed forecasts are automatically archived on [Zenodo](https://doi.org/10.5281/zenodo.2581421). The translation of the raw [Portal Data](https://github.com/weecology/PortalData) into model-ready formats is done via the [portalr package](https://github.com/weecology/Portalr) and the [portalcasting package](https://github.com/weecology/Portalcasting) is used to connect the data to the models, execute the models, synthesize the predictions, and produce the output figures.

For further detail, see our manuscript in [*Methods in Ecology and Evolution*]( https://doi.org/10.1111/2041-210X.13104).

<br>

# Acknowledgements 

This research was supported in part by the [National Science Foundation](http://nsf.gov/) through grant [DEB-1622425](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1622425) to S.K.M. Ernest and by the [Gordon and Betty Moore Foundation's Data-Driven Discovery Initiative](http://www.moore.org/programs/science/data-driven-discovery) through [Grant GBMF4563](http://www.moore.org/grants/list/GBMF4563) to E. P. White. 

We thank Henry Senyondo for help with continuous integration, Hao Ye for feedback on documents and code, Heather Bradley for logistical support, John Abatzoglou for assistance with climate forecasts, and James Brown for establishing the Portal Project.