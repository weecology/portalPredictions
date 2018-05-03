library(tidyverse)

######################################
# This processes the original climate hindcasts from John Abatzoglou to one more akin to portal weather observations
# The original format had a column for every climate model and lead time (up to 7 months in advance)
# 
# Note there are occasional missing values
# Citation for data: https://doi.org/10.1175/WAF-D-16-0117.1
######################################

temp_hindcasts = read_csv('data/climate_hindcasts_original_data/Taylor_319555N1090744W_NMME_tmean.csv') %>%
  gather(model_name, meantemp, -year,-month)
precip_hindcast = read_csv('data/climate_hindcasts_original_data/Taylor_319555N1090744W_NMME_precipitation.csv') %>%
  gather(model_name, precipitation, -year,-month)

formatted_hindcasts = temp_hindcasts %>%
  left_join(precip_hindcast, c('year','month','model_name')) %>%
  # Split model name and lead time to 2 columns
  separate(model_name, c('model_name','lead_time_months'), sep=-1, extra='merge', convert = TRUE) %>%
  # Drop trailing underscore
  mutate(model_name = stringr::str_sub(model_name, start=1, end=-2)) %>% 
  mutate(date = as.Date(paste(year,month,'15',sep='-'))) %>%
  mutate(issue_date = date - months(lead_time_months)) %>%
  mutate(issue_year = lubridate::year(issue_date), issue_month=lubridate::month(issue_date)) %>%
  select(-date, -issue_date) %>%
  rename(climate_model = model_name)

write_csv(formatted_hindcasts, 'data/climate_hindcasts.csv')
