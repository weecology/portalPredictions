library(dplyr)
library(portalr)
library(ggrepel)
source("tools/forecast_tools.R")

# The length of time to look backward
prior_new_moons = 12

new_moon_file = portalr::FullPath('PortalData/Rodents/moon_dates.csv', '~')
new_moons = read.csv(new_moon_file)
most_recent_newmoon = max(new_moons$newmoonnumber)
earliest_newmoon_to_graph = most_recent_newmoon - prior_new_moons

species_of_interest = c('BA','DM','DO','PP','OT','NA')
species_names = portalr::FullPath('PortalData/Rodents/Portal_rodent_species.csv', '~') %>%
  read.csv(stringsAsFactors=FALSE) %>%
  select(species = speciescode, full_species_name = scientificname)

#Fix neotoma, and add a total entry
species_names$species[species_names$full_species_name=='Neotoma albigula'] <- "NA"
species_names = species_names %>%
  mutate(species = ifelse(full_species_name=='Neotoma albigula', 'NA', species)) %>%
  add_row(species='total', full_species_name='Total Rodents')

species_abundance = portalr::abundance(shape='flat', level='Site') %>%
  rename(actual = abundance) %>%
  mutate(level='All',currency='abundance') %>%
  left_join(new_moons, by='period') %>%
  filter(species %in% species_of_interest)

total_abundance = portalr::abundance(shape='flat', level='Site') %>%
  group_by(period) %>%
  summarise(actual=sum(abundance)) %>%
  ungroup() %>%
  mutate(level='All',currency='abundance',species='total') %>%
  left_join(new_moons, by='period')

observation_data = species_abundance %>%
  bind_rows(total_abundance) %>% 
  filter(newmoonnumber %in% earliest_newmoon_to_graph:most_recent_newmoon)

#Get the all the forecasts made during observation period
forecast_data = compile_forecasts(use_hindcasts = FALSE) %>%
  filter(newmoonnumber %in% earliest_newmoon_to_graph:most_recent_newmoon)

forecast_data$estimate = round(forecast_data$estimate, 3)
forecast_data$LowerPI = round(forecast_data$LowerPI, 3)
forecast_data$UpperPI = round(forecast_data$UpperPI, 3)

# Scale down the number of forecasts presented. Since a new one is issued every week
# or every time a commit is made, a lot of redudant forecasts are done.
# This chooses, for every calendar month, only unique initial_newmoon forecasts. If
# there are multiple forecasts in the same month with the same initial_newmoon value,
# it chooses the first one. 
# forecast_dates_to_keep = forecast_data %>%
#   select(date, initial_newmoon) %>%
#   distinct() %>%
#   mutate(year = year(date), month = month(date)) %>%
#   group_by(year, month, initial_newmoon) %>%
#   filter(date == min(date)) %>%
#   ungroup() %>%
#   mutate(keep='yes') %>%
#   select(-year, -month)

# No, only keep 1 forecast per unique initial_newmoon
forecast_dates_to_keep = forecast_data %>%
  select(date, initial_newmoon) %>%
  distinct() %>%
  group_by(initial_newmoon) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  mutate(keep='yes') 


forecast_data = forecast_data %>%
  left_join(forecast_dates_to_keep, by=c('date','initial_newmoon')) %>%
  filter(keep=='yes') %>%
  select(-keep)
##################################
  
forecast_errors = forecast_data %>%
  left_join(observation_data, by=c('species','level','currency','newmoonnumber')) %>% 
  mutate(rmse = sqrt((estimate - actual)^2))

# Sometimes there are model runs on the same day and with all the same info,
# this gets rid of those
forecast_errors = forecast_errors %>%
  distinct()

# Drop any entries that don't have an observation
# (ie. a rainy sample period)
forecast_errors = forecast_errors %>%
  filter(!is.na(actual))

###################################
forecast_date_labels = forecast_errors %>%
  group_by(date, model, species, censusdate) %>%
  summarize(y_position = mean(rmse)) %>%
  ungroup() %>%
  distinct() %>%
  filter(!is.na(censusdate)) %>%
  group_by(date, model, species) %>%
  filter(as.Date(censusdate) == min(as.Date(censusdate))) %>%
  ungroup() %>%
  distinct() %>%
  mutate(label_text = paste0('Issued: ',date))


####################################
# Setup a nice palette for when catagories become greater than 9
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
#Randomize pallette so nearby regions are not similar colors
set.seed(2)
large_color_palette = sample(getPalette(40))
#####################################
ggplot(forecast_errors, aes(x=censusdate, y=rmse, group=as.character(date), color=as.character(date))) +
  #geom_jitter(size=2) +
  geom_point()+
  geom_line() +
  geom_label_repel(data=forecast_date_labels, fill='grey90',force=10, aes(label=label_text, y=y_position)) +
  #scale_color_brewer(palette='Dark2') +
  scale_color_manual(values=large_color_palette) +
  facet_wrap(model~species) + 
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x='Sample Date',y='RMSE')
