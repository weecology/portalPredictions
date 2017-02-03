# This script extracts the rodent capture data from the latest census and summarizes it in a dataframe
# which can then be plotted against the prediction.
# EMC 2/2017


library(dplyr)

setwd('C:/Users/EC/Desktop/git/portalPredictions')
rdat = read.csv('../PortalData/Rodents/Portal_rodent.csv',na.strings = '',stringsAsFactors = F)


extract_recent_census_data = function(rdat,newperiod) {
  # this function summarizes captures from the specified period (newperiod)
  # and returns a data frame of species, count, level (all or control) and period
  
  # list of target animals
  rodentsp = c('BA','DM','DO','DS','NA','OL','OT','PE','PF','PP','PH','PI','PB','PL','PM','RM','RO','RF','SO','SF','SH')
  # create data frame of species counts and total counts on control plots
  recent_c = rdat %>% filter(period==newperiod,plot %in% c(4,5,6,7,11,13,14,17,18,24), 
                             species %in% rodentsp) %>% 
    select(period,plot,species)
  spcountC = aggregate(recent_c$species,by=list(species=recent_c$species),FUN=length)
  names(spcountC) = c('species','actual')
  controls = rbind(spcountC,c('total',sum(spcountC$actual)))
  controls$level = rep('Controls')
  
  
  
  
  # create data frame of species counts and total counts on all plots
  
  # extract data from krat exclosures, excluding any krats removed from these plots
  recent_ex = rdat %>% filter(period==newperiod,plot %in% c(2,3,8,15,19,20,21,22), 
                              species %in% rodentsp, !(species %in% c('DM','DO','DS'))) %>%
    select(period,plot,species)
  #add control counts to exclosure counts
  recent_all = rbind(recent_ex,recent_c)
  spcountA = aggregate(recent_all$species,by=list(species=recent_all$species),FUN=length)
  names(spcountA) = c('species','actual')
  allplots = rbind(spcountA,c('total',sum(spcountA$actual)))
  allplots$level = rep('All')
  
  
  # bind control data and allplot data into one data frame
  actual_counts = rbind(allplots,controls)
  actual_counts$period = rep(newperiod)
  return(actual_counts)
}


# ==========================================
# identify period number of latests census
newperiod = max(rdat$period)

actual_counts = extract_recent_census_data(rdat, newperiod)
