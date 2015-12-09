#Compile the raw landsat data obtained USGS
#The raw data used a 2km x 2km square around the portal size, and gives averages for the 
#mean, 

library(dplyr)

#Read in raw data file and add formatted date
rawData=read.csv('./NDVI/ModisNDVI_MOD_clean_summary.csv', skip=8) %>%
  mutate(date=as.Date(paste(year,julianday,sep='-'), format='%Y-%j')) %>%
  mutate(month=as.numeric(format(date, '%m')))

monthlyMean=rawData %>%
  group_by(year,month) %>%
  summarize(ndvi=mean(mean))

write.csv(monthlyMean, './NDVI/ModisMonthlyNDVI.csv')
