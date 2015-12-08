#Compile the raw landsat data obtained USGS (credit: Erica)
#The raw data used a 2km x 2km square around the portal size, and gives averages for the 
#mean, 

library(dplyr)

files=c("LandsatNDVI_L5.csv","LandsatNDVI_L7_SLCoff.csv", "LandsatNDVI_L7_SLCon.csv","LandsatNDVI_L8.csv")

rawValues=data.frame()
for(thisFile in files){
  rawValues=rbind(rawValues, read.csv(thisFile))
}

rawValues = rawValues %>%
  filter(PctCloud < 40)
with(rawValues, plot(Year~JulianDay))
