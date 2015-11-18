library(dplyr)
dataFolder='~/data/portal/'


# Sean’s munging code -----------------------------------------------------

rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"), colClasses=c('tag'='character'))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))

#1st try. only estimate after 1994 when pit tags were in heavy use. 
rodents=rodents[rodents$yr>=1995,]
rodents=rodents[rodents$yr<=2010,]

#Get trapping dates for *all* periods/plots before I cull things
trappingDates=select(rodents, period, yr, mo, dy, plot) %>% distinct()
trappingDates$period=abs(trappingDates$period)

rodents=rodents[rodents$period>0,]
rodents=rodents[!is.na(rodents$plot),]
rodents=rodents[!is.na(rodents$species),]

#For resources I use the total precip of the prior n months. n is:
precipMonthLag=6


###################################################################################################
#Functions to retrieve weather data
##################################################################################################
#Several functions to ultimately build 2 final lookup tables to quicly build mark data frames.
#One for the nightly precip & low temperature for trapping probability
#One for the prior 6 months of precip for survivorship
precipRaw=read.csv(paste(dataFolder,'Hourly_PPT_mm_1989_present_fixed.csv',sep=''))


#Setup a weather dataframe to work with nightly temp and precip data. 
#Days in the capture data are always from the morning of trapping. So here I need to sum precip from the prior
#night, or get the temp info across two different days. Mean sunset/sunrise times throughout the year are roughly 6pm-6am

#Get all days in the range of the study, so that you can easily lookup any previous day
uniqueDays = unlist(strsplit(as.character(seq.Date(as.Date('1977/1/1'), as.Date('2018/1/1'), 'days')), '-'))
uniqueDays = data.frame(matrix(uniqueDays, ncol=3, byrow=TRUE))
colnames(uniqueDays)=c('Year','Month','Day')
#These days started out as Date strings, so need to convert them to integers
uniqueDays[] = lapply(uniqueDays, as.character) 
uniqueDays[] = lapply(uniqueDays, as.integer)

#Explicetly order by year, month, and day
uniqueDays=uniqueDays[with(uniqueDays, order(Year,Month,Day)),]
#Extract hourly readings for the evening hours 6pm-6am
nightlyPrecip=subset(precipRaw, (Hour>=1800 & Hour <=2400) | (Hour>=100 & Hour<=600))
#Label hours evening/morning. 
nightlyPrecip$TimeOfDay='morning'
nightlyPrecip$TimeOfDay[nightlyPrecip$Hour>1200]='evening'

####################################################
#Get the total precip from a previous night of trapping

getNightlyPrecip=function(year,month,day){
  #From the day given, get the previous days actual date from the unique list. This will account for nights spanning
  #different months, or years.
  todayIndex=which(uniqueDays$Year==year & uniqueDays$Month==month & uniqueDays$Day==day)
  yesterdayIndex=todayIndex-1
  yesterdayYear=uniqueDays$Year[yesterdayIndex]
  yesterdayMonth=uniqueDays$Month[yesterdayIndex]
  yesterdayDay=uniqueDays$Day[yesterdayIndex]
  
  morningPrecip=subset(nightlyPrecip, Year==year & Month==month & Day==day & TimeOfDay=='morning')
  morningPrecip=sum(morningPrecip$Precipitation)
  
  eveningPrecip=subset(nightlyPrecip, Year==yesterdayYear & Month==yesterdayMonth & Day==yesterdayDay & TimeOfDay=='evening')
  eveningPrecip=sum(eveningPrecip$Precipitation)
  
  return(morningPrecip+eveningPrecip)
}

#####################################################
#Get the low temp from a previous night of trapping

#Gives nightly low
getNightlyTemp=function(year, month, day){
  todayIndex=which(uniqueDays$Year==year & uniqueDays$Month==month & uniqueDays$Day==day)
  yesterdayIndex=todayIndex-1
  yesterdayYear=uniqueDays$Year[yesterdayIndex]
  yesterdayMonth=uniqueDays$Month[yesterdayIndex]
  yesterdayDay=uniqueDays$Day[yesterdayIndex]
  
  morningTemp=subset(nightlyPrecip, Year==year & Month==month & Day==day & TimeOfDay=='morning')
  #Data is missing from some days. If thats the case then get the average nightly low from
  #the prior 2 and the next 2 years.
  if(nrow(morningTemp)<6){
    morningTemp = filter(nightlyPrecip, (Year>=year-2 & year<=year+2) & Month==month & Day==day & TimeOfDay=='morning') %>%
      group_by(Hour) %>%
      summarize(TempAir=mean(TempAir))
  }
  morningTemp=min(morningTemp$TempAir)
  
  eveningTemp=subset(nightlyPrecip, Year==yesterdayYear & Month==yesterdayMonth & Day==yesterdayDay & TimeOfDay=='evening')
  if(nrow(eveningTemp)<6){
    eveningTemp = filter(nightlyPrecip, (Year>=yesterdayYear-2 & year<=yesterdayYear+2) & Month==yesterdayMonth & Day==yesterdayDay & TimeOfDay=='evening') %>%
      group_by(Hour) %>%
      summarize(TempAir=mean(TempAir))
  }
  eveningTemp=min(eveningTemp$TempAir)
  
  return(min(morningTemp, eveningTemp))
}

####################################################
#Get the total precipiation from the prior n months (most likely 6)
#A rolling n months that doesn't use 6 months seasons like most portal papers. Hopefully it's still fine. 
monthlyPrecipTotals = precipRaw %>%
  group_by(Year, Month) %>%
  summarize(precip=sum(Precipitation))

#Setup an ordered list of months to figure out lags
uniqueMonths = uniqueDays %>%
  select(Year,Month) %>%
  distinct()
#Explicetly order by year, and month
uniqueMonths=uniqueMonths[with(uniqueMonths, order(Year,Month)),]

getPrior6MonthPrecip=function(year, month){
  thisMonthIndex=which(uniqueMonths$Year==year & uniqueMonths$Month==month)
  #Say the row index for our inputed month is 150. With a lag of 6 months, this gives a list of indexes 149,148,147,146,145,144
  priorMonthsIndex=(thisMonthIndex-precipMonthLag) : (thisMonthIndex-1)
  #Now use those indexes to draw the actual year and months 
  priorYears=uniqueMonths$Year[priorMonthsIndex]
  priorMonths=uniqueMonths$Month[priorMonthsIndex]
  
  #Using the actual year and month values, now grab the total precip numbers
  
  totalPrecip=subset(precipRaw, Year %in% priorYears & Month %in% priorMonths)
  totalPrecip=sum(totalPrecip$Precipitation)
  
  return(totalPrecip)
}

########################################################################
#Setup a lookup table for nightly precip and low temps for 
#every period/plot/night combo. Takes a few minutes,
#so store it for future use. 

nightlyLookupTableFile=paste(dataFolder, 'nightlyLookup.csv',sep='')
if(file.exists(nightlyLookupTableFile)){
  nightlyLookupTable=read.csv(nightlyLookupTableFile)
} else{
  #trappingDates pulled from rodents file in the beginning
  
  #Some plots are trapped > 1 night per period. It's very rare but causes issues with this analysis.
  #Here I account for that by finding those instances, and assigning just a single date for that plot/period.
  #It could probably be written better, but whatever, it works. 
  doubleDates= trappingDates %>% group_by(period, plot) %>% summarise(count=n()) %>% filter(count>1)
  for(thisRow in 1:nrow(doubleDates)){
    thisPeriod=doubleDates$period[thisRow]
    thisPlot=doubleDates$plot[thisRow]
    dateToUse=rodents %>% filter(period==thisPeriod, plot==thisPlot) %>% group_by(yr, mo, dy) %>% summarise(count=n()) 
    dateToUse=dateToUse[which.max(dateToUse$count),]
    if(nrow(dateToUse) > 0){
      trappingDates$yr[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$yr
      trappingDates$mo[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$mo
      trappingDates$dy[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$dy
    }
  }
  trappingDates=trappingDates %>% distinct()
  
  #Get nightly temp/precip values for each plot/period 
  nightlyLookupTable = trappingDates %>%
    rowwise() %>%
    #For each date/plot, get the nightly low temp and total precip, which affect probability of trapping
    mutate(precip=getNightlyPrecip(yr,mo,dy), lowTemp=getNightlyTemp(yr,mo,dy))
  #Save file for future use
  write.csv(nightlyLookupTable, nightlyLookupTableFile, row.names=FALSE)
}
rm(nightlyLookupTableFile)

#Lookup table for resources (precip) in the last 6 months
resourceLookupTable = rodents %>%
  select(period,yr,mo) %>%
  distinct() %>%
  rowwise() %>%
  mutate(totalPrecip=getPrior6MonthPrecip(yr, mo))


#only need control and k-rat exclosure plots
controlPlots=c(2,4,8,11,12,14,17,22) #controls
kratPlots=c(3,6,13,18,19,20) #krat exclosure

# Dave’s code -------------------------------------------------------------
library(lubridate)
library(mgcv)


allPeriods=unique(rodents$period)
allPlots=c(controlPlots,kratPlots)
allPeriodPlots=expand.grid(allPeriods, allPlots)
colnames(allPeriodPlots)=c('period','plot')

# Note that yr_continuous may drift a bit with leap years, but should never be
# more than one day from the correct value
munge_species = function(sp){
    rodents %>% 
    filter(species == sp) %>%
    group_by(period, plot) %>%
    summarize(abundance = n()) %>%
    ungroup() %>%
    full_join(allPeriodPlots, c('period','plot')) %>%
    replace(is.na(.), 0) %>%
    mutate(is_control = plot %in% controlPlots) %>% 
    mutate(is_exclosure = plot %in% kratPlots) %>%
    inner_join(resourceLookupTable, c("period")) %>%
    select(-yr, -mo) %>%
    inner_join(nightlyLookupTable, c("plot", "period")) %>%
    mutate(date = as.Date(paste(yr, mo, dy, sep = "-"))) %>% 
    mutate(yday = yday(date)) %>%
    mutate(yr_continuous = julian(date, origin = as.Date("1900-01-01")) / 365.24 + 1900)
}

fit_gam = function(species){
  data = munge_species(species)
  
  # note the periodic spline for yday with knots between 1 and 365.24
  # The Poisson distribution may not be ideal for this data set.
  gam(
    abundance ~ is_control + is_exclosure + 
      s(totalPrecip) + s(precip) + s(lowTemp) + 
      s(yday, bs = "cc") + s(yr_continuous),
    data = data,
    family = poisson,
    knots=list(yday=c(1,365.24))
  )
}

# totalPrecip   : cumulative rain over previous 6 months
# precip        : rain over the previous night
# lowTemp       : lowest temp over the previous night
# yday          : periodic/seasonal trend
# yr_continuous : long-term trend

plot(fit_gam("PP"), residuals = TRUE, rug = FALSE, pages = 1, n = 1000, main = "PP", shade = TRUE, lwd = 3)
plot(fit_gam("PB"), residuals = TRUE, rug = FALSE, pages = 1, n = 1000, main = "PB", shade = TRUE, lwd = 3)
plot(fit_gam("DM"), residuals = TRUE, rug = FALSE, pages = 1, n = 1000, main = "DM", shade = TRUE, lwd = 3)
plot(fit_gam("DO"), residuals = TRUE, rug = FALSE, pages = 1, n = 1000, main = "DO", shade = TRUE, lwd = 3)
