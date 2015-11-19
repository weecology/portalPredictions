library(dplyr)
library(marked)

dataFolder='~/data/portal/'

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
    trappingDates$yr[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$yr
    trappingDates$mo[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$mo
    trappingDates$dy[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$dy
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

rm(monthlyPrecipTotals, nightlyPrecip, precipRaw, uniqueDays, uniqueMonths, getNightlyTemp, getNightlyPrecip, getPrior6MonthPrecip, doubleDates,
   thisPeriod, thisPlot, thisRow, dateToUse)
##########################################################################################################
#Functions to setup the data frame capture history + exogounous variables
#########################################################################################################

###########################################################
#Take a portal dataframe and get a "mark" formatted capture history for each individual. 
#does not do any processing for invalid periods, multiple spp, etc.
processCH=function(df){
  periods=sort(unique(df$period))
  periods=min(periods):max(periods)
  tagList=sort(unique(df$tag))
  tagList=tagList[!is.na(tagList)]
  tagList=tagList[tagList!='0']
  tagList=tagList[tagList!='000000']
  tagCHList=c()
  
  for(tag in tagList){
    tag=as.character(tag)
    #Get a list of periods this tag was seen in. 
    thisTagPeriods=df$period[df$tag==tag]
    thisTagPeriods=thisTagPeriods[!is.na(thisTagPeriods)] #The list is full of NA's! What?! why do I have to do this? screw you R
    #convert list of seen/not seen periods to '00001001000111000.....' etc. and add it to the list of all tags
    tagCHList=c(tagCHList, as.character(paste((periods %in% thisTagPeriods)*1, collapse='')))
    
    #Pack into a dataframe and ensure capture histories are set as characters
    dfCH=data.frame(ch=tagCHList)
    dfCH[] = lapply(dfCH, as.character)
  }
  return(dfCH)
  
}

#############################################################
#Mode of a vector of numbers, used for various things
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
###########################################################
#Get various weather variables for a particular period/plot
#Don't think I actually use this
getPlotWeatherInfo=function(period, plot){
  dayOfTrapping=unique(rodents[rodents$period==300 & rodents$plot==5 , c('yr','dy','mo','period','plot')])
  if(nrow(dayOfTrapping)>1){stop('trapping happend over >1 days!')}
  
  #dot = day of trapping
  dotYear=dayOfTrapping$yr
  dotMonth=dayOfTrapping$mo
  dotDay=dayOfTrapping$dy
  
  nightlyTemp=getNightlyTemp(dotYear, dotMonth, dotDay)
  nightlyPrecip=getNightlyPrecip(dotYear, dotMonth, dotDay)
  prior6Precip=getPrior6MonthPrecip(dotYear, dotMonth, precipMonthLag)
  
  return(list(nightlyTemp=nightlyTemp, nightlyPrecip=nightlyPrecip, prior6MonthPrecip=prior6MonthPrecip))
}

#########################################################
#Combine capture history and nightly weather data of an arbitrary rodent subset
#This function combines everything above to make a data frame that RMark, or marked will work with. it looks like this.
#ch, nightlyPrecip1, nightlyPrecip2, nightlyPrecip3, ...... nightlyTemp1, nightlyTemp2, nightlyTemp3,......
#0010101110....., 0, 0, .3, ....... 14, 12, 14
createMarkDF=function(rodentDF){
  ch=processCH(rodentDF)
  tagInfo=rodentDF %>% select(tag, period, plot)

  periods=sort(unique(tagInfo$period))
  periods=min(periods):max(periods)
  tagList=unique(tagInfo$tag)
  tagList=tagList[!is.na(tagList)]
  tagList=tagList[tagList!='0']
  tagList=tagList[tagList!='000000']
  tagList=sort(tagList)

  weatherDF=data.frame(matrix(data=NA, nrow=length(tagList), ncol=length(periods)*2))
  precipColNames=paste('nightlyPrecip',1:length(periods), sep='')
  tempColNames=paste('nightlyTemp',1:length(periods), sep='')
  colnames(weatherDF)=c(precipColNames, tempColNames)

  for(thisTagIndex in 1:length(tagList)){
    thisTag=tagList[thisTagIndex]
    #Because some mice roam around the plots, there is an issue with inputting weather info where you can't know which plot
    #a mice *wasn't* caught in. It's definitly not the norm though, so just pick the plot it's in the most to model weather.
    thisTagPlot=tagInfo %>% filter(tag==thisTag)
    thisTagPlot=mode(thisTagPlot$plot)
    thisTagPeriodInfo=expand.grid(periods, thisTagPlot)
    colnames(thisTagPeriodInfo)=c('period','plot')
    
    thisTagPeriodInfo=merge(thisTagPeriodInfo, nightlyLookupTable, by=c('period','plot'), all.x=TRUE, all.y=FALSE)
    thisTagPeriodInfo= thisTagPeriodInfo %>% arrange(period)
  
    weatherDF[thisTagIndex,precipColNames]=thisTagPeriodInfo$precip
    weatherDF[thisTagIndex,tempColNames]=thisTagPeriodInfo$lowTemp
  
}

  #Merge capture history and nightly weather data
  ch=cbind(ch, weatherDF)
  return(ch)
}

##################################################################################
#The mark/recapture model!
runModel=function(df){
  #Don't understand the need for all this, I just copied it from the marked helpfile.
  x.proc=process.data(df, accumulate=FALSE)
  
  design.p=list(time.varying=c('nightlyPrecip','nightlyTemp'))
  design.parameters=list(p=design.p)
  ddl=make.design.data(x.proc, parameters=design.parameters)
  
  p.formula=list(formula=~nightlyPrecip+nightlyTemp+Time)
  model=crm(x.proc, ddl, hessian=FALSE, model.parameters = list(p=p.formula), accumulate = FALSE, model='cjs')
  return(model$results$reals$p)
}

#########################################################################################################
#Setup the data
###########################################################################################################


#only need control and k-rat exclosure plots
controlPlots=c(2,4,8,11,12,14,17,22) #controls
kratPlots=c(3,6,13,18,19,20) #krat exclosure

#Only model particular spp
speciesToUse=c('PP','DM')

finalDF=data.frame(species=character(), plot=integer(), plotType=character(), nightlyTemp=integer(), p=integer())

for(thisSpp in speciesToUse){
  for(plotType in c('control','exclosure')){
    #Get growth rates for this plotType/spp combo
    if(plotType=='control'){ 
      for(thisPlot in controlPlots){
        x=runModel(createMarkDF(filter(rodents, species==thisSpp, plot ==thisPlot)))
        x$species=thisSpp
        x$plot=thisPlot
        x$plotType=plotType
        x=x %>% select(estimate, nightlyTemp, species, plot, plotType)
        finalDF=rbind(finalDF, x)
        }
      }
    if(plotType=='exclosure'){ 
      
      }
    
    
  }
}

with(finalDF[finalDF$species=='PP',], plot(estimate~nightlyTemp, main='Pocket Mouse (PP)',xlab='Nightly Low Temp', ylab='Trapping Probability'))

with(finalDF[finalDF$species=='DM',], plot(estimate~nightlyTemp, main='Kangaroo Rat (DM)', xlab='Nightly Low Temp', ylab='Trapping Probability'))

