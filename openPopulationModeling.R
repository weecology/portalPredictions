#Using various packages (marked, Rcapture, mra) to do open population mark-recapture models at
#portal. See http://warnercnr.colostate.edu/~gwhite/fw663/lecture8.pdf

dataFolder='~/data/portal/'
rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))

#1st try. only estimate after 1994 when pit tags were in heavy use. Only DM and only in control plots.
rodents=rodents[rodents$yr>=1994,]
rodents=rodents[rodents$species=='DM',]
rodents=rodents[!is.na(rodents$plot),]
#Only work with long term control plots
rodents=rodents[rodents$plot==2 | rodents$plot==4 | rodents$plot==8 | rodents$plot==11 |
                  rodents$plot==12 | rodents$plot==14 | rodents$plot==17 | rodents$plot==22,]

#List of individuals. After 1994 all non-PIT tags look to be used only once, except when it's numbered 0
tagList=sort(unique(rodents$tag))
tagList=tagList[!is.na(tagList)]
tagList=tagList[tagList!='0']
tagList=tagList[tagList!='000000']

#Build data matrix. All packages take data in the same format as RMark.
#ch = capture history, a single column with a long text string of 0's and 1s. 
#1=captured, 0= not captured. Each column in the string is a capture session. one row per individual. 

periods=sort(unique(rodents$period))
periods=periods[periods>0]

tagCHList=c()

for(tag in tagList){
  #Get a list of periods this tag was seen in. 
  thisTagPeriods=rodents$period[rodents$tag==tag]
  thisTagPeriods=thisTagPeriods[!is.na(thisTagPeriods)] #The list is full of NA's! What?! why do I have to do this? screw you R
  #convert list of seen/not seen periods to '00001001000111000.....' etc. and add it to the list of all tags
  tagCHList=c(tagCHList, as.character(paste((thisTagPeriods==periods)*1, collapse='')))
}

tagCHlist=as.character(tagCHList)

modelMatrix=data.frame(ch=tagCHList)
