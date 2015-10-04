dataFolder='~/data/portal/'
rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))

rodents=rodents[rodents$yr>=1994,]
rodents=rodents[rodents$species=='DM',]
#Only work with long term control plots
rodents=rodents[rodents$plot==2 | rodents$plot==4 | rodents$plot==8 | rodents$plot==11 |
                  rodents$plot==12 | rodents$plot==14 | rodents$plot==17 | rodents$plot==22,]

tagList=sort(unique(rodents$tag))
tagList=tagList[!is.na(tagList)]
tagList=tagList[tagList!='0']
tagList=tagList[tagList!='000000']

periods=sort(unique(rodents$period))

modelMatrix=data.frame(ch=NULL)



for(tag in tagList){
  
}