library(plyr)
library(mistnet)

dataFolder='~/data/portal/'
rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))

##########################################
#Clean up data a bit

#Negative period numbers are bad
rodents=rodents[rodents$period>0,]
#working with species here, so get rid of any entries with NA for species
rodents=rodents[!is.na(rodents$species),]
#rodents=rodents[rodents$species!='NAO',]

#Only work with long term control plots
rodents=rodents[rodents$plot==2 | rodents$plot==4 | rodents$plot==8 | rodents$plot==11 |
                rodents$plot==12 | rodents$plot==14 | rodents$plot==17 | rodents$plot==22,]

#Get ride of non-rodents
rodents=merge(rodents, sppCodes[,c('new_code','rodent')], by.x='species', by.y='new_code',all.x=TRUE, all.y=FALSE)
rodents=rodents[rodents$rodent==1,]
rm(sppCodes)
#Represent time as months since the 1st trapping (Jan, 1978)
#It's much easier to represent the time series as one continuous number, instead of months and years.
#The period value does not equal this exactly since some months are skipped and period numbers are not
rodents$projectMonth=with(rodents, (yr-1977)*12 + mo-1)

##########################################
#Prepare data matrix's
#The response variables will be the current months abundances, predictor variables the prior months abundances.
#Other predictors, like weather and time of year, added in later.

#summarize data 
rodents=ddply(rodents, c('species','yr','mo','projectMonth','plot','period'), summarize, N=length(species))
rodents=rodents[!is.na(rodents$species),]

#Get variables to iterate thru
projectMonths=unique(rodents$projectMonth)
speciesList=unique(rodents$species)
plots=unique(rodents$plot)

#Account for blue moons, where two samples are in 1 month
#If 2 periods share a single projectMonth, set the higher period to projectMonth+0.5
#Can then account for this in setting up the data matrixes
#possibly need to account for 0 rodents being trapped in a plot here
#Also when a single sample spans 2 months (ie. 31st and 1st)
for(thisMonth in projectMonths){
  for(thisPlot in plots){
    if(length(unique(rodents$period[rodents$projectMonth==thisMonth & rodents$plot==thisPlot]))>1){
      monthPeriods=sort(unique(rodents$period[rodents$projectMonth==thisMonth & rodents$plot==thisPlot]), decreasing=TRUE)
      if(length(monthPeriods)>2){
        stop('Greater than 2 periods in 1 month') #Shouldn't happen, but still...
      }
      rodents$projectMonth[rodents$period==monthPeriods[1]]=thisMonth+0.5
    }
  }
}

#redo to get the new 0.5 months
projectMonths=unique(rodents$projectMonth)


#Not all species are caught every month. This data frame will put in zeros
#for species not caught as we move through the data.
blankSpeciesAbundance=data.frame(species=speciesList)

#Initialize matrixes
x=matrix(ncol=length(speciesList))
y=matrix(ncol=length(speciesList))

#For each projectMonth in each plot, add that month to y (response) and the prior month to x (predictor), if that matchup exists.
# ie. if there is no month 142, do not put together months 141 and 143.
for(thisPlot in plots){
  #1st check for a prior sample 0.5 months prior (a blue moon sample, see above)
  #Then check for the normal sample thisMonth-1
  #If those don't exsit then there probably a missed month, so skip this entry.
  for(thisMonth in projectMonths){
    if(nrow(rodents[rodents$projectMonth==thisMonth-0.5 & rodents$plot==thisPlot,])>0){
      priorMonth=thisMonth-0.5
    }else if(nrow(rodents[rodents$projectMonth==thisMonth-1 & rodents$plot==thisPlot,])>0){
      priorMonth=thisMonth-1
    }else {
      priorMonth=FALSE
    }
    #Check that the prior period for this plot exists
    if(priorMonth){
      #Gather this months abundances, fill in zeros for non-present species
      yToAdd=merge(blankSpeciesAbundance, rodents[rodents$projectMonth==thisMonth & rodents$plot==thisPlot,], by='species', all.x=TRUE)
      yToAdd$N[is.na(yToAdd$N)]=0
      yToAdd=yToAdd[order(yToAdd$species),]
      #same thing with the prior months abundances
      xToAdd=merge(blankSpeciesAbundance, rodents[rodents$projectMonth==priorMonth & rodents$plot==thisPlot,], by='species', all.x=TRUE)
      xToAdd$N[is.na(xToAdd$N)]=0
      xToAdd=xToAdd[order(xToAdd$species),]
    
      #Add this month/plot combo to the response matrix y, last month/plot is added to response matrix x
      #This produces a bunch of errors about columns lengths not matching, but they totally do. 
      y=rbind(y, yToAdd$N)
      x=rbind(x, xToAdd$N)
    }
  }
}

#Knock off the 1st row, which is from initialization.
x=x[-1,]
y=y[-1,]

rm(blankSpeciesAbundance, yToAdd, xToAdd, thisMonth, projectMonths, speciesList, plots, thisPlot )
#########################
#Setup a test set for cross validation
testSize=0.3
set.seed(2)
#Randomly pick months throughout the time series
#testSet = sample(nrow(y), nrow(y)*testSize)
#Pick the last testSize % of months from the tail end of the time series
testSet= (nrow(y)-(nrow(y)*testSize)):nrow(y)

# put a vector in [] and it selects those elements, a -vector selects everything but those elemens
xTest=x[testSet,]
xTrain=x[-testSet,]
yTest=y[testSet,]
yTrain=y[-testSet,]

rm(testSize, testSet)
#########################
#Scoring function. Euclidean distance between predicted & actual communities

#Mean euclidiean distance between predicted and actual
#of each row (the community in each plot). 
#Essentially mean squared error with multiple dimensions (species). 
portalScore=function(actual, prediction, returnMean=TRUE){
  if(nrow(prediction)!=nrow(actual) | nrow(prediction) != nrow(actual)){ stop('Predicted and Actual dimensions must match')}
  allScores=c()
  for(thisRow in 1:nrow(actual)){
    allScores=c(allScores, dist(rbind(prediction[thisRow,], actual[thisRow,])))
  }
  #print(allScores)
  if(returnMean){
    return(mean(allScores))
  }else {
    return(allScores)
  }
}

#########################
#The model

# Create the network object
net = mistnet(
  x = xTrain,
  y = yTrain,
  layer.definitions = list(
    defineLayer(
      nonlinearity = leaky.rectify.nonlinearity(),
      size = 50,
      prior = gaussian.prior(mean = 0, sd = 0.1)
    ),
    defineLayer(
      nonlinearity = exp.nonlinearity(),
      size = ncol(y),
      prior = gaussian.prior(mean = 0, sd = 0.1)
    )
  ),
  loss = poissonLoss(),
  updater = adagrad.updater(learning.rate = 0.1),
  sampler = gaussian.sampler(ncol = 0L, sd = 1),
  n.importance.samples = 50,
  n.minibatch = 50,
  training.iterations = 0
)

# Fit the model
net$fit(iterations = 500)

yPred=predict(net, newdata = xTest, n.importance.samples = 1)
#mistnet outputs in a funny way, so I have to go thru and build a matrix from the results manually.
tempMatrix=matrix(data=NA, nrow=nrow(yTest), ncol=ncol(yTest))
colLength=nrow(yTest)
colEnd=nrow(yTest)
colStart=1
for(thisCol in 1:ncol(yTest)){
  tempMatrix[,thisCol]=yPred[colStart:colEnd]
  colStart=colStart+colLength
  colEnd=colEnd+colLength
}
yPred=tempMatrix

#Neural networks have linear equations embedded in them, so negative numbers from extrapolation
#are possible. Set any of these to just 0. 
yPred[yPred<0]=0

rm(tempMatrix, colLength, colEnd, colStart, thisCol)

##############################
#test cross validation

modelScores=portalScore(yTest, yPred, returnMean=FALSE)
print(mean(modelScores))

##############################
#Scoring xTest and yTest is actually just a niave model, since ytest is month X and xTest is month X-1
naiveScores=portalScore(yTest, xTest, returnMean=FALSE)
print(mean(naiveScores))

