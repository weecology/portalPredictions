setwd('~/projects/portalStuff')
library(plyr)
library(mistnet)
library(reshape2)
#library(caret)
#library(stringr)
#library(ggplot2)
#library(RColorBrewer)

#rodents=read.csv('~/data/PortalMammals_main.csv', na.strings=c("","NA"))
rodents=read.csv('~/data/allrodents_1978-2012.csv', na.strings=c("","NA"))

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

#Represent time as months since the 1st trapping (Jan, 1978)
#It's much easier to represent the time series as one continuous number, instead of months and years.
#The period value does not equal this exactly since some months are skipped and period numbers are not
rodents$projectMonth=with(rodents, (yr-1978)*12 + mo-1)

##########################################
#Prepare data matrix's
#The response variables will be the current months abundances, predictor variables the prior months abundances.
#Other predictors, like weather and time of year, added in later.

#summarize data 
rodents=ddply(rodents, c('species','yr','mo','projectMonth','plot'), summarize, N=length(species))

#Get variables to iterate thru
projectMonths=unique(rodents$projectMonth)
speciesList=unique(rodents$species)
plots=unique(rodents$plot)

#Not all species are caught every month. This data frame will put in zeros
#for species not caught as we move through the data.
blankSpeciesAbundance=data.frame(species=speciesList)

#Initialize matrixes
x=matrix(ncol=length(speciesList))
y=matrix(ncol=length(speciesList))

#For each projectMonth in each plot, add that month to y (response) and the prior month to x (predictor), if that matchup exists.
# ie. if there is no month 142, do not put together months 141 and 143.
for(thisPlot in plots){
  for(thisMonth in projectMonths){
    #Check that the prior period for this plot exists
    if(nrow(rodents[rodents$projectMonth==thisMonth-1 & rodents$plot==thisPlot,])>0){
      #Gather this months abundances, fill in zeros for non-present species
      yToAdd=merge(blankSpeciesAbundance, rodents[rodents$projectMonth==thisMonth & rodents$plot==thisPlot,], by='species', all.x=TRUE)
      yToAdd$N[is.na(yToAdd$N)]=0
      yToAdd=yToAdd[order(yToAdd$species),]
      #same thing with the prior months abundances
      xToAdd=merge(blankSpeciesAbundance, rodents[rodents$projectMonth==thisMonth-1 & rodents$plot==thisPlot,], by='species', all.x=TRUE)
      xToAdd$N[is.na(xToAdd$N)]=0
      xToAdd=xToAdd[order(xToAdd$species),]
    
      #Add this month/plot combo to the response matrix y, last month/plot is added to response matrix x
      y=rbind(y, yToAdd$N)
      x=rbind(x, xToAdd$N)
    }
  }
}

#Knock off the 1st row, which is from initialization.
x=x[-1,]
y=y[-1,]

rm(blankSpeciesAbundance, yToAdd, xToAdd, thisPlot, thisMonth, projectMonths, speciesList, plots, thisPlot, )
#########################
#Setup a test set for cross validation
testSize=0.2
set.seed(2)
testSet = sample(nrow(y), nrow(y)*testSize)

xTrain=x[-testSet,]
xTest=x[testSet,]
yTrain=y[-testSet,]
yTest=y[testSet,]

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
      nonlinearity = rectify.nonlinearity(),
      size = 30,
      prior = gaussian.prior(mean = 0, sd = 0.1)
    ),
    defineLayer(
      nonlinearity = rectify.nonlinearity(),
      size = 30,
      prior = gaussian.prior(mean = 0, sd = 0.1)
    ),
    defineLayer(
      nonlinearity = linear.nonlinearity(),
      size = ncol(y),
      prior = gaussian.prior(mean = 0, sd = 0.1)
    )
  ),
  loss = squaredLoss(),
  updater = adagrad.updater(learning.rate = .01),
  sampler = gaussian.sampler(ncol = 10L, sd = 1),
  n.importance.samples = 30,
  n.minibatch = 10,
  training.iterations = 0
)

# Fit the model
net$fit(iterations = 500)

yPred=predict(net, newdata = xTest, n.importance.samples = 1)
#mistnet outputs in a funny way, so I have to go thru and build a matrix from it manually.
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
rm(tempMatrix, colLength, colEnd, colStart, thisCol)

##############################
#test cross validation
yPred[yPred<0]=0
modelScores=portalScore(yTest, yPred, returnMean=FALSE)

##############################
#Scoring xTest and yTest is actually just a niave model, since ytest is month X and xTest is month X-1
naiveScores=portalScore(yTest, xTest, returnMean=FALSE)

