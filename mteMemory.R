#Model total portal MTE using precip and ecological memory from Ogle et al. 2015

library(dplyr)
library(magrittr)
library(tidyr)
library(rstan)

dataFolder='~/data/portal/'
rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))

##########################################
#Clean up data a bit

#Negative period numbers are bad
rodents=rodents %>%
  filter(period>0)

#Only work with long term control plots
rodents=rodents %>%
  filter(plot %in% c(2,4,8,11,12,14,17,22))

#Get ride of non-rodents
rodents=rodents %>%
  left_join(select(sppCodes, new_code, rodent), by=c('species'='new_code')) %>%
  filter(rodent==1)

rodents=rodents %>%
  filter(!is.na(wgt))

#Represent time as months since the 1st trapping (Jan, 1978)
#It's much easier to represent the time series as one continuous number, instead of months and years.
#The period value does not equal this exactly since some months are skipped and period numbers are not
#rodents$projectMonth=with(rodents, (yr-1977)*12 + mo-1)

totalMTE=rodents %>%
  mutate(mte=wgt*0.75) %>%
  mutate(season=ifelse(mo<=6, 'spring','fall')) %>%
  group_by(period, yr, season) %>%
  summarize(mte=sum(mte)) %>%
  ungroup() %>%
  group_by(yr, season) %>%
  summarize(mte=mean(mte))

#interpret time periods as "seasons since spring 1977"
totalMTE$projectSeason=with(totalMTE, (yr-1977)*2 + ifelse(season=='spring',1,2))

totalMTE= totalMTE %>% arrange(projectSeason)


############################################################################
#Weather. Get total precip by season.

weatherRaw = read.csv('data/Hourly_PPT_mm_1989_present_fixed.csv') %>%
  group_by(Year, Month) %>%
  summarize(precip=sum(Precipitation)) %>%
  ungroup() %>%
  mutate(season=ifelse(Month<=6,'spring','fall'), projectSeason = (Year-1977)*2 + ifelse(season=='spring',1,2)) %>%
  group_by(Year, projectSeason) %>%
  summarize(precip=sum(precip)) %>%
  ungroup() %>%
  rename(yr=Year)

###########################################################################
#Build rodent data with precipitation time lags

#projectSeason for beginning and end to analys. 31 is the beginning of the good precip data + 5. 77 is spring 2015
timeSeriesStart=32
timeSeriesStop=77  

#Up to 6 seasons of lag (including current season)
timeLag=5

#Initialize precip variables with lags
precipLagColNames=paste('precipLag_T-',0:timeLag,sep='')
totalMTE[,precipLagColNames]=0

for(thisSeason in timeSeriesStart:timeSeriesStop){
  precip=weatherRaw %>% 
    filter(projectSeason %in% thisSeason:(thisSeason-timeLag)) %>%
    arrange(-projectSeason) %>%
    extract2('precip')
  totalMTE[totalMTE$projectSeason==thisSeason, precipLagColNames]=precip
}


totalMTE=totalMTE %>%
  filter(projectSeason >=timeSeriesStart, projectSeason<=timeSeriesStop)

#########################################################################


N=nrow(totalMTE)
numLags=timeLag+1
mte=extract2(totalMTE, 'mte')
precip=as.matrix(totalMTE[,precipLagColNames])
precipWeightPrior=rep(1,numLags)

stanData=list(N=N, numLags=numLags, mte=mte,precip=precip, wPPTprior=precipWeightPrior)

stanCode= "
data {
  int N;   //number of time points
  int numLags;  //length of lag 
  real<lower=0> mte[N]; //mte data
  real<lower=0> precip[N,numLags]; // precip data, with lag already organized in R
  vector<lower=0>[numLags] wPPTprior;
}

parameters{
  //real mu[N]; //process model mean MTE
  real a[4]; //for the process model 
  real obsSD; //observation model sd

  simplex[numLags] wPPT; //weights for the time lags. this is the memory. needs to be simplex for dirichlet dist.

  real<lower=0> latentStart; //a starting point for the time series latent process. there is probably a better way to do this
}

model {
  real weightedPrecip;
  real weightedPrecipVector[numLags];
  real mu[N];
  //priors
  a ~ normal(0, 10);
  obsSD ~ gamma(1,1);

  wPPT ~ dirichlet(wPPTprior);

  latentStart ~ normal(1200, 500); //large prior around the observed mte

  mu[1] <- latentStart;

  for(i in 2:N){
    //Get weighted PPT with mmemory  
    for(thisLag in 1:numLags){
      weightedPrecipVector[thisLag] <- precip[i,thisLag] * wPPT[thisLag];
    }
    weightedPrecip <- sum(weightedPrecipVector);

    mu[i] <- a[1] + a[2]*mu[i-1] + a[3]*weightedPrecip + a[4]*weightedPrecip*mu[i-1]; //ps. need error term
    //mu[i] <- a[1] + a[2]*weightedPrecip;    

    mte[i] ~ normal(mu[i], obsSD);
  }

}
"

initials=list(list(a=c(0.1,0.1,0.1,0.1),
              obsSD=0.001,
              wPPT=rep(1/numLags, numLags),
              latentStart=1100))

fit = stan(model_code=stanCode, data=stanData, iter=200, warmup=20, thin=1, chains=1, init=initials)
#fit = stan(model_code=stanCode, data=stanData, iter=20000, warmup=500, thin=2, chains=1, init=initials)

