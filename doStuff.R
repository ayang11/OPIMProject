# TODO: Add comment
# 
# Author: Andrew
###############################################################################

clearWorkspace()
setwd('C:/Users/Andrew/Desktop/Workspace/OPIMProject')
source('FileUtilities.R')
source('PlotUtilities.R')
source('Utilities.R')
source('RunMatlab.R')

#Parameters
startTime=as.Date("1980/01/01")
holdoutStart=as.Date('2005/01/01')
endTime=as.Date("2010/01/01")
splits=252
currDocument<<-'SP100'
#Setting rTimeInHoldout does nothing at the moment
rTimeInHoldout=252

#Read raw data from file
data=read.csv(r(csv(currDocument)),header=TRUE,na.strings=c('B','C'),stringsAsFactors=FALSE)
data$DATE=as.Date(as.character(data$DATE),'%Y%m%d')

#Prunes data by screening for dates before startTime and after endTime
data=data[startTime<=data$DATE&data$DATE<endTime,]

#Stores cusip and numStocks. Instantiates mapping
cusip=unique(data$CUSIP)
numStocks=length(cusip)
mapping=matrix(0,nrow=numStocks,ncol=2)

#Remove stocks that weren't around at start date or end date and set up mapping
#Updates data, cusip, and numstocks
removeBadStocksAndMap()

#Set up the holdout and training period
holdout=data[data$DATE>=holdoutStart,]
data=data[data$DATE<holdoutStart,]

#Construct training and holdout matrix and then name them.
trainReturnsMatrix=matrix(data$RET,ncol=numStocks)
trainReturnsMatrix[is.na(trainReturnsMatrix)]=0
stockReturns=apply(trainReturnsMatrix,2,mymean)
holdoutReturnsMatrix=matrix(holdout$RET,ncol=numStocks)
holdoutReturnsMatrix[is.na(holdoutReturnsMatrix)]=0
covar=splits*cov(trainReturnsMatrix)
rownames(covar)=cusip
colnames(covar)=cusip

saveWorkspace()
loadWorkspace()

#Call this before running efficientFrontier.m or optimalPortfolio.m
writeForMatlab()

#Call Matlab
runEfficientFrontier()
runOptimalPortfolio(0.15)

#Plot the efficient Frontier given that you have run efficientFrontier.m
plotIndividualReturns()
plotEfficientCurve()

#Plot the optimal portfolio's performance over holdout
riskreward=optimalPortfolio()
plotRealizedPoint(riskreward[1],riskreward[2])