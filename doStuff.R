# TODO: Add comment
# 
# Author: Andrew
###############################################################################

setwd('C:/Users/Andrew/Desktop/Workspace/OPIMProject')

#Parameters
startTime=as.Date("1980/01/01")
holdoutStart=as.Date('2005/01/01')
endTime=as.Date("2010/01/01")
splits=252
rTimeInHoldout=252

#Utilities
m=function(filename)
	return(paste(sep='/','ForMatlab',filename))
i=function(filename)
	return(paste(sep='/','Intermediate',filename))
r=function(filename)
	return(paste(sep='/','RawData',filename))

#Read Stuff
data=read.csv(r('SP500.csv'),header=TRUE,na.strings=c('B','C'),stringsAsFactors=FALSE)
data$DATE=as.Date(as.character(data$DATE),'%Y%m%d')

#Screen Dates
data=data[startTime<=data$DATE&data$DATE<endTime,]

#Find how many days were between start and end dates
#Also sets up mapping between ticker and cusip
allDateLength=0
cusip=unique(data$CUSIP)
numStocks=length(cusip)
mapping=matrix(0,nrow=numStocks,ncol=2)
for (k in 1:numStocks)
{
	currData=data[cusip[k]==data['CUSIP'],]
	rows=nrow(currData)
	mapping[k,1]=cusip[k]
	mapping[k,2]=currData$TICKER[rows]
	allDateLength=max(allDateLength,rows)
}
map=function(cusip=NULL,ticker=NULL){
	if(is.null(cusip)){
		if(is.null(ticker)){
			return(NULL)
		}else{
			return(mapping[which(mapping[,2]==ticker),1])
		}
	}else{
		return(mapping[which(mapping[,1]==cusip),2])
	}
}

#Remove stocks that weren't around at start date or end date
for (k in unique(unlist(data['CUSIP'])))
	if(allDateLength!=nrow(data[k==data['CUSIP'],]))
		data=data[which(k!=data['CUSIP']),]

#Create the holdout period
holdout=data[data$DATE>=holdoutStart,]
data=data[data$DATE<holdoutStart,]

#Find correlations/covariances and name the matrix columns
cusip=unique(data$CUSIP)
numStocks=length(cusip)
trainReturnsMatrix=matrix(data$RET,ncol=numStocks)
trainReturnsMatrix[is.na(trainReturnsMatrix)]=0
returns=apply(trainReturnsMatrix,2,mymean)
holdoutReturnsMatrix=matrix(holdout$RET,ncol=numStocks)
holdoutReturnsMatrix[is.na(holdoutReturnsMatrix)]=0
covar=splits*cov(trainReturnsMatrix)
rownames(covar)=cusip
colnames(covar)=cusip

#Save the data
save(file=i('SP500'),list=ls())

#Load the data
load(i('SP500'))

#Set Up Optimization for Matlab
#mymean finds geometric return over a year from daily returns
mymean=function(x,na.rm=TRUE){
	return(exp(splits*mean(log(1+x),na.rm=na.rm))-1)
}
A=rbind(-returns,-diag(numStocks),diag(numStocks))
Aeq=rep(1,numStocks)
B=c(rep(0,numStocks+1),rep(1,numStocks))
Beq=1
Start=rep(0,numStocks)
Start[1]=1
write.table(sep=',',A,m('A.csv'),col.names=FALSE,row.names=FALSE)
write.table(sep=',',B,m('B.csv'),col.names=FALSE,row.names=FALSE)
write.table(sep=',',t(Aeq),m('Aeq.csv'),col.names=FALSE,row.names=FALSE)
write.table(sep=',',Beq,m('Beq.csv'),col.names=FALSE,row.names=FALSE)
write.table(sep=',',covar,m('Cov.csv'),col.names=FALSE,row.names=FALSE)
write.table(sep=',',Start,m('Start.csv'),col.names=FALSE,row.names=FALSE)


#Read the results from matlab and plot the efficient frontier
std=rep(0,nrow(covar))
for(i in 1:nrow(covar))
	std[i]=sqrt(covar[i,i])
plot(std,returns,col='blue',xlim=c(0,1))
riskreward=read.csv(i('results.csv'),header=FALSE)
risk=unlist(riskreward[1])
reward=unlist(riskreward[2])
points(risk,reward,col='red')

#Returns:
#Optimal solution should be a vector consisting of asset i's percentage contribution to portfolio
#Number of rows should equal num stocks
#Number of columns should be equal to number of returns / rTimeInHoldout
rTimeInHoldout=nrow(holdoutReturnsMatrix)
whatToPurchase=data.matrix(read.csv(header=FALSE,i('optimalSolution.csv')))
combPortfolio=holdoutReturnsMatrix%*%whatToPurchase
numReturns=nrow(combPortfolio)
index=1:numReturns
rebalance=floor((index-1)/rTimeInHoldout)
portfolio=combPortfolio[rebalance*numReturns+index]
actualReturn=mymean(portfolio)
actualStd=sqrt(splits)*sd(portfolio)