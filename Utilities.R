# TODO: Contains code to cleanup data
# 
# Author: Andrew
###############################################################################

#Annualized geometric mean
mymean=function(x,na.rm=TRUE){
	return(exp(splits*mean(log(1+x),na.rm=na.rm))-1)
}

#remove stocks that didn't exist at start period/end period
#Also sets up mapping variable (Done here so I wouldn't have to iterate twice)
removeBadStocksAndMap=function(){
	minDate=min(data$DATE)
	maxDate=max(data$DATE)
	for (k in 1:numStocks)
	{
		curr=cusip[k]
		tmp=data[curr==data['CUSIP'],]
		mapping[k,1]=curr
		mapping[k,2]=tmp$TICKER[nrow(tmp)]
		if(min(tmp$DATE)!=minDate|max(tmp$DATE)!=maxDate)
			data=data[which(curr!=data['CUSIP']),]
	}
	data<<-data
	cusip<<-unique(data$CUSIP)
	numStocks<<-length(cusip)
}

#Sets up map between cusip and ticker
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

#Optimal solution read from Matlab should be a matrix consisting of asset i's percentage contribution to portfolio
#Element i,j is percent to hold of asset i in rebalance period j.
#Number of rows should equal num stocks
#Number of columns should be equal to number of returns / rTimeInHoldout
optimalPortfolio=function(){
#	Currently rTimeInHoldout is the whole holdout period -> n = 1
	rTimeInHoldout=nrow(holdoutReturnsMatrix)
	
#	What to purchase should be a matrix consisting of asset i's percentage in portfolio with each column being a different rebalanced portfolio
	whatToPurchase=readOptimalFromMatlab()
	
#	combPortfolio is a mxn matrix with m daily returns and n rebalances
	combPortfolio=holdoutReturnsMatrix%*%whatToPurchase
	
#	compresses combPortfolio into the portfolio based on the rebalance frequency. 
	numReturns=nrow(combPortfolio)
	index=1:numReturns
	rebalance=floor((index-1)/rTimeInHoldout)
	portfolio=combPortfolio[rebalance*numReturns+index]
	
#	actualReturn is the realized return, actualStd is the realized risk
	actualReturn=mymean(portfolio)
	actualStd=sqrt(splits)*sd(portfolio)
	
	return(c(actualStd,actualReturn))
}