# TODO: Contains code to plot the efficient frontier and individual points
# 
# Author: Andrew
###############################################################################

#Plot return to risk for individual securities
plotIndividualReturns=function()
{
	std=rep(0,nrow(covar))
	for(i in 1:nrow(covar))
		std[i]=sqrt(covar[i,i])
	plot(std,stockReturns,col='blue',xlim=c(0,1))
}

#reads the efficient curve from matlab and plots the efficient curve. 
plotEfficientCurve=function(){
	riskreward=readEfficientFromMatlab()
	risk=unlist(riskreward[1])
	reward=unlist(riskreward[2])
	points(risk,reward,col='red')
}

#adds a single risk/return point to the plot
plotRealizedPoint=function(risk,reward){
	points(risk,reward,col='green')
}