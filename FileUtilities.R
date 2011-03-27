# TODO: Contains code to read and write to file and set up matlab relationship
# 
# Author: Andrew
###############################################################################

#File Locations
m=function(filename)
	return(paste(sep='/','ForMatlab',filename))
i=function(filename)
	return(paste(sep='/','Intermediate',filename))
r=function(filename)
	return(paste(sep='/','RawData',filename))
csv=function(filename)
	return(paste(sep='.',filename,'csv'))

#Write covariances and constraints for the matlab file to run. 
writeForMatlab=function(){
	A=rbind(-stockReturns,-diag(numStocks),diag(numStocks))
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
}

#read the efficient frontier from matlab. Written by efficientFrontier.m
readEfficientFromMatlab=function(){
	return(read.csv(i('results.csv'),header=FALSE))
}

#read the optimal portfolio from matlab. Written by optimalPortfolio.m
readOptimalFromMatlab=function(){
	return(data.matrix(read.csv(header=FALSE,i('optimalSolution.csv'))))
}

#save and load workspace
saveWorkspace=function(){
	save(file=i(currDocument),list=ls(globalenv()),envir = globalenv())
}
loadWorkspace=function(){
	load(i(currDocument),envir=globalenv())
}
clearWorkspace=function(){
	rm(list=ls(globalenv()),envir=globalenv())
}