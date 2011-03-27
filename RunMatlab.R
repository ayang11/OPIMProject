# TODO: Add comment
# 
# Author: Andrew
###############################################################################

runEfficientFrontier=function(){
	system('matlab -nosplash -nodesktop -minimize -r efficientFrontier -logfile Logs/matlablogs')
}
runOptimalPortfolio=function(returns){
	system(sprintf('matlab -nosplash -nodesktop -minimize -r optimalPortfolio(%f) -logfile Logs/matlablogs',returns))
}
