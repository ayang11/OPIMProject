%Assuming that the writeForMatlab() command has been run, this code will
%return the optimal portfolio and risk value for a given return level. It
%will write the data to a file called optimalSolution.csv
function [solution,value]=optimalPortfolio(returnLevel)

readData;
global Cov A Aeq B Beq start


B(1)=-returnLevel;
x=findMin;
std=sqrt(risk(x));
solution=x;
value=std;

dlmwrite('Intermediate/optimalSolution.csv',solution,',')
exit;
end