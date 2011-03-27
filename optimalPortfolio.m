function [solution,value]=optimalPortfolio(returnLevel)

global Cov A Aeq B Beq start


A=dlmread('A.csv');
Aeq=dlmread('Aeq.csv');
B=dlmread('B.csv');
Beq=dlmread('Beq.csv');
Cov=dlmread('Cov.csv');
start=dlmread('Start.csv');

B(1)=-returnLevel;
x=findMin;
std=sqrt(risk(x));
solution=x;
value=std;

dlmwrite('optimalSolution.csv',solution,',')
end

function [ output ] = risk( x )
global Cov
output= x'*Cov*x;
end


function bestSol=findMin
global A Aeq B Beq start
bestVal=Inf;
bestSol=[];

for(i=1:10)
    x=(fmincon(@risk,start(randperm(length(start))),A,B,Aeq,Beq));
    x=(fmincon(@risk,x,A,B,Aeq,Beq));
    if(bestVal>risk(x))
        bestVal=risk(x);
        bestSol=x;
    end
end

end