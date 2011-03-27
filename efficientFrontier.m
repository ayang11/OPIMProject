function [risks,returns]=efficientFrontier
minPoint=0;
maxPoint=-min(A(1,:)');
points=40;


global Cov A Aeq B Beq start

returns=linspace(minPoint,maxPoint,points);
risks=zeros(size(returns));

A=dlmread('A.csv');
Aeq=dlmread('Aeq.csv');
B=dlmread('B.csv');
Beq=dlmread('Beq.csv');
Cov=dlmread('Cov.csv');
start=dlmread('Start.csv');

for i = 1:length(returns)
    B(1)=-returns(i);
    x=findMin;
    risks(i)=sqrt(risk(x));
end
dlmwrite('results.csv',[risks;returns]',',');
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