%Assuming that the writeForMatlab command has been run, this code will read
%the data from the ForMatlab folder and arrive at efficientFrontier points.
%It will write the result to a file called results.csv
function [risks,returns]=efficientFrontier
readData;
global Cov A Aeq B Beq start

minPoint=0;
maxPoint=-min(A(1,:)');
points=20;

returns=linspace(minPoint,maxPoint,points);
risks=zeros(size(returns));

for i = 1:length(returns)
    B(1)=-returns(i);
    x=findMin;
    risks(i)=sqrt(risk(x));
end
dlmwrite('Intermediate/results.csv',[risks;returns]',',');
exit;
end

