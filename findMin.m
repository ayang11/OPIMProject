function bestSol=findMin
global A Aeq B Beq start
bestVal=Inf;
bestSol=[];

for(i=1:5)
    x=(fmincon(@risk,start(randperm(length(start))),A,B,Aeq,Beq));
    x=(fmincon(@risk,x,A,B,Aeq,Beq));
    if(bestVal>risk(x))
        bestVal=risk(x);
        bestSol=x;
    end
end

end