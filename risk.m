function [ output ] = risk( x )
global Cov
output= x'*Cov*x;
end
