function [ output_args ] = readData( input_args )
%READDATA Summary of this function goes here
%   Detailed explanation goes here

global Cov A Aeq B Beq start

A=dlmread('ForMatlab/A.csv');
Aeq=dlmread('ForMatlab/Aeq.csv');
B=dlmread('ForMatlab/B.csv');
Beq=dlmread('ForMatlab/Beq.csv');
Cov=dlmread('ForMatlab/Cov.csv');
start=dlmread('ForMatlab/Start.csv');

end

