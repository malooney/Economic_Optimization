%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function determines the portfolio variance in the example of 
% portfolio selection. For each dollar invested in a stock or pair of
% stocks, the variance and covariance values are given below:
% Variance of stock 1 = 0.2
% Variance of stock 2 = 0.08
% Variance of stock 3 = 0.18
% Covariance of stocks 1&2 = 0.10
% Covariance of stocks 1&3 = 0.04
% Covariance of stocks 2&3 = 0.06

function [v] = pvar(x)
v = 0.2* x(1)^2+ 0.08* x(2)^2+ 0.18*x(3)^2+ 0.1* x(1)* x(2)+ 0.04* x(1)* x(3) + 0.06* x(2)* x(3);
end