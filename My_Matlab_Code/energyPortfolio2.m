%test portfolio selection
clear;
clc;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This program solves the portfolio selection problem. In order to evaluate
% the program, the m-function file 'pvar' must be saved in the same folder
% in which this present script is saved.

% The endogenous variables are the dollars invested in the three stocks, 
% denoted by vector, x.

A= [100 90 130 108 111 90 144 87];
b= 100;
Aeq=[-1 -1 -1 -1 -1 -1 -1 -1];
beq= -225;
lb = zeros(8,1);

x0 = [0.1; 0.1; 0.1; 0.1; 0.1; 0.1; 0.1; 0.1];
options=optimset('Algorithm','interior-point');

%returns = 100:130;
%sd = [];
%stocks = [];
%for i = 1:numel(returns);
    %b = -returns(i);
v = @(x)484* x(1)^2+ 900* x(2)^2+ 225*x(3)^2+ 400*x(4)^2+ 900*x(5)^2+ 1296*x(6)^2+ 1024*x(7)^2+ 1600*x(8)^2;%+ 0.1* x(1)* x(2)+ 0.04* x(1)* x(3) + 0.06* x(2)* x(3);

[x,fval,exitflag,output,lambda,grad,hessian] = fmincon(v, x0, A, b, [], [], lb, [], [], options);
%sd = [sd sqrt(fval)];
%stocks = [stocks x];
%end;

% subplot(2,2,1)
% plot(returns,sd);
% xlabel('Expected Returns');
% ylabel('Standard Deviation');
% 
% subplot(2,2,2);
% plot(returns, stocks(1,:));
% xlabel('Expected Returns');
% ylabel('stock 1 value');
% 
% subplot(2,2,3);
% plot(returns, stocks(2,:));
% xlabel('Expected Returns');
% ylabel('stock 2 value');
% 
% subplot(2,2,4);
% plot(returns, stocks(3,:));
% xlabel('Expected Returns');
% ylabel('stock 3 value');
% 
% 
