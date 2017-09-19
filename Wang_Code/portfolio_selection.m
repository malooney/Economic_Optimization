%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This program solves the portfolio selection problem. In order to evaluate
% the program, the m-function file 'pvar' must be saved in the same folder
% in which this present script is saved.

% The endogenous variables are the dollars invested in the three stocks, 
% denoted by vector, x.
clear
A = [-0.14 -0.11 -0.1]; % expected returns of three stocks
b = -120;               
Aeq = [1 1 1];
beq = 1000;
lb = zeros(3,1);
x0 = [500; 300; 200];
options=optimset('Algorithm','interior-point');

returns = 100:130;
sd = [];
stocks = [];
v = @(x)0.2* x(1)^2+ 0.08* x(2)^2+ 0.18*x(3)^2+ 0.1* x(1)* x(2)+ 0.04* x(1)* x(3) + 0.06* x(2)* x(3);
%[x, fval] = fmincon(v, x0, A, b, Aeq, beq, lb, [], [], options);

for i = 1:numel(returns)
    b = -returns(i);
[x, fval] = fmincon(v, x0, A, b, Aeq, beq, lb, [], [], options);
sd = [sd sqrt(fval)];
stocks = [stocks x];
end

subplot(2,2,1)
plot(returns,sd);
xlabel('Expected Returns');
ylabel('Standard Deviation');

subplot(2,2,2);
plot(returns, stocks(1,:));
xlabel('Expected Returns');
ylabel('stock 1 value');

subplot(2,2,3);
plot(returns, stocks(2,:));
xlabel('Expected Returns');
ylabel('stock 2 value');

subplot(2,2,4);
plot(returns, stocks(3,:));
xlabel('Expected Returns');
ylabel('stock 3 value');


