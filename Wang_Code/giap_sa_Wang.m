%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This program determines Giapetto's production choices and weekly profit
% when the price of wooden soldiers varies in the rage of $20 - 40$.

% To run the following script, you need to save the m-file 'giap' in the
% same folder in which the present script is saved.

P = 20:40;              % Define the range of the price of wooden soldiers.
z = zeros (6,numel(P)); % Create a zero matrix for storing results.

for k = 1:numel(P)
P1 = P(k);   % price of wooden soldiers
Cm1 = 10;    % Material costs per wooden soldier
Cvf1 = 14;   % Variable costs and overhead per wooden soldier
Lf1 = 2;     % Finishing labor requirements per wooden soldier
Lc1 = 1;     % Carpentry labor requirements per wooden soldier
P2 = 21;     % Price of wooden trains
Cm2 = 9;     % Material cost per wooden train
Cvf2 = 10;   % Variable costs and overhead per wooden train
Lf2 = 1;     % Finishing labor requirements per wooden train
Lc2 = 1;     % Carpentry labor requirements per wooden train
Lfmax = 100; % Maximium finishing labor
Lcmax = 80;  % Maximum carpentry labor
D1max = 40;  % Maximum weekly demand for wooden soldiers

% Call the function, giap, to solve for the optimal outputs of woodern 
% soldiers and trains.  Save optimal outputs, weekly profit, and Lagrangian
% multiplies in output matrix z.

[x fval lambda] = giap(P1, Cm1, Cvf1, Lf1, Lc1, P2, Cm2, Cvf2, Lf2, Lc2,...
    Lfmax, Lcmax, D1max);

z ([1 2], k) = x;
z (3, k) = -fval;
z (4, k) = lambda.ineqlin(1);
z (5, k) = lambda.ineqlin(2);
z (6, k) = lambda.upper(1);
end

% Plotting the solution functions and value function with respect to price
% of wooden soldiers.

subplot(2,3,1)

plot(P, z(1,:));
ylabel('Weekly Output of Wooden Soldiers')
xlabel('Price of Woodern Soldiers')

subplot(2,3,2)
plot(P, z(2,:));
ylabel('Weekly Output of Wooden Trains')
xlabel('Price of Woodern Soldiers')

subplot(2,3,3)
plot(P, z(3,:));
ylabel('Weekly Profit')
xlabel('Price of Woodern Soldiers')

subplot(2,3,4)
plot(P, z(4,:));
ylabel('Shadow Price -- Finishing Labor')
xlabel('Price of Woodern Soldiers')

subplot(2,3,5)
plot(P, z(5,:));
ylabel('Shadow Price -- Capertry Labor')
xlabel('Price of Woodern Soldiers')

subplot(2,3,6)
plot(P, z(6,:));
ylabel('Shadow Price -- Demand Constraint - Woodern Soldiers')
xlabel('Price of Woodern Soldiers')
