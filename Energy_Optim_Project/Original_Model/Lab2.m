%% CE 191 - Systems Analysis
%   Lab 2 : Energy Portfolio Optimization
%   Oski Bear, SID 18681868
%   Prof. Moura

% Lab2.m

%%
clear

%% Problem 1
%%% Input problem parameters

% Expxected cost of resources in 2020 [USD/MWh]
% Source: http://en.wikipedia.org/wiki/Cost_of_electricity_by_source
c = [100; 90; 130; 108; 111; 90; 144; 87];

% Maximum expected cost [USD/MWh]
cmax = 100;

% CA Demand in 2020 [MWh]
d = 225;
%d = 300;

% Standard deviation of resource cost [USD/MWh]
sig = [22; 30; 15; 20; 30; 36; 32; 40];

%%% Create QP matrices
Q = diag(2*sig.^2);
R = zeros(8,1);
A = [-1*ones(1,8);...
     (c - cmax)';...
     -eye(8)];
b = [-d; zeros(9,1)];

%% Problem 2
%%% Solve QP
[x_star,J_star,exitflag,~,lam] = quadprog(Q, R, A, b);

% Output Results
fprintf(1,'Risk or variance : %1.2e USD^2\n', 0.5 * x_star' * Q * x_star);
fprintf(1,'Normalized Risk : %2.2f USD^2\n', 0.5 * x_star' * Q * x_star / sum(x_star));
fprintf(1,'Expected Cost : %1.2e USD\n', c'*x_star);
fprintf(1,'Normalized Expected Cost : %2.2f USD/MWh\n', c'*x_star/sum(x_star));

%% Problem 3
%%% 2012 Energy portfolio applied to 2020 [MWh]
x_prob3 = [7.5; 8.3; 43.4; 9.0; 2.3; 4.4; 0.9; 6.3] / 100 * d;

% Output Results
fprintf(1,'Risk or variance : %1.2e USD^2\n', 0.5 * x_prob3' * Q * x_prob3);
fprintf(1,'Normalized Risk : %2.2f USD^2\n', 0.5 * x_prob3' * Q * x_prob3 / sum(x_prob3));
fprintf(1,'Expected Cost : %1.2e USD\n', c'*x_prob3);
fprintf(1,'Normalized Expected Cost : %2.2f USD/MWh\n', c'*x_prob3/sum(x_prob3));

%% Problem 4
N = 41;
cmax_vec = linspace(88, 250, N);

% Preallocate matrices
risk = zeros(N,1);

for k = 1:N
    
    % Create new A matrix depending on max energy price c_max
    A = [-1*ones(1,8);...
         (c - cmax_vec(k))';...
         -eye(8)];
    
    % Solve QP
    [x_star,J_star,exitflag,~,lam] = quadprog(Q, R, A, b);
    
    % Compute price and standard deviation
    risk(k) = J_star;
    
    % Output to command prompt
    fprintf(1,'Max price per MWh: %3.0f USD/MWh\n',cmax_vec(k));
    fprintf(1,'Risk : %1.2e USD^2 \n',risk(k));
    fprintf(1,'Total price : %3.0f USD\n',c'*x_star);
    
end

% Plot Pareto front
fs = 16;
figure(1); clf;
plot(cmax_vec, risk,'LineWidth',2); hold on;
plot(cmax_vec(1), risk(1),'ro','MarkerSize',16,'MarkerFaceColor','r');
xlabel('Max Expected CA Energy Cost in 2020 [USD/MWh]','FontSize',fs)
ylabel('Risk [USD^2]','FontSize',fs)
set(gca,'FontSize',fs)

%% Problem 5

% Original constraints
A = [-1*ones(1,8);...
     (c - cmax)';...
     -eye(8)];
b = [-d; zeros(9,1)];

% Resource Limits
A_rlim = eye(8);
b_rlim = [40; 50; 150; 35; 10; 15; 200; 50];

% RPS constraints
r = [0 0 0 0 1 1 1 1];
A_RPS = 1/3 - r;
b_RPS = 0;

% Add constraints
A5 = [A; A_rlim; A_RPS];
b5 = [b; b_rlim; b_RPS];

%%% Solve QP
[x_star5,J_star5,exitflag5,~,lam5] = quadprog(Q, R, A5, b5);
x_star5
x_star5/d*100



