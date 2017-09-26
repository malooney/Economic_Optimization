
% cakeEating_staticPlot.m
% need utilityFn.m file, a function to define the utility function

% References:

% Adda, J., & Cooper, R. W. (2003). Dynamic economics : quantitative 
% methods and applications. (R. W. Cooper, Ed.). Cambridge, Mass. : MIT Press.

% Miranda, M. J., & Fackler, P. L. (2002). Applied computational economics 
% and finance. (P. L. Fackler, Ed.). Cambridge, Mass. : MIT Press.

% Scripts adapted from Dr. Chenggang Wang and Ming Kang 

% housekeeping
close all
clear
clc
fprintf('\nCake Eating MODEL\n')

% Enter model parameters
finite = 0;     % finite horizon 0/1
T = 15;         % Time horizon

% A discount factor, beta, which indicates the agents measure of time 
% preference, with a higher beta meaning valuing future more and thus
% being more patient.

% Elasticity of Utility, alpha, a measure of risk preference. A lower
% value of alpha means a greater preference for low risk, or a higher
% preference for more consumption over the long run.

alpha = 0.5;        % elasticity of utility parameter
beta = 0.8;         % discount factor

tp = 0.5;       % time pause between plot iterations

% % Construct state and action spaces
N = 1000;                   % grid size
k = linspace(0,1,N)';       % cake fractions

figure('units','normalized','outerposition',[0 0 0.75 1]) % make plot window open to X-fraction of screen
    
% Construct reward function (f)
f = utilityFn(tril(k*ones(1, N)-ones(N, 1)*k'), alpha); % using function utilityFn.m

% Construct state transition function (g)
g = zeros(N ,N);
for s=1:N
    g(s, :) = min(s, (1:N));
end

% Pack model structure
clear model
model.reward = f;
model.transfunc = g;
model.discount = beta;
if (finite)
    model.horizon = T;
end
    
% Solve model using the function "ddpsolve"
[v, index pstar] = ddpsolve(model);

% Compute optimal cake and consumption path
k1 = k(index);
c = k*ones(1, size(index, 2)) - k1;

c_star = zeros(T, 1);
k_star = zeros(T+1, 1);
k_star(1) = 1;

for t=1:T
    s = getindex(k_star(t), k);
    if (finite)
        k_star(t+1) = k1(s, t);
        c_star(t) = c(s, t);
    else
        k_star(t+1) = k1(s);
        c_star(t) = c(s);
    end
end

%figure(1);
subplot(1, 1, 1);
plot((1:T), c_star);
axis([1 T 0 1]);
title( {...
    '{\bf\fontsize{20} Optimal Consumption Path}';...
    '{\it \Delta\alpha and \Delta\beta to evaluate changes in consumption preferences}'...
    }, 'FontSize', 14, 'FontWeight', 'normal')
xlabel('time', 'FontSize', 18);
ylabel('c^*_t', 'FontSize', 18);
legend({['c^*_t  \alpha =' num2str(alpha), ', \beta =' num2str(beta)]}, 'FontSize', 18, 'Location', 'North', 'Box','off');

