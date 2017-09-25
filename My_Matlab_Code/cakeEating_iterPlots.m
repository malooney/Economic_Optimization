
% cakeEating_iterPlots.m
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
warning ('off')
optset('ddpsolve', 'prtiters', 0)
fprintf('\nCake Eating MODEL\n')
fprintf('\nddpsolve iteration output supressed, comment out optset to view\n')

% Enter model parameters
finite = 0;     % finite horizon 0/1
T = 10;         % Time horizon

% A discount factor, beta, which indicates the agents measure of time 
% preference, with a higher beta meaning valuing future more and thus
% being more patient.

% Elasticity of Utility, alpha, a measure of risk preference. A lower
% value of alpha means a greater preference for low risk, or a higher
% preference for more consumption over the long run.

alpha = linspace(0.1, 0.9, 9);      % elasticity of utility parameter
beta = linspace(0.1, 0.9, 9);       % discount factor
tp = 0.5;     % time pause between plot iterations

% Construct state and action spaces
N = 1000;        % grid size
k = linspace(0, 1 ,N)';       % cake fractions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% changing beta, holding alpha constant

figure('units','normalized','outerposition',[0 0 0.75 1]) % make plot window open to X-fraction of screen
figure(1); hold on % allow plot build-up

for j = 1:numel(beta)
    
% Construct reward function (f)
f = utilityFn(tril(k*ones(1, N)-ones(N, 1)*k'), alpha(5)); % using function utilityFn.m

% Construct state transition function (g)
g = zeros(N, N);
for s=1:N
    g(s, :) = min(s, (1:N));
end

% Pack model structure
clear model
model.reward = f;
model.transfunc = g;
model.discount = beta(j);
if (finite)
    model.horizon = T;
end
    
% Solve model using the function "ddpsolve"
[v, index] = ddpsolve(model);

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

%Plot of consumption path vs. change in discount rate

%figure(1);
subplot(2, 1, 1);
plot((1:T), c_star);
axis([1 T 0 1]);
title( {...
    '{\bf\fontsize{20} Optimal Consumption}';...
    '{\it Evaluate changes in consumption preference as discount factors change}'...
    }, 'FontSize', 12, 'FontWeight', 'normal')
xlabel('time', 'FontSize', 18);
ylabel('c^*_t', 'FontSize', 18);
legend({...
    'c^*_t \alpha = 0.5, \beta = 0.1',...
    'c^*_t \alpha = 0.5, \beta = 0.2',...
    'c^*_t \alpha = 0.5, \beta = 0.3',...
    'c^*_t \alpha = 0.5, \beta = 0.4',...
    'c^*_t \alpha = 0.5, \beta = 0.5',...
    'c^*_t \alpha = 0.5, \beta = 0.6',...
    'c^*_t \alpha = 0.5, \beta = 0.7',...
    'c^*_t \alpha = 0.5, \beta = 0.8',...
    'c^*_t \alpha = 0.5, \beta = 0.9'},...
    'FontSize', 10,'Location','Northeast','Box','on');
hold on;
pause(tp)

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% changing Alpha, holding beta constant at 0.5

for i = 1:numel(alpha)
    
% Construct reward function
f = utilityFn(tril(k*ones(1, N)-ones(N, 1)*k'), alpha(i)); % using function utilityFn.m

% Construct transition function
g = zeros(N, N);

for s=1:N
    g(s, :) = min(s, (1:N));
end

% Pack model structure
clear model
model.reward = f;
model.transfunc = g;
model.discount = beta(5);
if (finite)
    model.horizon = T;
end
    
% Solve model
[v, index] = ddpsolve(model);

% Compute optimal cake and consumption path
k1 = k(index);
c = k*ones(1, size(index, 2)) - k1;

c_star = zeros(T,1);
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

% Plot of consumption path vs. change in elasticity of utility
%textLeg(i,:) = ['c^*_t \alpha =' num2str(alpha(i)), ', \beta =' num2str(beta(5))];
%textLeg = [{'c^*_t \alpha = 0.1, \beta = 0.5', 'c^*_t \alpha = 0.2, \beta = 0.5'}];

%figure(2);
subplot(2, 1, 2);
plot((1:T), c_star);
axis([1 T 0 1]);
title( {...
    '{\bf\fontsize{20} Optimal Consumption}';...
    '{\it Evaluate changes in consumption preference as the elasticity of utility changes}'...
    }, 'FontSize', 12, 'FontWeight', 'normal')
xlabel('time', 'FontSize', 18);
ylabel('c^*_t', 'FontSize', 18);
%legend({textLeg(1,1), textLeg(1,2)}, 'FontSize', 10,'Location','North','Box','off');
legend({...
    'c^*_t \alpha = 0.1, \beta = 0.5',...
    'c^*_t \alpha = 0.2, \beta = 0.5'...
    'c^*_t \alpha = 0.3, \beta = 0.5'...
    'c^*_t \alpha = 0.4, \beta = 0.5'...
    'c^*_t \alpha = 0.5, \beta = 0.5'...
    'c^*_t \alpha = 0.6, \beta = 0.5'...
    'c^*_t \alpha = 0.7, \beta = 0.5'...
    'c^*_t \alpha = 0.8, \beta = 0.5'...
    'c^*_t \alpha = 0.9, \beta = 0.5'},...
    'FontSize', 10,'Location','Northeast','Box','on');
%legend({['c^*_t \alpha =' num2str(alpha), ', \beta =' num2str(beta) '\newline']}, 'FontSize', 10, 'Location', 'North', 'Box','off');
hold on;
pause(tp)

end

%print('Looney_cake_plot', '-dpdf', '-bestfit') % export plot to file

