% Cake Eating Model
  fprintf('\nCake Eating Model\n')
  close all
  clear
  clc

% Enter model parameters
  sbar  = 1;                   % total cake
  alpha = 0.5;                 % elasticity of utility
  delta = 0.8;                 % discount factor  
  
% Construct state and action spaces
  S = linspace(0,sbar, 100)';                % vector of states
  X = linspace(0,sbar, 100)';                % vector of actions
  n = length(S);                % number of states
  m = length(X);                % number of actions

% Construct reward function (f) and state transition function (g)
% Non-vectorized version
% if 1
    
  f= zeros(n,m);
  
  for i=1:n
  for k=1:m
    if X(k)<=S(i)
      f(i,k) = X(k).^alpha;
    else
      f(i,k) = -inf;
    end
  end
  end 
  
  g = zeros(n,m);
  for i=1:n
  for k=1:m
    snext = S(i)-X(k);
    g(i,k) = getindex(snext,S);
  end
  end
% % Vectorized version
% else
%   [SS,XX] = gridmake(S,X);
%   f = (price-XX./(1+SS)).*XX;
%   f(XX>SS) = -inf;
%   f = reshape(f,n,m);
%   g = getindex(SS-XX,SS);
%   g = reshape(g,n,m);
%   clear SS XX
% end

% Pack model data
  clear model
  model.reward     = f;
  model.transfunc  = g;
  model.discount   = delta;

% Solve model using the function "ddpsolve"
  [v,x,pstar] = ddpsolve(model);
  
% Plot optimal value function
  subplot(2,2,1);
  plot(S,v);
  title('Optimal Value Function');
  xlabel('Stock'); ylabel('Value');

% Plot optimal policy function
  subplot(2,2,2)
  plot(S,X(x)); 
  title('Optimal Extraction Policy');
  xlabel('Stock'); ylabel('Extraction');

% Generate optimal state and action paths
  sinit = getindex(sbar,S); nyrs = 15;
  [spath, xpath] = ddpsimul(pstar,sinit,nyrs,x);

% Plot optimal state path
  subplot(2,2,3);
  plot(0:nyrs,S(spath));
  title('Optimal State Path');
  xlabel('Year'); ylabel('Stock');
  
% Plot optimal action path
  subplot(2,2,4);
  plot(0:nyrs,X(xpath));
  title('Optimal Action Path');
  xlabel('Year'); ylabel('extraction');
