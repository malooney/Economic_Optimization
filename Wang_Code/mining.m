% DEMDDP01 Mine Management Model
  fprintf('\nDEMDDP01 MINE MANAGEMENT MODEL\n')
  close all
  clear
  clc

% Enter model parameters
  price = 1;                    % price of ore
  sbar  = 60;                   % initial ore stock
  delta = 0.9;                  % discount factor  
  
% Construct state and action spaces
  S = (0:sbar)';                % vector of states
  X = (0:sbar)';                % vector of actions
  n = length(S);                % number of states
  m = length(X);                % number of actions


% Construct reward function (f) and state transition function (g)
% Non-vectorized version
if 1
  f = zeros(n,m);
  for i=1:n
  for k=1:m
    if X(k)<=S(i)
      f(i,k) = price*X(k)-(X(k)^2)./(1+S(i));
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
% Vectorized version
else
  [SS,XX] = gridmake(S,X);
  f = (price-XX./(1+SS)).*XX;
  f(XX>SS) = -inf;
  f = reshape(f,n,m);
  g = getindex(SS-XX,SS);
  g = reshape(g,n,m);
  clear SS XX
end

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
  [spath xpath] = ddpsimul(pstar,sinit,nyrs,x);

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
