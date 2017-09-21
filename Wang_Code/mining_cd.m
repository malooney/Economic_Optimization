% DEMDDP01 Mine Management Model
  fprintf('\nDEMDDP01 MINE MANAGEMENT MODEL\n')
  close all

% Enter model parameters
  price = 1;                    % price of ore
  sbar  = 60;                   % initial ore stock
  delta = 0.9;                  % discount factor 
  theta = 1:5;                  % coefficient of extraction efficiency
  
% Construct state and action spaces
  S = (0:sbar)';                % vector of states
  X = (0:sbar)';                % vector of actions
  n = length(S);                % number of states
  m = length(X);                % number of actions

  action = [];
  value = [];
  XPATH = [];
  SPATH = [];
  
for z = 1:numel(theta)
% Construct reward function (f) and state transition function (g)

  f = zeros(n,m);
  for i=1:n
  for k=1:m
    if X(k)<=S(i)
      f(i,k) = price*X(k)-(X(k)^2)./(1+theta(z)*S(i));
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

% Pack model data
  clear model
  model.reward     = f;
  model.transfunc  = g;
  model.discount   = delta;

% Solve model using the function "ddpsolve"
  [v,x,pstar] = ddpsolve(model);
  action = [action x];
  value = [value v];
  
  % Generate optimal state and action paths
  sinit = getindex(sbar,S); nyrs = 15;
  [spath xpath] = ddpsimul(pstar,sinit,nyrs,x);
  XPATH = [XPATH; xpath];
  SPATH = [SPATH; spath];
end

% Plot optimal value function
  subplot(2,2,1);
  plot(S,value);
  title('Optimal Value Function');
  xlabel('Stock'); ylabel('Value');
  legend('theta=1','theta=2','theta=3','theta=4','theta=5') 

% Plot optimal policy function
  subplot(2,2,2);
  plot(S,X(action)); 
  title('Optimal Extraction Policy');
  xlabel('Stock'); ylabel('Extraction');
  legend('theta=1','theta=2','theta=3','theta=4','theta=5')
  
% Plot optimal state path
  subplot(2,2,3);
  plot(0:nyrs,S(SPATH));
  title('Optimal State Path');
  xlabel('Year'); ylabel('Stock');
  legend('theta=1','theta=2','theta=3','theta=4','theta=5')

% Plot optimal action path
  subplot(2,2,4);
  plot(0:nyrs,X(XPATH));
  title('Optimal Action Path');
  xlabel('Year'); ylabel('extraction');
  legend('theta=1','theta=2','theta=3','theta=4','theta=5')
