% DEMDDP02 Asset Replacement Model
% This program conducts a senstivity analysis of the assest replacmenet
% decision, examining how the replacement cost affects the policy function. 
  fprintf('\nDEMDDP02 ASSET REPLACEMENT MODEL\n')
  close all
  
% Enter model parameters
  maxage  = 5;                          % maximum machine age
  repcost = 0:25:75;                         % replacement cost  
  delta   = 0.9;                        % discount factor  
 
% Construct state space
  S = (1:maxage)';                      % machine age
  n = length(S);                        % number of states
  
  X = [];
  V = [];
  
  for j = 1:numel(repcost)
      
% Construct reward function (keep=1, replace=2)
  f = [50-2.5*S-2.5*S.^2 (50-repcost(j))*ones(n,1)]; 
  f(end,1) = -inf;
  
% Construct state transition function
  g = zeros(n,2);
  for i=1:n
     g(i,1) = min(i+1,n);                % keep
     g(i,2) = 1;                         % replace
  end
  
% Pack model structute
  clear model
  model.reward     = f;
  model.transfunc  = g;
  model.discount   = delta;

  subplot(2,2,j); 
    
% Solve infinite-horizon model using policy iteration
  [v,x,pstar] = ddpsolve(model);
  % Generate optimal path
  sinit = min(S); nyrs = 12;
  spath = ddpsimul(pstar,sinit,nyrs);
  
% Plot State Path
  plot(1:nyrs+1,S(spath)); 
  title('Optimal State Path');
  xlabel('Year'); ylabel('Age of Machine');
  xlim([1 12])
 
  
  end
  

