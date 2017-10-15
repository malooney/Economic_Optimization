% DEMDDP01 Mine Management Model
  fprintf('\nDEMDDP01 MINE MANAGEMENT MODEL\n')
  close all
  clear
  clc

% Enter model parameters
  price = linspace(1, 10, 3);   % price of ore
  z= 1;                         % price counter
  sbar  = 60;                   % initial ore stock
  delta = 0.9;                  % discount factor
  theta = 1.0;                  % coefficient of extraction efficiency
  
% Construct state and action spaces
  S= linspace(0, 60, 500)';       % vector of states
  X= linspace(0, 60, 500)';       % vector of actions
  n = length(S);                  % number of states
  m = length(X);                  % number of actions
  
figure('units','normalized','outerposition',[0 0 0.75 1]) % make plot window open to X-fraction of screen
figure(1); hold on % allow plot build-up
tp= 0;

% Construct reward function (f) and state transition function (g)
% Non-vectorized version
for z=1:numel(price)
    
   f= zeros(n,m);
  
  for i=1:n
  for k=1:m
    if X(k)<=S(i)
      f(i,k) = price(z)*X(k)-(X(k)^2)./(1+ theta*S(i));
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
  
% Plot optimal value function
  subplot(2,2,1);
  plot(S,v);
  axis([0 60 0 550]);
  title('Optimal Value Function');
  xlabel('Stock'); 
  ylabel('Value');
  legend({...
    'Price of Ore=$1.00',...
    'Price of Ore=$5.50',...
    'Price of Ore=$10.00'},...
    'FontSize', 8, 'Location', 'Northwest','Box','on');
  hold on;
  
% Plot optimal policy function
  subplot(2,2,2)
  plot(S,X(x)); 
  title('Optimal Extraction Policy');
  xlabel('Stock'); 
  ylabel('Extraction');
   legend({...
    'Price of Ore=$1.00',...
    'Price of Ore=$5.50',...
    'Price of Ore=$10.00'},...
    'FontSize', 8, 'Location', 'Northwest','Box','on');
  hold on;
  
% Generate optimal state and action paths
  sinit = getindex(sbar,S); nyrs = 15;
  [spath, xpath] = ddpsimul(pstar,sinit,nyrs,x);

% Plot optimal state path
  subplot(2,2,3);
  plot(0:nyrs,S(spath));
  title('Optimal State Path');
  xlabel('Year'); 
  ylabel('Stock');
   legend({...
    'Price of Ore=$1.00',...
    'Price of Ore=$5.50',...
    'Price of Ore=$10.00'},...
    'FontSize', 8, 'Location', 'Northeast','Box','on');
  hold on;
  
% Plot optimal action path
  subplot(2,2,4);
  plot(0:nyrs,X(xpath));
  title('Optimal Action Path');
  xlabel('Year'); 
  ylabel('extraction');
  legend({...
    'Price of Ore=$1.00',...
    'Price of Ore=$5.50',...
    'Price of Ore=$10.00'},...
    'FontSize', 8, 'Location', 'Northeast','Box','on');
  hold on;
  
  pause(tp)
  z= z+1;

end
  
print('mideterm_Q2_plot', '-dpdf', '-bestfit') % export plot to file


