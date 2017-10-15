
% DEMDDP05 Water Management Model
% This program solves the water management model under the high rainfall
% scenario.
  fprintf('\nDEMDDP05 WATER MANAGEMENT MODEL\n')
  warning ('off','all');
  close all
  clear
  clc

% Enter model parameters
  alpha1 = linspace(10, 18, 3);     % producer benefit function parameter
  beta1  = 0.8;                     % producer benefit function parameter
  alpha2 = 10;                      % recreational user benefit function parameter
  beta2  = 0.4;                     % recreational user benefit function parameter  
  maxcap = 30;                      % maximum dam capacity
  r      = [0 1 2 3 4];             % rain levels
  p      = [0.1 0.2 0.4 0.2 0.1];   % rain probabilities
  delta  = 0.9;                     % discount factor
  z= 1;
  avgstock = zeros(1,3);

% Construct state space
  S = (0:maxcap)';              % vector of states
  n = length(S);                % number of states

% Construct action space
  X = (0:maxcap)';              % vector of actions
  m = length(X);                % number of actions
  
figure('units','normalized','outerposition',[0 0 0.75 1]) % make plot window open to X-fraction of screen
figure(1); hold on % allow plot build-up
tp= 1;

% Construct reward function
for z= 1:numel(alpha1)

  f = zeros(n,m);
  for i=1:n
  for k=1:m
    if k>i
      f(i,k) = -inf;
    else
      f(i,k) = alpha1(z)*X(k).^beta1+ alpha2*(S(i)-X(k)).^beta2; 
      
    end
  end
  end
  
% Construct state transition matrix
  P = zeros(m,n,n);  
  for k=1:m
  for i=1:n
  for j=1:length(r)
    snext = min(S(i)-X(k)+r(j),maxcap);
    inext = getindex(snext,S);
    P(k,i,inext) = P(k,i,inext) + p(j);
  end
  end
  end

% Pack model structure
  clear model
  model.reward     = f;
  model.transprob  = P;
  model.discount   = delta;

% Solve infinite-horizon model using policy iteration
  [v,x,pstar] = ddpsolve(model);

% Plot optimal policy
  subplot(2,2,1); 
  plot(S,X(x),'*'); 
  % set(h,'FaceColor',[.75 .75 .75])
  % axis([0 maxcap -inf inf]);
  title('Optimal Irrigation Policy');
  xlabel('Water Level'); ylabel('Irrigation');
  xlim([-1 31]);
  ylim([0 6]);
  legend({...
    '\alphax^{0.8}, \alpha = 10',...
    '\alphax^{0.8}, \alpha = 14',...
    '\alphax^{0.8}, \alpha = 18'},...
    'FontSize', 8,'Location','Northwest','Box','on');
hold on;

% Plot optimal value function
  subplot(2,2,2); 
  plot(S,v); 
  title('Optimal Value Function');
  xlabel('Water Level'); 
  ylabel('Value');
  legend({...
    '\alphax^{0.8}, \alpha = 10',...
    '\alphax^{0.8}, \alpha = 14',...
    '\alphax^{0.8}, \alpha = 18'},...
    'FontSize', 8,'Location','Southeast','Box','on');
hold on;

% Generate random optimal paths, starting from zero water level
  sinit = ones(10000,1); 
  nyrs  = 30;
  spath = ddpsimul(pstar,sinit,nyrs);  
  ylim([200 700])
  
  % Plot expected water level over time, starting from zero water level
  subplot(2,2,3); 
  plot(0:nyrs,mean(S(spath)));
  title('Optimal State Path'); 
  xlabel('Year'); 
  ylabel('Water Level');
  ylim([0 14]);
  legend({...
    '\alphax^{0.8}, \alpha = 10',...
    '\alphax^{0.8}, \alpha = 14',...
    '\alphax^{0.8}, \alpha = 18'},...
    'FontSize', 8,'Location','Southeast','Box','on');
hold on;

% Compute steady state distribution of water level
  subplot(2,2,4); 
  pi = markov(pstar);
  avgstock(1,z) = pi'*S;
  h=bar(pi);
  line([avgstock(1,z) avgstock(1,z)], [0 0.25]);
  %set(h,'FaceColor',[.75 .75 .75])
  title('Steady State Distribution');
  xlabel('Water Level'); ylabel('Probability'); 
  xlim([-1 31])
  ylim([0 0.25])
  legend({...
    '\alphax^{0.8}, \alpha = 10',...
    ['avgstock =' num2str(avgstock(1,1) )],...
    '\alphax^{0.8}, \alpha = 14',...
    ['avgstock =' num2str(avgstock(1,2) )],...
    '\alphax^{0.8}, \alpha = 18',...
    ['avgstock =' num2str(avgstock(1,3) )]},...
    'FontSize', 8,'Location','Northeast','Box','on');
hold on;
  
  z= z+1;
  pause(tp)
  
  % Compute steady-state water level
  % avgstock = zeros(1,3);
  % fprintf('\nSteady-state Stock        %8.2f\n',avgstock)
  
end
  
print('mideterm_Q3_plot', '-dpdf', '-bestfit') % export plot to file
