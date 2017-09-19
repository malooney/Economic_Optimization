%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a function solving for Giapetto's Woodcarving problem.

function [x, fval, lambda] = giap_Wang(P1, Cm1, Cvf1, Lf1, Lc1, P2, Cm2, Cvf2, Lf2, Lc2, Lfmax, Lcmax, D1max)

pi1 = P1 - (Cm1 + Cvf1);      %weekly profit per wooden soldier
pi2 = P2 - (Cm2 + Cvf2);      %weekly profit per wooden train


% Define the parameter vector in the objective function.
f = [-pi1 -pi2];
 
% Define matrix A containing technological paremeters in the ineqaulity
% constraints.

A = [Lf1 Lf2;
     Lc1 Lc2];
%  
% Define vector b, bounds of the inequality constraints.

b = [Lfmax; Lcmax];

% Define the lower and upper bounds of endogenous varialbes

lb = [0; 0];
ub = [D1max; Inf];


% Call the linear programming routine.

[x, fval, ~, ~, lambda] = linprog(f,A,b,[],[],lb,ub);
end
