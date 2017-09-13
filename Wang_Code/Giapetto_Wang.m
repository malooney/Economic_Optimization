% This program solves Giapetto's Woodcarving problem with the matlab
% function "linprog."

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define the parameter vector in the objective function.

f = [-3 -2]; % profits per unit of output
 
% Define matrix A containing technological paremeters in the ineqaulity
% constraints.

A = [2 1;
     1 1;
     1 0;
     -1 0;
     0 -1];
%  
% Define vector b.

b = [100; 80; 40; 0; 0];

% Call a linear programming routine.

[x, fval, exitflag, output, lambda] = linprog(f,A,b);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The problem can be set up in a different way as follows

% Define the parameter vector in the objective function.

f = [-3 -2];
 
% Define matrix A containing technological paremeters in the ineqaulity
% constraints.

A = [2 1;
     1 1];
%  
% Define vector b.

b = [100; 80];

% Define the lower and upper bound vectors

lb = [0; 0];
ub = [40; Inf];


% Call a linear programming routine.

[x, fval, exitflag, output, lambda] = linprog(f,A,b,[],[],lb,ub);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now we can change the constraints

% Define the parameter vector in the objective function.

f = [-3 -2];
 
% Define matrix A containing technological paremeters in the ineqaulity
% constraints.

A = [2 1;
     1 1];
%  
% Define vector b.

b = [101; 80];

% Define the lower and upper bound vectors

lb = [0; 0];
ub = [40; Inf];


% Call a linear programming routine.

[x, fval, exitflag, output, lambda] = linprog(f,A,b,[],[],lb,ub);